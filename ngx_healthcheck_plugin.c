#include <ngx_core.h>
#include <ngx_http.h>

typedef struct {
    ngx_str_node_t  sn;
    ngx_str_t       data;
} upstream_node_t;

static ngx_rbtree_t       *upstreams_rbt;
static ngx_rbtree_node_t   upstreams_snt;

ngx_int_t plugin_ngx_http_haskell_healthcheck(ngx_cycle_t *cycle,
    void *umcf_data, volatile void *ntime_data, ngx_uint_t check_peers_in,
    ngx_uint_t active, u_char *peers_in, u_char **peers_out, size_t *peers_len);
static void plugin_ngx_http_haskell_healthcheck_update_peer(ngx_cycle_t *cycle,
    time_t now, ngx_http_upstream_rr_peer_t *peer, ngx_uint_t good);
static void wrap_ngx_str_rbtree_insert_value(ngx_rbtree_node_t *temp,
    ngx_rbtree_node_t *node, ngx_rbtree_node_t *sentinel);
static uint32_t local_ngx_crc32_long(u_char *p, size_t len);


ngx_int_t
plugin_ngx_http_haskell_healthcheck(ngx_cycle_t *cycle, void *umcf_data,
    volatile void *ntime_data, ngx_uint_t check_peers_in, ngx_uint_t active,
    u_char *peers_in, u_char **peers_out, size_t *peers_len)
{
    ngx_http_upstream_main_conf_t   *umcf = umcf_data;
    volatile ngx_time_t             *ntime = ntime_data;

    ngx_uint_t                       i, len = 0;
    u_char                          *p, *p_prev;
    ngx_http_upstream_srv_conf_t   **uscf, *u = NULL;
    ngx_http_upstream_rr_peers_t    *peers = NULL, *backup_peers = NULL;
    ngx_http_upstream_rr_peer_t     *peer;
    upstream_node_t                 *uhost;
    uint32_t                         hash;
    ngx_str_t                        uname, sname;
    time_t                           now = 0;
    ngx_uint_t                       found = 0, good = 0, stop = 0;
    ngx_uint_t                       staged_to_good = 0, peers_locked = 0;
    ngx_int_t                        rc = NGX_OK;

    *peers_out = NULL;
    *peers_len = 0;

    if (upstreams_rbt == NULL) {
        upstreams_rbt = ngx_palloc(cycle->pool, sizeof(ngx_rbtree_t));
        if (upstreams_rbt == NULL) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "Cannot allocate memory for upstream data storage");
            return NGX_ERROR;
        }
        ngx_rbtree_init(upstreams_rbt, &upstreams_snt,
                        wrap_ngx_str_rbtree_insert_value);
    }

    for (p = peers_in; *p != '\0'; p++) {
        if (*p == '|') {
            found = 1;
            break;
        }
    }

    if (!found) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "Cannot extract upstream name from: \"%s\"", peers_in);
        return NGX_ERROR;
    }

    uname.data = peers_in;
    uname.len = p++ - peers_in;

    uscf = umcf->upstreams.elts;

    for (i = 0; i < umcf->upstreams.nelts; i++) {
        if (uscf[i]->host.len == uname.len
            && ngx_strncmp(uscf[i]->host.data, uname.data, uname.len) == 0)
        {
            u = uscf[i];
            peers = uscf[i]->peer.data;
            if (peers != NULL) {
                if (active) {
                    ngx_http_upstream_rr_peers_wlock(peers);
                } else {
                    ngx_http_upstream_rr_peers_rlock(peers);
                }
                peers_locked = 1;
                backup_peers = peers->next;
            }
            break;
        }
    }

    if (u == NULL) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "Cannot find upstream: \"%V\"", &uname);
        rc = NGX_ERROR;
        goto unlock_and_return;
    }

    if (!active) {
#if (NGX_HTTP_UPSTREAM_ZONE)
        if (u->shm_zone == NULL) {
#endif
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "Upstream \"%V\" is not in shm zone, "
                          "consider using a normal per-worker service "
                          "for managing it!", &uname);
            rc = NGX_ERROR;
            goto unlock_and_return;
#if (NGX_HTTP_UPSTREAM_ZONE)
        }
#endif
    }

    if (peers == NULL) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "Cannot find peers in upstream: \"%V\"", &uname);
        rc = NGX_ERROR;
        goto unlock_and_return;
    }

    if (!check_peers_in) {
        goto check_peers_out;
    }

    for (p_prev = p; *p_prev != '\0' && !stop; p++) {
        found = 0;
        good = staged_to_good;
        if (*p == '/') {
            found = 1;
            staged_to_good = 1;
        }
        if (*p == ',' || *p == '\0') {
            found = 1;
            stop = *p == '\0' ? 1 : 0;
        }
        if (!found) {
            continue;
        }
        sname.data = p_prev;
        sname.len = p++ - p_prev;
        if (sname.len == 0) {
            if (!stop) {
                p_prev = p++;
            }
            continue;
        }
        found = 0;
        for (peer = peers->peer; peer; peer = peer->next) {
            if (active && peer->name.len == sname.len
                && ngx_strncmp(peer->name.data, sname.data, sname.len) == 0)
            {
                if (now == 0) {
                    now = ntime->sec;
                }
                plugin_ngx_http_haskell_healthcheck_update_peer(cycle, now,
                                                                peer, good);
                found = 1;
            }
        }
        if (backup_peers != NULL) {
            for (peer = backup_peers->peer; peer; peer = peer->next) {
                if (active && peer->name.len == sname.len
                    && ngx_strncmp(peer->name.data, sname.data, sname.len) == 0)
                {
                    if (now == 0) {
                        now = ntime->sec;
                    }
                    plugin_ngx_http_haskell_healthcheck_update_peer(cycle, now,
                                                                    peer, good);
                    found = 1;
                }
            }
        }
        if (active && !found) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "Cannot find peer \"%V\" in upstream \"%V\", "
                          "ignoring it", &sname, &uname);
        }
        if (!stop) {
            p_prev = p++;
        }
    }

check_peers_out:

    for (peer = peers->peer; peer; peer = peer->next) {
        if (peer->max_fails && peer->fails >= peer->max_fails) {
            len += peer->name.len + 1;
        }
    }
    if (backup_peers != NULL) {
        for (peer = backup_peers->peer; peer; peer = peer->next) {
            if (peer->max_fails && peer->fails >= peer->max_fails) {
                len += peer->name.len + 1;
            }
        }
    }

    hash = local_ngx_crc32_long(uname.data, uname.len);

    uhost = (upstream_node_t *) ngx_str_rbtree_lookup(upstreams_rbt,
                                                      &uname, hash);

    if (len < 2) {
        if (uhost != NULL) {
            ngx_rbtree_delete(upstreams_rbt, &uhost->sn.node);
            if (uhost->data.data != NULL) {
                ngx_pfree(cycle->pool, uhost->data.data);
            }
            ngx_pfree(cycle->pool, uhost);
        }
        goto unlock_and_return;
    }

    if (uhost == NULL) {
        uhost = ngx_pcalloc(cycle->pool, sizeof(upstream_node_t));
        if (uhost == NULL) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "Cannot allocate memory for a new element in "
                          "upstream data storage");
            rc = NGX_ERROR;
            goto unlock_and_return;
        }
        uhost->sn.node.key = hash;
        uhost->sn.str.data = ngx_pnalloc(cycle->pool, uname.len);
        if (uhost->sn.str.data == NULL) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "Cannot allocate memory for the name of "
                          "the upstream in upstream data storage");
            rc = NGX_ERROR;
            goto unlock_and_return;
        }
        ngx_memcpy(uhost->sn.str.data, uname.data, uname.len);
        uhost->sn.str.len = uname.len;
        ngx_rbtree_insert(upstreams_rbt, &uhost->sn.node);
    }

    if (len > uhost->data.len + 1) {
        if (uhost->data.data != NULL) {
            ngx_pfree(cycle->pool, uhost->data.data);
        }
        uhost->data.data = ngx_pnalloc(cycle->pool, len);
        if (uhost->data.data == NULL) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "Cannot allocate memory for data in "
                          "upstream data storage");
            rc = NGX_ERROR;
            goto unlock_and_return;
        }
    }
    uhost->data.len = len - 1;

    p = uhost->data.data;

    for (peer = peers->peer; peer; peer = peer->next) {
        if (peer->max_fails && peer->fails >= peer->max_fails) {
            if (check_peers_in) {
                ngx_log_error(NGX_LOG_NOTICE, cycle->log, 0,
                              "Peer \"%V\" is in failed state and staged "
                              "for watching", &peer->name);
            }
            ngx_memcpy(p, peer->name.data, peer->name.len);
            p += peer->name.len;
            *p++ = ',';
        }
    }
    if (backup_peers != NULL) {
        for (peer = backup_peers->peer; peer; peer = peer->next) {
            if (peer->max_fails && peer->fails >= peer->max_fails) {
                if (check_peers_in) {
                    ngx_log_error(NGX_LOG_NOTICE, cycle->log, 0,
                                  "Peer \"%V\" is in failed state and staged "
                                  "for watching", &peer->name);
                }
                ngx_memcpy(p, peer->name.data, peer->name.len);
                p += peer->name.len;
                *p++ = ',';
            }
        }
    }

    *peers_out = uhost->data.data;
    *peers_len = uhost->data.len;

unlock_and_return:

    if (peers_locked) {
        ngx_http_upstream_rr_peers_unlock(peers);
    }

    return rc;
}


static void
plugin_ngx_http_haskell_healthcheck_update_peer(ngx_cycle_t *cycle, time_t now,
    ngx_http_upstream_rr_peer_t *peer, ngx_uint_t good)
{
    if (good) {
        peer->fails = 0;
        ngx_log_error(NGX_LOG_NOTICE, cycle->log, 0,
                      "Peer \"%V\" has been repaired and no longer watched",
                      &peer->name);
    }
    peer->checked = now;
}


static void
wrap_ngx_str_rbtree_insert_value(ngx_rbtree_node_t *temp,
    ngx_rbtree_node_t *node, ngx_rbtree_node_t *sentinel)
{
    ngx_str_rbtree_insert_value(temp, node, sentinel);
}


static uint32_t  local_ngx_crc32_table256[] = {
    0x00000000, 0x77073096, 0xee0e612c, 0x990951ba,
    0x076dc419, 0x706af48f, 0xe963a535, 0x9e6495a3,
    0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
    0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91,
    0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de,
    0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
    0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec,
    0x14015c4f, 0x63066cd9, 0xfa0f3d63, 0x8d080df5,
    0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
    0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
    0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940,
    0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
    0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116,
    0x21b4f4b5, 0x56b3c423, 0xcfba9599, 0xb8bda50f,
    0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
    0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,
    0x76dc4190, 0x01db7106, 0x98d220bc, 0xefd5102a,
    0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
    0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818,
    0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
    0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
    0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457,
    0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c,
    0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
    0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,
    0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb,
    0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
    0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9,
    0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086,
    0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
    0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4,
    0x59b33d17, 0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad,
    0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
    0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683,
    0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8,
    0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
    0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe,
    0xf762575d, 0x806567cb, 0x196c3671, 0x6e6b06e7,
    0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
    0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
    0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252,
    0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
    0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60,
    0xdf60efc3, 0xa867df55, 0x316e8eef, 0x4669be79,
    0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
    0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f,
    0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04,
    0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
    0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a,
    0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
    0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
    0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21,
    0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e,
    0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
    0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,
    0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45,
    0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
    0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db,
    0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0,
    0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
    0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6,
    0xbad03605, 0xcdd70693, 0x54de5729, 0x23d967bf,
    0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
    0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
};


static uint32_t
local_ngx_crc32_long(u_char *p, size_t len)
{
    uint32_t  crc;

    crc = 0xffffffff;

    while (len--) {
        crc = local_ngx_crc32_table256[(crc ^ *p++) & 0xff] ^ (crc >> 8);
    }

    return crc ^ 0xffffffff;
}

