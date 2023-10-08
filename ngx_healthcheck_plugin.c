#include <ngx_core.h>
#include <ngx_http.h>

#if (NGX_HTTP_UPSTREAM_ZONE)
#define NGX_HEALTHCHECK_UPSTREAM_NO_SHM_ZONE(u)  ((u)->shm_zone == NULL)
#else
#define NGX_HEALTHCHECK_UPSTREAM_NO_SHM_ZONE(u)  1
#endif

typedef struct {
    ngx_str_node_t  sn;
    ngx_str_t       data;
} upstream_node_t;

static ngx_rbtree_t       *upstreams_rbt;
static ngx_rbtree_node_t   upstreams_snt;

ngx_int_t plugin_ngx_http_haskell_healthcheck_srv(ngx_cycle_t *cycle,
    ngx_http_upstream_main_conf_t *umcf, u_char *uname_in, u_char **srv_out,
    size_t *srv_len);
ngx_int_t plugin_ngx_http_haskell_healthcheck(ngx_cycle_t *cycle,
    ngx_http_upstream_main_conf_t *umcf, volatile ngx_time_t *ntime,
    ngx_uint_t check_peers_in, ngx_uint_t active, u_char *peers_in,
    u_char **peers_out, size_t *peers_len);
static void plugin_ngx_http_haskell_healthcheck_update_peer(ngx_cycle_t *cycle,
    time_t now, ngx_http_upstream_rr_peer_t *peer, ngx_uint_t good);


ngx_int_t
plugin_ngx_http_haskell_healthcheck_srv(ngx_cycle_t *cycle,
    ngx_http_upstream_main_conf_t *umcf, u_char *uname_in, u_char **srv_out,
    size_t *srv_len)
{
    ngx_uint_t                       i, j, len = 0;
    ngx_http_upstream_srv_conf_t   **uscf;
    ngx_http_upstream_server_t      *us;
    ngx_str_t                        uname;

    uname.data = uname_in;
    uname.len = ngx_strlen(uname_in);

    *srv_out = NULL;
    *srv_len = 0;

    /* BEWARE: this computation runs in a thread not known to Nginx and as such
     *         is very fragile! Be very careful! */

    uscf = umcf->upstreams.elts;

    for (i = 0; i < umcf->upstreams.nelts; i++) {
        if (uscf[i]->host.len == uname.len
            && ngx_strncmp(uscf[i]->host.data, uname.data, uname.len) == 0)
        {
            if (uscf[i]->servers == NULL) {
                ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                              "Upstream \"%V\" doesn't contain servers",
                              &uname);
                return NGX_ERROR;
            }
            us = uscf[i]->servers->elts;
            for (j = 0; j < uscf[i]->servers->nelts; j++) {
                len += us[j].name.len + 1;
            }
            break;
        }
    }

    if (len == 0) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "Cannot find upstream \"%V\"", &uname);
        return NGX_ERROR;
    }

    *srv_out = ngx_alloc(len, cycle->log);

    if (srv_out == NULL) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "Cannot allocate memory for the upstream content");
        return NGX_ERROR;
    }

    *srv_len = len - 1;
    len = 0;

    for (j = 0; j < uscf[i]->servers->nelts; j++) {
        ngx_memcpy(*srv_out + len, us[j].name.data, us[j].name.len);
        len += us[j].name.len;
        (*srv_out)[len++] = ',';
    }

    return NGX_OK;
}


ngx_int_t
plugin_ngx_http_haskell_healthcheck(ngx_cycle_t *cycle,
    ngx_http_upstream_main_conf_t *umcf, volatile ngx_time_t *ntime,
    ngx_uint_t check_peers_in, ngx_uint_t active, u_char *peers_in,
    u_char **peers_out, size_t *peers_len)
{
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
                        ngx_str_rbtree_insert_value);
    }

    for (p = peers_in; *p != '\0'; p++) {
        if (*p == '|') {
            found = 1;
            break;
        }
    }

    if (!found) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "Cannot extract upstream name from \"%s\"", peers_in);
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
                      "Cannot find upstream \"%V\"", &uname);
        rc = NGX_ERROR;
        goto unlock_and_return;
    }

    if (!active && NGX_HEALTHCHECK_UPSTREAM_NO_SHM_ZONE(u)) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "Upstream \"%V\" is not in shm zone, consider using "
                      "a normal per-worker service for managing it!", &uname);
        rc = NGX_ERROR;
        goto unlock_and_return;
    }

    if (peers == NULL) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "Cannot find peers in upstream \"%V\"", &uname);
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
            sname = peer->server.len > 0 ? peer->server : peer->name;
            len += peer->name.len + sname.len + 2;
        }
    }
    if (backup_peers != NULL) {
        for (peer = backup_peers->peer; peer; peer = peer->next) {
            if (peer->max_fails && peer->fails >= peer->max_fails) {
                sname = peer->server.len > 0 ? peer->server : peer->name;
                len += peer->name.len + sname.len + 2;
            }
        }
    }

    hash = ngx_crc32_long(uname.data, uname.len);

    uhost = (upstream_node_t *) ngx_str_rbtree_lookup(upstreams_rbt,
                                                      &uname, hash);

    if (len == 0) {
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
            *p++ = '/';
            sname = peer->server.len > 0 ? peer->server : peer->name;
            ngx_memcpy(p, sname.data, sname.len);
            p += sname.len;
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
                *p++ = '/';
                sname = peer->server.len > 0 ? peer->server : peer->name;
                ngx_memcpy(p, sname.data, sname.len);
                p += sname.len;
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
                      "Peer \"%V\" has been recovered and no longer watched",
                      &peer->name);
    }
    peer->checked = now;
}

