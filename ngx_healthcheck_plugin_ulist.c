/* BEWARE: in this file a linked-list-based storage for failed upstreams is
 * implemented. This was eventually superseded by an rb-tree-based storage,
 * currently located in file ngx_healthcheck_plugin.c. Therefore, this file is
 * no longer a part of the build process and held just in case.
 */

#include "ngx_healthcheck_plugin.h"

struct upstream_list_s {
    ngx_str_t                name;
    ngx_str_t                data;
    struct upstream_list_s  *next;
};

typedef struct upstream_list_s upstream_list_t;

static upstream_list_t  *upstreams;

static void plugin_ngx_http_haskell_healthcheck_update_peer(ngx_cycle_t *cycle,
    time_t now, ngx_http_upstream_rr_peer_t *peer, ngx_uint_t good);


ngx_int_t
plugin_ngx_http_haskell_healthcheck(void *cycle_data, void *umcf_data,
    volatile void *ntime_data, ngx_uint_t check_peers_in, ngx_uint_t active,
    u_char *peers_in, u_char **peers_out, size_t *peers_len)
{
    ngx_cycle_t                     *cycle = cycle_data;
    ngx_http_upstream_main_conf_t   *umcf = umcf_data;
    volatile ngx_time_t             *ntime = ntime_data;

    ngx_uint_t                       i, len = 0;
    u_char                          *p, *p_prev;
    ngx_http_upstream_srv_conf_t   **uscf, *u = NULL;
    ngx_http_upstream_rr_peers_t    *peers = NULL, *backup_peers = NULL;
    ngx_http_upstream_rr_peer_t     *peer;
    upstream_list_t                 *uhost, *u_prev;
    ngx_str_t                        uname, sname;
    time_t                           now = 0;
    ngx_uint_t                       found = 0, good = 0, stop = 0;
    ngx_uint_t                       staged_to_good = 0, peers_locked = 0;
    ngx_int_t                        rc = NGX_OK;

    *peers_out = NULL;
    *peers_len = 0;

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
                          "deleting it from the watch list",
                          &sname, &uname);
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

    for (uhost = upstreams, u_prev = NULL; uhost != NULL;
         u_prev = uhost, uhost = uhost->next)
    {
        if (uhost->name.len == uname.len
            && ngx_strncmp(uhost->name.data, uname.data, uname.len) == 0)
        {
            break;
        }
    }

    if (len < 2) {
        if (uhost != NULL) {
            if (u_prev == NULL) {
                upstreams = uhost;
            } else {
                u_prev->next = uhost->next;
            }
            if (uhost->data.data != NULL) {
                ngx_pfree(cycle->pool, uhost->data.data);
            }
            ngx_pfree(cycle->pool, uhost);
        }
        goto unlock_and_return;
    }

    if (uhost == NULL) {
        uhost = ngx_pcalloc(cycle->pool, sizeof(upstream_list_t));
        if (uhost == NULL) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "Cannot allocate memory for a new link in "
                          "upstream list");
            rc = NGX_ERROR;
            goto unlock_and_return;
        }
        uhost->name.data = ngx_pnalloc(cycle->pool, uname.len);
        if (uhost->name.data == NULL) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "Cannot allocate memory for the name of "
                          "the upstream in upstream list");
            rc = NGX_ERROR;
            goto unlock_and_return;
        }
        ngx_memcpy(uhost->name.data, uname.data, uname.len);
        uhost->name.len = uname.len;
        uhost->next = upstreams;
        upstreams = uhost;
    }

    if (len > uhost->data.len + 1) {
        if (uhost->data.data != NULL) {
            ngx_pfree(cycle->pool, uhost->data.data);
        }
        uhost->data.data = ngx_pnalloc(cycle->pool, len);
        if (uhost->data.data == NULL) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "Cannot allocate memory for data in upstream list");
            rc = NGX_ERROR;
            goto unlock_and_return;
        }
    }
    uhost->data.len = len - 1;

    p = uhost->data.data;

    for (peer = peers->peer; peer; peer = peer->next) {
        if (peer->max_fails && peer->fails >= peer->max_fails) {
            if (check_peers_in) {
                ngx_log_error(NGX_LOG_ALERT, cycle->log, 0,
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
                    ngx_log_error(NGX_LOG_ALERT, cycle->log, 0,
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
        ngx_log_error(NGX_LOG_ALERT, cycle->log, 0,
                      "Peer \"%V\" is repaired and no longer watched",
                      &peer->name);
    }
    peer->checked = now;
}

