#ifndef NGX_HEALTHCHECK_PLUGIN_H
#define NGX_HEALTHCHECK_PLUGIN_H

#include <ngx_core.h>
#include <ngx_http.h>

ngx_int_t plugin_ngx_http_haskell_healthcheck(void *cycle_data, void *umcf_data,
    volatile void *ntime_data, ngx_uint_t check_peers_in, ngx_uint_t active,
    u_char *peers_in, u_char **peers_out, size_t *peers_len);

#endif

