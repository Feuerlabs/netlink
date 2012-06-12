//
// Netlink driver 
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netlink/cli/link.h>
#include <netlink/cli/addr.h>
#include <netlink/cli/utils.h>
#include <netlink/route/link.h>
#include <netlink/object-api.h>
#include <linux/if.h>

#include "erl_driver.h"

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

typedef struct {
    ErlDrvPort       port;
    ErlDrvEvent      event;
    int              active; // -1=always, 0=no, 1=once,...
    int           connected;
    struct nl_sock*  sock;
    struct nl_cache* link_cache;
    struct nl_cache* addr_cache;
} drv_data_t;

ErlDrvEntry nl_drv_entry;

ErlDrvTermData atm_netlink;

ErlDrvTermData atm_name;
ErlDrvTermData atm_index;
ErlDrvTermData atm_mtu;
ErlDrvTermData atm_txqlen;
ErlDrvTermData atm_flags;
ErlDrvTermData atm_if_state;
ErlDrvTermData atm_if_lower_state;
ErlDrvTermData atm_oper_status;
ErlDrvTermData atm_link_mode;
ErlDrvTermData atm_addr;
ErlDrvTermData atm_bcast;
ErlDrvTermData atm_mask;
ErlDrvTermData atm_qdisc;
ErlDrvTermData atm_inet;
ErlDrvTermData atm_inet6;

ErlDrvTermData atm_up;
ErlDrvTermData atm_down;
ErlDrvTermData atm_broadcast;
ErlDrvTermData atm_debug;
ErlDrvTermData atm_loopback;
ErlDrvTermData atm_pointopoint;
ErlDrvTermData atm_notrailers;
ErlDrvTermData atm_running;
ErlDrvTermData atm_noarp;
ErlDrvTermData atm_promisc;
ErlDrvTermData atm_allmulti;
ErlDrvTermData atm_master;
ErlDrvTermData atm_slave;
ErlDrvTermData atm_multicast;
ErlDrvTermData atm_portsel;
ErlDrvTermData atm_automedia;
ErlDrvTermData atm_dynamic;
ErlDrvTermData atm_lower_up;
ErlDrvTermData atm_dormant;
ErlDrvTermData atm_echo;


static int        nl_drv_init(void);
static void       nl_drv_finish(void);
static void       nl_drv_stop(ErlDrvData);
static void       nl_drv_output(ErlDrvData,char*,ErlDrvSizeT);
static void       nl_drv_outputv(ErlDrvData, ErlIOVec*);
static void       nl_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void       nl_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData nl_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT nl_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**,ErlDrvSizeT);
static void       nl_drv_timeout(ErlDrvData);

//
// send event to erlang: 
//   {netlink,[{name,Name},{index,I},{mtu,M}...]}
//

static void link_obj_send(drv_data_t* dptr, struct rtnl_link* link)
{
    uint8_t state;
    uint8_t mode;
    unsigned int flags;
    int num_flags;
    char bf1[1024];
    char bf2[1024];
    char link_name[1024];
    char qdisc_name[1024];
    ErlDrvTermData message[1024];
    int mix = 0;
    int if_index;
    char* str;
    struct nl_addr *addr;
    int elems = 0;

#define push_atm(atm) do {			\
	message[mix++] = ERL_DRV_ATOM;		\
	message[mix++] = (atm);			\
    } while(0)

#define push_str(str) do {			\
	message[mix++] = ERL_DRV_STRING;		\
	message[mix++] = (ErlDrvTermData) (str);	\
	message[mix++] = strlen(str);		\
    } while(0)

#define push_int(val) do {			\
	message[mix++] = ERL_DRV_INT;		\
	message[mix++] = (val);			\
    } while(0)

#define push_tuple(n) do {			\
	message[mix++] = ERL_DRV_TUPLE;		\
	message[mix++] = (n);			\
    } while(0)

#define push_nil() do {				\
	message[mix++] = ERL_DRV_NIL;		\
    } while(0)

#define push_list(n) do {			\
	message[mix++] = ERL_DRV_LIST;		\
	message[mix++] = (n);			\
    } while(0)

    // {netlink, List}
    push_atm(atm_netlink);

    // {name, Name}
    strcpy(link_name, rtnl_link_get_name(link));
    push_atm(atm_name);
    push_str(link_name);
    push_tuple(2);
    elems++;

    // {index,Index}
    if_index = rtnl_link_get_ifindex(link);
    push_atm(atm_index);
    push_int(if_index);
    push_tuple(2);
    elems++;

    // {mtu,Mtu}
    push_atm(atm_mtu);
    push_int(rtnl_link_get_mtu(link));
    push_tuple(2);
    elems++;

    // {txqlen,Len}
    push_atm(atm_txqlen);
    push_int(rtnl_link_get_txqlen(link));
    push_tuple(2);
    elems++;

    // {flags,[Flag1,Flag2,...]}
    flags = rtnl_link_get_flags(link);
    num_flags = 0;
    push_atm(atm_flags);

    if (flags & IFF_UP) { push_atm(atm_up); num_flags++; }
    if (flags & IFF_BROADCAST) { push_atm(atm_broadcast); num_flags++; }
    if (flags & IFF_DEBUG) { push_atm(atm_debug); num_flags++; }
    if (flags & IFF_LOOPBACK) { push_atm(atm_loopback); num_flags++; }

    if (flags & IFF_POINTOPOINT) { push_atm(atm_pointopoint); num_flags++; }
    if (flags & IFF_NOTRAILERS) { push_atm(atm_notrailers); num_flags++; }
    if (flags & IFF_RUNNING) { push_atm(atm_running); num_flags++; }
    if (flags & IFF_NOARP) { push_atm(atm_noarp); num_flags++; }
    if (flags & IFF_PROMISC) { push_atm(atm_promisc); num_flags++; }
    if (flags & IFF_ALLMULTI) { push_atm(atm_allmulti); num_flags++; }
    if (flags & IFF_MASTER) { push_atm(atm_master); num_flags++; }
    if (flags & IFF_SLAVE) { push_atm(atm_slave); num_flags++; }
    if (flags & IFF_MULTICAST) { push_atm(atm_multicast); num_flags++; }
    if (flags & IFF_PORTSEL) { push_atm(atm_portsel); num_flags++; }
    if (flags & IFF_AUTOMEDIA) { push_atm(atm_automedia); num_flags++; }
    if (flags & IFF_DYNAMIC) { push_atm(atm_dynamic); num_flags++; }
    if (flags & IFF_LOWER_UP) { push_atm(atm_lower_up); num_flags++; }
    if (flags & IFF_DORMANT) { push_atm(atm_dormant); num_flags++; }
    if (flags & IFF_ECHO) { push_atm(atm_echo); num_flags++; }
    message[mix++] = ERL_DRV_NIL;
    message[mix++] = ERL_DRV_LIST;
    message[mix++] = num_flags+1;
    push_tuple(2);
    elems++;

    // {if_state, up|down}
    push_atm(atm_if_state);
    push_atm((flags & IFF_UP) ? atm_up : atm_down);
    push_tuple(2);
    elems++;

    // {if_lower_state, up|down}
    push_atm(atm_if_lower_state);
    push_atm((flags & IFF_RUNNING) ? atm_up : atm_down);
    push_tuple(2);
    elems++;

    // {oper_status, Status}
    push_atm(atm_oper_status);
    state = rtnl_link_get_operstate(link);
    rtnl_link_operstate2str(state,bf1,sizeof(bf1));
    push_str(bf1);
    push_tuple(2);
    elems++;

    // {link_mode, Mode}
    push_atm(atm_link_mode);
    mode = rtnl_link_get_linkmode(link);
    rtnl_link_mode2str(mode,bf2,sizeof(bf2));
    push_str(bf2);
    push_tuple(2);
    elems++;

    // {addr,{1,2,3,4,5,6}} (optional)
    if ((addr = rtnl_link_get_addr(link)) != NULL) {
	unsigned char* ptr = nl_addr_get_binary_addr(addr);
	unsigned int len = nl_addr_get_len(addr);
	int j;

	push_atm(atm_addr);
	for (j = 0; j < len; j++) {
	    push_int(ptr[j]);
	}
	push_tuple(len);
	push_tuple(2);
	elems++;
    }

    // {bcast,{1,2,3,4,5,6}} (optional)
    if ((addr = rtnl_link_get_broadcast(link)) != NULL) {
	unsigned char* ptr = nl_addr_get_binary_addr(addr);
	unsigned int len = nl_addr_get_len(addr);
	int j;

	push_atm(atm_bcast);
	for (j = 0; j < len; j++) {
	    push_int(ptr[j]);
	}
	push_tuple(len);
	push_tuple(2);
	elems++;
    }
    
    if ((str = rtnl_link_get_qdisc(link)) != NULL) {
	strcpy(qdisc_name, str);
	push_atm(atm_qdisc);
	push_str(qdisc_name);
	push_tuple(2);
	elems++;
    }

    // iterate over all addresses find this link and print all 
    // addresses bound to this link
    // {inet,  {addr,Addr} [{bcast,Addr},{mask,Mask}]}
    // {inet6, {addr,Addr} [{bcast,Addr},{mask,Mask}]}
    {
	struct nl_object* obj = nl_cache_get_first(dptr->addr_cache);
	if (obj) {
	    do {
		struct rtnl_addr *addr = nl_object_priv(obj);
		if (addr && (rtnl_addr_get_ifindex(addr) == if_index)) {
		    struct nl_addr *naddr = rtnl_addr_get_local(addr);
		    int family = nl_addr_get_family(naddr);
		    if (family == AF_INET6) {
			unsigned char* ptr = nl_addr_get_binary_addr(naddr);
			unsigned int len = nl_addr_get_len(naddr);
			int j;
			push_atm(atm_inet6);
			push_atm(atm_addr);
			for (j = 0; j < len; j += 2) {
			    push_int((ptr[j] << 8) + (ptr[j+1]));
			}
			push_tuple(len>>1);
			push_tuple(2);
			push_tuple(2);
			elems++;
		    }
		    else if (family == AF_INET) {
			unsigned char* ptr = nl_addr_get_binary_addr(naddr);
			unsigned int len = nl_addr_get_len(naddr);
			int j;
			push_atm(atm_inet);
			push_atm(atm_addr);
			for (j = 0; j < len; j++) {
			    push_int(ptr[j]);
			}
			push_tuple(len);
			push_tuple(2);
			push_tuple(2);
			elems++;
		    }
		}
		obj = nl_cache_get_next(obj);
	    } while(obj);
	}
    }

    push_nil();           // end of prop list
    push_list(elems+1);  // element list & nil
    push_tuple(2);

    if (mix > (sizeof(message)/sizeof(ErlDrvTermData))) {
	fprintf(stderr, "message build overflow\n");
    }

    driver_output_term(dptr->port, message, mix);

    if (dptr->active > 0) {
	dptr->active--;
	if (dptr->active == 0)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
    }
}

static void link_obj_event(struct nl_object *obj, void *arg)
{
    struct rtnl_link* link = (struct rtnl_link*) obj;
    drv_data_t* dptr = (drv_data_t*) arg;
    link_obj_send(dptr, link);
}


static int link_event_input(struct nl_msg *msg, void *arg)
{
    if (nl_msg_parse(msg, &link_obj_event, arg) < 0)
        fprintf(stderr, "link_event_input: Unknown message type\r\n");
    // Exit nl_recvmsgs_def() and return to the main select()
    return NL_STOP;
}

static int nl_drv_init(void)
{
    atm_netlink = driver_mk_atom("netlink");
    atm_name  = driver_mk_atom("name");
    atm_index = driver_mk_atom("index");
    atm_mtu = driver_mk_atom("mtu");
    atm_txqlen = driver_mk_atom("txqlen");
    atm_flags = driver_mk_atom("flags");
    atm_if_state = driver_mk_atom("if_state");
    atm_if_lower_state = driver_mk_atom("if_lower_state");
    atm_oper_status = driver_mk_atom("oper_status");
    atm_link_mode = driver_mk_atom("link_mode");
    atm_addr = driver_mk_atom("addr");
    atm_bcast = driver_mk_atom("bcast");
    atm_qdisc = driver_mk_atom("qdisc");
    atm_inet = driver_mk_atom("inet");
    atm_inet6 = driver_mk_atom("inet6");

    atm_up = driver_mk_atom("up");
    atm_down = driver_mk_atom("down");
    atm_broadcast = driver_mk_atom("broadcast");
    atm_debug = driver_mk_atom("debug");
    atm_loopback = driver_mk_atom("loopback");
    atm_pointopoint = driver_mk_atom("pointopoint");
    atm_notrailers = driver_mk_atom("notrailers");
    atm_running = driver_mk_atom("running");
    atm_noarp = driver_mk_atom("noarp");
    atm_promisc = driver_mk_atom("promisc");
    atm_allmulti = driver_mk_atom("allmulti");
    atm_master = driver_mk_atom("master");
    atm_slave = driver_mk_atom("slave");
    atm_multicast = driver_mk_atom("multicast");
    atm_portsel = driver_mk_atom("portsel");
    atm_automedia = driver_mk_atom("automedia");
    atm_dynamic = driver_mk_atom("dynamic");
    atm_lower_up = driver_mk_atom("lower_up");
    atm_dormant = driver_mk_atom("dormant");
    atm_echo = driver_mk_atom("echo");
    return 0;
}

static void       nl_drv_finish(void)
{
    fprintf(stderr, "nl_drv_finish called!!!\r\n");
}

static void       nl_drv_stop(ErlDrvData d)
{
    drv_data_t* dptr = (drv_data_t*) d;

    fprintf(stderr, "nl_drv_stop called!!!\r\n");
    if (dptr) {
	if (dptr->active)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
	if (dptr->sock) {
	    nl_close(dptr->sock);
	    nl_socket_free(dptr->sock);
	} 
	if (dptr->link_cache) {
	    nl_cache_free(dptr->link_cache);
	}
	if (dptr->addr_cache) {
	    nl_cache_free(dptr->addr_cache);
	}
	driver_free(dptr);
    }
}

static void       nl_drv_output(ErlDrvData d, char* buf,ErlDrvSizeT len)
{
    (void) d;
    (void) buf;
    (void) len;
    fprintf(stderr, "nl_drv_output called!!!\r\n");
}

static void       nl_drv_outputv(ErlDrvData d, ErlIOVec* iov)
{
    (void) d;
    (void) iov;
    fprintf(stderr, "nl_drv_outputv called!!!\r\n");
}

// netlink socket triggered process data
static void nl_drv_ready_input(ErlDrvData d, ErlDrvEvent event)
{
    drv_data_t* dptr = (drv_data_t*) d;
    (void) event;
    nl_recvmsgs_default(dptr->sock);
}

static void nl_drv_ready_output(ErlDrvData d, ErlDrvEvent event)
{
    (void) d;
    (void) event;
    fprintf(stderr, "nl_drv_read_output called!!!\r\n");
}

#define NL_CMD_CONNECT     1
#define NL_CMD_DISCONNECT  2
#define NL_CMD_ACTIVATE    3
#define NL_CMD_REFRESH     4

#define NL_REP_OK     0
#define NL_REP_ERROR  1

static ErlDrvSSizeT nl_drv_ctl(ErlDrvData d,unsigned int cmd,char* buf,
			       ErlDrvSizeT len,char** rbuf,ErlDrvSizeT rlen)
{
    drv_data_t* dptr = (drv_data_t*) d;
    char* rdata = *rbuf;
    char* err_str;
    int err_str_len;
    int err = 0;

    fprintf(stderr, "nl_drv_ctl called!!!\r\n");

    switch(cmd) {
    case NL_CMD_CONNECT: {
	int fd;

	if ((len != 0) || (dptr->connected))
	    goto L_einval;

	// FIXME: connect more targets?!
	nl_cli_connect(dptr->sock, NETLINK_ROUTE);
	dptr->connected = 1;

	if ((err = nl_socket_add_membership(dptr->sock, RTNLGRP_LINK)) < 0) {
	    err = 0;
	    fprintf(stderr, "nl_socket_add_membership: error: %s\n", 
		    nl_geterror(err));
	    // FIXME: mark this member ship and drop on error...
	    goto L_error;
	}

	if (!(dptr->link_cache = nl_cli_link_alloc_cache(dptr->sock))) {
	    err = errno;
	    fprintf(stderr, "unable to allocate nl_cli_link_cache\r\n");
	    goto L_error;
	}

	if (!(dptr->addr_cache = nl_cli_addr_alloc_cache(dptr->sock))) {
	    err = errno;
	    fprintf(stderr, "unable to allocate nl_cli_addr_cache\r\n");
	    goto L_error;
	}

	if ((fd = nl_socket_get_fd(dptr->sock)) >= 0) {
	    dptr->event = (ErlDrvEvent)((long)fd);
	    nl_socket_set_nonblocking(dptr->sock);
	}
	else {
	    err = errno;
	    goto L_error;
	}
	dptr->active = 0;
	break;
    }

    case NL_CMD_DISCONNECT:
	if ((len != 0) || !(dptr->connected))
	    goto L_einval;
	// FIXME:
	driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
	dptr->active = 0;	
	dptr->connected = 0;
	break;

    case NL_CMD_REFRESH: {
	if ((len != 0) || !(dptr->connected))
	    goto L_einval;
	// Loop at startup to get interface states updated!
	struct nl_object* obj = nl_cache_get_first(dptr->link_cache);
	if (obj) {
	    do {
		link_obj_send(dptr, (struct rtnl_link*) obj);
		obj = nl_cache_get_next(obj);
	    } while(obj);
	}
	break;
    }


    case NL_CMD_ACTIVATE: {  // start/stop sending events
	int active;

	if ((len != 2) || !(dptr->connected))
	    goto L_einval;
	active = (((uint8_t*)buf)[0] << 8) | ((uint8_t*)buf)[1];
	if (active == 0xffff)
	    active = -1;
	if (active) {
	    if (!dptr->active)
		driver_select(dptr->port, dptr->event, ERL_DRV_READ, 1);
	    dptr->active = active;
	}
	else {
	    if (dptr->active)
		driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
	    dptr->active = 0;
	}
	break;
    }

    default:
	return -1;
    }

// L_ok:
    rdata[0] = NL_REP_OK;
    return 1;

L_einval:
    err = EINVAL;
L_error:
    if (dptr->link_cache != NULL) {
	nl_cache_free(dptr->link_cache);
	dptr->link_cache = NULL;
    }
    if (dptr->addr_cache != NULL) {
	nl_cache_free(dptr->addr_cache);
	dptr->addr_cache = NULL;
    }
	
    rdata[0] = NL_REP_ERROR;
    err_str = strerror(err);
    err_str_len = strlen(err_str);
    if (err_str_len > 255) err_str_len = 255;
    if (err_str_len >= rlen) err_str_len = rlen - 1;
    memcpy(&rdata[1], err_str, err_str_len);
    return err_str_len;
}

static void       nl_drv_timeout(ErlDrvData d)
{
    (void) d;
    fprintf(stderr, "nl_drv_timeout called!!!\r\n");
}


static ErlDrvData nl_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    int err;
    drv_data_t* dptr;

    // Setup
    if (!(dptr = driver_alloc(sizeof(drv_data_t))))
	return ERL_DRV_ERROR_ERRNO;

    memset(dptr, 0, sizeof(drv_data_t));
    dptr->port = port;

    if (!(dptr->sock = nl_cli_alloc_socket())) {
	err = errno;
	driver_free(dptr);
	errno = err;
	return ERL_DRV_ERROR_ERRNO;
    }

    // do not want to hang !
    nl_socket_set_nonblocking(dptr->sock);

    // needed to be able to probe for any kind of netlink messages
    nl_socket_disable_seq_check(dptr->sock);

    nl_socket_modify_cb(dptr->sock, NL_CB_VALID, NL_CB_CUSTOM,
			link_event_input, dptr);
    return (ErlDrvData) dptr;
}

DRIVER_INIT(nl_drv)
{
    ErlDrvEntry* ptr = &nl_drv_entry;

    ptr->driver_name = "netlink_drv";
    ptr->init  = nl_drv_init;
    ptr->start = nl_drv_start;
    ptr->stop  = nl_drv_stop;
    ptr->output = nl_drv_output;
    ptr->ready_input  = nl_drv_ready_input;
    ptr->ready_output = nl_drv_ready_output;
    ptr->finish = nl_drv_finish;
    ptr->control = nl_drv_ctl;
    ptr->timeout = nl_drv_timeout;
    ptr->outputv = nl_drv_outputv;
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = 0;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = 0;  // add me

    return (ErlDrvEntry*) ptr;
}
