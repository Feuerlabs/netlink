//
// Netlink driver 
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netlink/cli/link.h>
#include <netlink/cli/utils.h>
#include <netlink/route/link.h>
#include <netlink/object-api.h>
#include <linux/if.h>

#include "erl_driver.h"

typedef struct {
    ErlDrvPort      port;
    ErlDrvEvent     event;
    int             active; // 0=no, 1=once, -1=yes
    struct nl_sock* sock;
    struct nl_cache* link_cache;
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
static void       nl_drv_output(ErlDrvData, char*, int);
static void       nl_drv_outputv(ErlDrvData, ErlIOVec*);
static void       nl_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void       nl_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData nl_drv_start(ErlDrvPort, char* command);
static int        nl_drv_ctl(ErlDrvData,unsigned int,char*, int,char**,int);
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
    int i;
    ErlDrvTermData message[1024];

#define push_atm(atm) do {			\
	message[i++] = ERL_DRV_ATOM;		\
	message[i++] = (atm);			\
    } while(0)

#define push_str(str) do {			\
	message[i++] = ERL_DRV_STRING;		\
	message[i++] = (ErlDrvTermData) (str);	\
	message[i++] = strlen(str);		\
    } while(0)

#define push_int(val) do {			\
	message[i++] = ERL_DRV_INT;		\
	message[i++] = (val);			\
    } while(0)



    i = 0;

    // {netlink, List}
    push_atm(atm_netlink);

    // 1- {name, Name}
    strcpy(link_name, rtnl_link_get_name(link));
    push_atm(atm_name);
    push_str(link_name);
    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    // debug
    fprintf(stderr, "sending link event for link: %s\r\n", link_name);    
	
    // 2- {index,Index}
    push_atm(atm_index);
    push_int(rtnl_link_get_ifindex(link));
    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    // 3 - {mtu,Mtu}
    push_atm(atm_mtu);
    push_int(rtnl_link_get_mtu(link));
    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    // 4 - {txqlen,Len}
    push_atm(atm_txqlen);
    push_int(rtnl_link_get_txqlen(link));
    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    flags = rtnl_link_get_flags(link);
    num_flags = 0;

    // 5 - {flags,[Flag1,Flag2,...]}
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
    message[i++] = ERL_DRV_NIL;
    message[i++] = ERL_DRV_LIST;
    message[i++] = num_flags+1;
    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    // 6 - {if_state, up|down}
    push_atm(atm_if_state);
    push_atm((flags & IFF_UP) ? atm_up : atm_down);
    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    // 7 - {if_lower_state, up|down}
    push_atm(atm_if_lower_state);
    push_atm((flags & IFF_RUNNING) ? atm_up : atm_down);
    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    // 8 - {oper_status, Status}
    push_atm(atm_oper_status);
    state = rtnl_link_get_operstate(link);
    rtnl_link_operstate2str(state,bf1,sizeof(bf1));
    push_str(bf1);
    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    // 9 - {link_mode, Mode}
    push_atm(atm_link_mode);
    mode = rtnl_link_get_linkmode(link);
    rtnl_link_mode2str(mode,bf2,sizeof(bf2));
    push_str(bf2);
    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    message[i++] = ERL_DRV_NIL;   // end of prop list

    message[i++] = ERL_DRV_LIST;
    message[i++] = 10;   // 9 element list & nil

    message[i++] = ERL_DRV_TUPLE;
    message[i++] = 2;

    driver_output_term(dptr->port, message, i);

    if (dptr->active == 1) {
	driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
	dptr->active = 0;
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
	if (dptr->sock)
	    nl_socket_free(dptr->sock);
	driver_free(dptr);
    }
}

static void       nl_drv_output(ErlDrvData d, char* buf, int len)
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

#define NL_CONNECT     1
#define NL_DISCONNECT  2
#define NL_ACTIVATE    3
#define NL_DEACTIVATE  4
#define NL_ACTIVATE_1  5

#define NL_OK     0
#define NL_ERROR  1
#define NL_ERRNO  2

static int nl_drv_ctl(ErlDrvData d,unsigned int cmd,char* buf,
		      int len,char** rbuf,int rlen)
{
    drv_data_t* dptr = (drv_data_t*) d;
    char* rdata = *rbuf;
    (void) buf;
    (void) len;
    (void) rbuf;
    int err;

    fprintf(stderr, "nl_drv_ctl called!!!\r\n");

    switch(cmd) {
    case NL_CONNECT: {
	int fd;

	nl_cli_connect(dptr->sock, NETLINK_ROUTE);
  
	if ((err = nl_socket_add_membership(dptr->sock, RTNLGRP_LINK)) < 0) {
	    err = 0;
	    fprintf(stderr, "nl_socket_add_membership: error: %s\n", 
		    nl_geterror(err));
	    goto L_error;
	}

	if (!(dptr->link_cache = nl_cli_link_alloc_cache(dptr->sock))) {
	    err = errno;
	    fprintf(stderr, "unable to allocate nl_cli_link_cache\r\n");
	    goto L_err;
	}

	if ((fd = nl_socket_get_fd(dptr->sock)) >= 0)
	    dptr->event = (ErlDrvEvent)((long)fd);
	else {
	    err = errno;
	    goto L_err;
	}
	dptr->active = 0;
	

	// Loop at startup to get interface states updated!
	{
	    struct nl_object* obj = nl_cache_get_first(dptr->link_cache);
	    if (obj) {
		do {
		    link_obj_send(dptr, (struct rtnl_link*) obj);
		    obj = nl_cache_get_next(obj);
		} while(obj);
	    }
	}
	
	break;
    }

    case NL_DISCONNECT:
	// FIXME:
	driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);	
	break;

    case NL_ACTIVATE:  // start sending events
	if (!dptr->active)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 1);
	dptr->active = -1;  // always
	break;

    case NL_DEACTIVATE: // stop sending events
	if (dptr->active)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
	dptr->active = 0;
	break;

    case NL_ACTIVATE_1:  // send one event
	if (!dptr->active)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 1);
	dptr->active = 1;  // active one event
	break;

    default:
	return -1;
    }

// L_ok:
    rdata[0] = NL_OK;
    return 1;

L_err:
    rdata[1] = NL_ERRNO;
    rdata[2] = err;
    return 2;
L_error:
    rdata[1] = NL_ERROR;
    rdata[2] = err;
    return 2;
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
