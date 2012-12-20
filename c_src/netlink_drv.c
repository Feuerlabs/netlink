/****** BEGIN COPYRIGHT *******************************************************
 *
 * Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 ****** END COPYRIGHT ********************************************************/
//
// Netlink driver
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netlink/cli/link.h>
#include <netlink/cli/addr.h>
#include <netlink/cli/route.h>
#include <netlink/cli/utils.h>
#include <netlink/route/link.h>
#include <netlink/route/route.h>
#include <netlink/route/nexthop.h>
#include <netlink/object-api.h>
#include <linux/if.h>

#include "erl_driver.h"
#include "dterm.h"

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

typedef struct {
    ErlDrvPort       port;
    ErlDrvTermData   dport;
    ErlDrvEvent      event;
    int              active; // -1=always, 0=no, 1=once,...
    int           connected;
    struct nl_sock*  sock;
    struct nl_cache* link_cache;
    struct nl_cache* addr_cache;
    struct nl_cache* route_cache;
} drv_data_t;

ErlDrvEntry nl_drv_entry;

ErlDrvTermData atm_netlink;

ErlDrvTermData atm_name;
ErlDrvTermData atm_index;
ErlDrvTermData atm_label;
ErlDrvTermData atm_mtu;
ErlDrvTermData atm_txqlen;
ErlDrvTermData atm_flags;
ErlDrvTermData atm_if_state;
ErlDrvTermData atm_if_lower_state;
ErlDrvTermData atm_oper_status;
ErlDrvTermData atm_link_mode;
ErlDrvTermData atm_addr;
ErlDrvTermData atm_bcast;
ErlDrvTermData atm_peer;
ErlDrvTermData atm_mcast;
ErlDrvTermData atm_prefixlen;
ErlDrvTermData atm_qdisc;
ErlDrvTermData atm_inet;
ErlDrvTermData atm_inet6;
ErlDrvTermData atm_dst;
ErlDrvTermData atm_src;
ErlDrvTermData atm_pref_src;
ErlDrvTermData atm_metric;
ErlDrvTermData atm_scope;
ErlDrvTermData atm_tos;
ErlDrvTermData atm_protocol;
ErlDrvTermData atm_priority;
ErlDrvTermData atm_type;
ErlDrvTermData atm_weight;
ErlDrvTermData atm_gateway;
ErlDrvTermData atm_realms;
ErlDrvTermData atm_nexthop;


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
ErlDrvTermData atm_undefined;
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

// {<tag>, address}
int dterm_address(dterm_t* p, ErlDrvTermData tag,int family,
		  unsigned char* ptr, size_t len)
{
    dterm_mark_t prop;
    dterm_mark_t addr;

    dterm_tuple_begin(p, &prop); {
	dterm_atom(p,tag);

	switch(family) {
	case AF_INET6:
	    dterm_tuple_begin(p, &addr); {
		int i;
		for (i = 0; i < len; i += 2)
		    dterm_int(p,(ptr[i] << 8) + (ptr[i+1]));
	    }
	    dterm_tuple_end(p, &addr);
	    break;
	case AF_INET:
	case AF_UNSPEC:
	    dterm_tuple_begin(p, &addr); {
		int i;
		for (i = 0; i < len; i++)
		    dterm_int(p,ptr[i]);
	    }
	    dterm_tuple_end(p, &addr);
	    break;
	default:
	    dterm_atom(p, atm_undefined);
	    break;
	}
    }
    return dterm_tuple_end(p, &prop);
}

int dterm_nl_addr(dterm_t* p, ErlDrvTermData tag, int family,
		  struct nl_addr* naddr)
{
    if (naddr == NULL)
	return 0;
    if ((family == AF_INET) || (family == AF_INET6)) {
	dterm_mark_t prop;

	dterm_tuple_begin(p, &prop); {
	    // {<fam>, {<tag>, Address}}
	    dterm_atom(p,(family == AF_INET6)?atm_inet6:atm_inet);
	    dterm_address(p, tag, family,
			  nl_addr_get_binary_addr(naddr),
			  nl_addr_get_len(naddr));
	}
	dterm_tuple_end(p, &prop);
	return 1;
    }
    return 0;
}


//
// send event to erlang:
//   {netlink,<port>,"route/link",[{name,Name},{index,I},{mtu,M}...
//

static void link_obj_send(drv_data_t* dptr, struct rtnl_link* link)
{
    uint8_t state;
    uint8_t mode;
    char bf1[1024];
    char bf2[1024];
    char link_name[1024];
    char qdisc_name[1024];
    dterm_t m;
    int if_index;
    unsigned int flags = 0;
    char* str;
    struct nl_addr *addr;
    dterm_mark_t msg;
    dterm_mark_t prop;
    dterm_mark_t prop_list;

    dterm_init(&m);

    // msg = {netlink,<port>,"route/link",<prop_list>}
    dterm_tuple_begin(&m, &msg);
    dterm_atom(&m,atm_netlink);
    dterm_port(&m, dptr->dport);
    dterm_string(&m,"route/link",10);

    dterm_list_begin(&m, &prop_list);

    dterm_tuple_begin(&m, &prop); {
	// {name, Name}
	strcpy(link_name, rtnl_link_get_name(link));
	dterm_atom(&m,atm_name);
	dterm_string(&m,link_name,strlen(link_name));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	// {index,Index}
	if_index = rtnl_link_get_ifindex(link);
	dterm_atom(&m,atm_index);
	dterm_int(&m,if_index);
    }
    dterm_tuple_end(&m, &prop);


    dterm_tuple_begin(&m, &prop); {
	// {mtu,Mtu}
	dterm_atom(&m,atm_mtu);
	dterm_int(&m,rtnl_link_get_mtu(link));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	// {txqlen,Len}
	dterm_atom(&m,atm_txqlen);
	dterm_int(&m,rtnl_link_get_txqlen(link));
    }
    dterm_tuple_end(&m, &prop);

    // {flags,[Flag1,Flag2,...]}
    dterm_tuple_begin(&m, &prop); {
	dterm_mark_t flag_list;
	flags = rtnl_link_get_flags(link);

	dterm_atom(&m,atm_flags);
	dterm_list_begin(&m, &flag_list);
	if (flags & IFF_UP) { dterm_atom(&m,atm_up); }
	if (flags & IFF_BROADCAST) { dterm_atom(&m,atm_broadcast); }
	if (flags & IFF_DEBUG) { dterm_atom(&m,atm_debug); }
	if (flags & IFF_LOOPBACK) { dterm_atom(&m,atm_loopback); }

	if (flags & IFF_POINTOPOINT) { dterm_atom(&m,atm_pointopoint); }
	if (flags & IFF_NOTRAILERS) { dterm_atom(&m,atm_notrailers);  }
	if (flags & IFF_RUNNING) { dterm_atom(&m,atm_running);  }
	if (flags & IFF_NOARP) { dterm_atom(&m,atm_noarp);  }
	if (flags & IFF_PROMISC) { dterm_atom(&m,atm_promisc); }
	if (flags & IFF_ALLMULTI) { dterm_atom(&m,atm_allmulti); }
	if (flags & IFF_MASTER) { dterm_atom(&m,atm_master); }
	if (flags & IFF_SLAVE) { dterm_atom(&m,atm_slave); }
	if (flags & IFF_MULTICAST) { dterm_atom(&m,atm_multicast); }
	if (flags & IFF_PORTSEL) { dterm_atom(&m,atm_portsel); }
	if (flags & IFF_AUTOMEDIA) { dterm_atom(&m,atm_automedia); }
	if (flags & IFF_DYNAMIC) { dterm_atom(&m,atm_dynamic); }
	if (flags & IFF_LOWER_UP) { dterm_atom(&m,atm_lower_up); }
	if (flags & IFF_DORMANT) { dterm_atom(&m,atm_dormant); }
	if (flags & IFF_ECHO) { dterm_atom(&m,atm_echo); }
	dterm_list_end(&m, &flag_list);
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	// {if_state, up|down}
	dterm_atom(&m,atm_if_state);
	dterm_atom(&m,(flags & IFF_UP) ? atm_up : atm_down);
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	// {if_lower_state, up|down}
	dterm_atom(&m,atm_if_lower_state);
	dterm_atom(&m,(flags & IFF_RUNNING) ? atm_up : atm_down);
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	// {oper_status, Status}
	dterm_atom(&m,atm_oper_status);
	state = rtnl_link_get_operstate(link);
	rtnl_link_operstate2str(state,bf1,sizeof(bf1));
	dterm_string(&m,bf1,strlen(bf1));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	// {link_mode, Mode}
	dterm_atom(&m,atm_link_mode);
	mode = rtnl_link_get_linkmode(link);
	rtnl_link_mode2str(mode,bf2,sizeof(bf2));
	dterm_string(&m,bf2,strlen(bf2));
    }
    dterm_tuple_end(&m, &prop);

    // {addr,{1,2,3,4,5,6}} (optional, not defined for ppp)
    if ((addr = rtnl_link_get_addr(link)) != NULL) {
	dterm_address(&m, atm_addr, 0,
		      nl_addr_get_binary_addr(addr),
		      nl_addr_get_len(addr));
    }

    // {bcast,{1,2,3,4,5,6}} (optional, not defined for ppp)
    if ((addr = rtnl_link_get_broadcast(link)) != NULL) {
	dterm_address(&m,atm_bcast,0,
		      nl_addr_get_binary_addr(addr),
		      nl_addr_get_len(addr));
    }

    if ((str = rtnl_link_get_qdisc(link)) != NULL) {
	strcpy(qdisc_name, str);
	dterm_tuple_begin(&m, &prop); {
	    dterm_atom(&m,atm_qdisc);
	    dterm_string(&m,qdisc_name,strlen(qdisc_name));
	}
	dterm_tuple_end(&m, &prop);
    }

    dterm_list_end(&m, &prop_list);
    dterm_tuple_end(&m, &msg);

    driver_output_term(dptr->port, dterm_data(&m), dterm_used_size(&m));
    if (m.base != m.data) {
	fprintf(stderr, "dterm allocated %d bytes\r\n",
		dterm_allocated_size(&m));
    }
    dterm_finish(&m);

    if (dptr->active > 0) {
	dptr->active--;
	if (dptr->active == 0)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
    }
}

//
// {netlink,[{label,Name},{index,I},
//           {inet,{addr,A}},{inet,{bcast,A}},{inet,{mask,A}},
//           {inet6,{addr,A}},{inet6,{bcast,A}},{inet6,{mask,A}}
//
static void addr_obj_send(drv_data_t* dptr, struct rtnl_addr* addr)
{
    int if_index;
    int family;
    char label_name[256];
    char* if_label;
    dterm_t m;
    dterm_mark_t msg;
    dterm_mark_t prop;
    dterm_mark_t prop_list;

    if_index = rtnl_addr_get_ifindex(addr);
    if_label = rtnl_addr_get_label(addr);

    fprintf(stderr, "addr_obj_send %s ifindex=%d\r\n", if_label, if_index);

    dterm_init(&m);

    // msg = {netlink,<port>,"route/addr",<prop_list>}
    dterm_tuple_begin(&m, &msg);
    dterm_atom(&m,atm_netlink);
    dterm_port(&m, dptr->dport);
    dterm_string(&m,"route/addr",10);

    dterm_list_begin(&m, &prop_list);

    if (if_label != NULL) {
	dterm_tuple_begin(&m, &prop); {
	    // {label,Label}
	    strcpy(label_name, if_label);
	    dterm_atom(&m,atm_label);
	    dterm_string(&m,label_name,strlen(label_name));
	}
	dterm_tuple_end(&m, &prop);
    }

    dterm_tuple_begin(&m, &prop); {
	// {index,Index}
	dterm_atom(&m,atm_index);
	dterm_int(&m,if_index);
    }
    dterm_tuple_end(&m, &prop);

    family = rtnl_addr_get_family(addr);

    dterm_nl_addr(&m,atm_addr, family,  rtnl_addr_get_local(addr));
    dterm_nl_addr(&m,atm_bcast,family,  rtnl_addr_get_broadcast(addr));
    // peer address (P-to-P)
    dterm_nl_addr(&m,atm_peer,family, rtnl_addr_get_peer(addr));
    dterm_nl_addr(&m,atm_multicast,family, rtnl_addr_get_multicast(addr));

    dterm_tuple_begin(&m, &prop); {
	dterm_mark_t flag;
	// {inet,{prefixlen,Len}}
	dterm_atom(&m,(family == AF_INET6)?atm_inet6:atm_inet);
	dterm_tuple_begin(&m, &flag); {
	    dterm_atom(&m,atm_prefixlen);
	    dterm_int(&m,rtnl_addr_get_prefixlen(addr));
	}
	dterm_tuple_end(&m, &flag);
    }
    dterm_tuple_end(&m, &prop);


    dterm_list_end(&m, &prop_list);
    dterm_tuple_end(&m, &msg);

    driver_output_term(dptr->port, dterm_data(&m), dterm_used_size(&m));

    if (m.base != m.data) {
	fprintf(stderr, "dterm allocated %d bytes\r\n",
		dterm_allocated_size(&m));
    }
    dterm_finish(&m);

    if (dptr->active > 0) {
	dptr->active--;
	if (dptr->active == 0)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
    }
}

//
// Build [{weight,W}...{gateway,Gw}]
//
static void make_nexthop(dterm_t* p, int family, struct rtnl_nexthop* n)
{
    dterm_mark_t prop;
    dterm_mark_t prop_list;

    dterm_list_begin(p, &prop_list);

    dterm_tuple_begin(p, &prop); {
	// weight
	dterm_atom(p, atm_weight);
	dterm_int(p, rtnl_route_nh_get_weight(n));
    }
    dterm_tuple_end(p, &prop);

    dterm_tuple_begin(p, &prop); {
	// weight
	dterm_atom(p, atm_index);
	dterm_int(p, rtnl_route_nh_get_ifindex(n));
    }
    dterm_tuple_end(p, &prop);

    dterm_nl_addr(p, atm_gateway, family, rtnl_route_nh_get_gateway(n));

    dterm_tuple_begin(p, &prop); {
	// flags
	dterm_atom(p, atm_flags);
	dterm_int(p, rtnl_route_nh_get_flags(n));
    }
    dterm_tuple_end(p, &prop);

    dterm_tuple_begin(p, &prop); {
	// flags
	dterm_atom(p, atm_realms);
	dterm_int(p, rtnl_route_nh_get_realms(n));
    }
    dterm_tuple_end(p, &prop);

    dterm_list_end(p, &prop_list);
}

//
// {netlink,<port>,[{label,Name},{index,I},
//
static void route_obj_send(drv_data_t* dptr, struct rtnl_route* route)
{
    int family;
    dterm_t m;
    dterm_mark_t msg;
    dterm_mark_t prop;
    dterm_mark_t prop_list;

    family = rtnl_route_get_family(route);

    dterm_init(&m);

    // msg = {netlink,<port>,"route/route",<prop_list>}
    dterm_tuple_begin(&m, &msg);
    dterm_atom(&m,atm_netlink);
    dterm_port(&m, dptr->dport);
    dterm_string(&m,"route/route",11);

    dterm_list_begin(&m, &prop_list);

    dterm_tuple_begin(&m, &prop); {
	// scope
	dterm_atom(&m, atm_scope);
	dterm_int(&m, rtnl_route_get_scope(route));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	// tos
	dterm_atom(&m, atm_tos);
	dterm_int(&m, rtnl_route_get_tos(route));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	// protocol
	dterm_atom(&m, atm_protocol);
	dterm_int(&m, rtnl_route_get_protocol(route));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	dterm_atom(&m, atm_priority);
	dterm_int(&m, rtnl_route_get_priority(route));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	dterm_atom(&m, atm_type);
	dterm_int(&m, rtnl_route_get_type(route));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	dterm_atom(&m, atm_flags);
	dterm_int(&m, rtnl_route_get_flags(route));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	uint32_t value;
	dterm_atom(&m, atm_metric);
	// loop?
	rtnl_route_get_metric(route,0,&value);
	dterm_int(&m, (int)value);
    }
    dterm_tuple_end(&m, &prop);

    dterm_nl_addr(&m, atm_dst, family,  rtnl_route_get_dst(route));
    dterm_nl_addr(&m, atm_src, family,  rtnl_route_get_src(route));
    dterm_nl_addr(&m, atm_pref_src, family, rtnl_route_get_pref_src(route));

    dterm_tuple_begin(&m, &prop); {
	dterm_atom(&m, atm_index);
	dterm_int(&m, rtnl_route_get_iif(route));
    }
    dterm_tuple_end(&m, &prop);

    dterm_tuple_begin(&m, &prop); {
	dterm_mark_t nexthop_list;
	dterm_atom(&m, atm_nexthop);
	dterm_list_begin(&m, &nexthop_list); {
	    int len = rtnl_route_get_nnexthops(route);
	    int i;
	    for (i = 0; i < len; i++) {
		struct rtnl_nexthop *n;
		n = rtnl_route_nexthop_n(route, i);
		make_nexthop(&m, family, n);
	    }
	}
        dterm_list_end(&m, &nexthop_list);
    }
    dterm_tuple_end(&m, &prop);

    dterm_list_end(&m, &prop_list);
    dterm_tuple_end(&m, &msg);

    driver_output_term(dptr->port, dterm_data(&m), dterm_used_size(&m));

    if (m.base != m.data) {
	fprintf(stderr, "dterm allocated %d bytes\r\n",
		dterm_allocated_size(&m));
    }
    dterm_finish(&m);

    if (dptr->active > 0) {
	dptr->active--;
	if (dptr->active == 0)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
    }
}


static void link_obj_event(struct nl_object *obj, void *arg)
{
    drv_data_t* dptr = (drv_data_t*) arg;
    fprintf(stderr, "link_obj_event msgtype=%d\r\n", obj->ce_msgtype);
    fprintf(stderr, "oo_name = %s\n", obj->ce_ops->oo_name);
    if (strcmp(obj->ce_ops->oo_name, "route/link") == 0) {
	struct rtnl_link* link = (struct rtnl_link*) obj;
	link_obj_send(dptr, link);
    }
    else if (strcmp(obj->ce_ops->oo_name, "route/addr") == 0) {
	struct rtnl_addr* addr = (struct rtnl_addr*) obj;
	addr_obj_send(dptr, addr);
    }
    else if (strcmp(obj->ce_ops->oo_name, "route/route") == 0) {
	struct rtnl_route* route = (struct rtnl_route*) obj;
	route_obj_send(dptr, route);
    }
}


static int link_event_input(struct nl_msg *msg, void *arg)
{
    fprintf(stderr, "link_event_input\r\n");
    if (nl_msg_parse(msg, &link_obj_event, arg) < 0) {
        fprintf(stderr, "link_event_input: Unknown message type\r\n");
	// Exit nl_recvmsgs_def() and return to the main select()
	return NL_STOP;
    }
    return NL_OK;
}

static int nl_drv_init(void)
{
    atm_netlink = driver_mk_atom("netlink");
    atm_name  = driver_mk_atom("name");
    atm_index = driver_mk_atom("index");
    atm_label = driver_mk_atom("label");
    atm_mtu = driver_mk_atom("mtu");
    atm_txqlen = driver_mk_atom("txqlen");
    atm_flags = driver_mk_atom("flags");
    atm_if_state = driver_mk_atom("if_state");
    atm_if_lower_state = driver_mk_atom("if_lower_state");
    atm_oper_status = driver_mk_atom("oper_status");
    atm_link_mode = driver_mk_atom("link_mode");
    atm_addr = driver_mk_atom("addr");
    atm_bcast = driver_mk_atom("bcast");
    atm_peer = driver_mk_atom("peer");
    atm_mcast = driver_mk_atom("mcast");
    atm_prefixlen = driver_mk_atom("prefixlen");
    atm_qdisc = driver_mk_atom("qdisc");
    atm_inet = driver_mk_atom("inet");
    atm_inet6 = driver_mk_atom("inet6");
    atm_undefined = driver_mk_atom("undefined");
    atm_dst = driver_mk_atom("dst");
    atm_src = driver_mk_atom("src");
    atm_pref_src = driver_mk_atom("pref_src");
    atm_metric = driver_mk_atom("metric");
    atm_scope = driver_mk_atom("scope");
    atm_tos = driver_mk_atom("tos");
    atm_protocol = driver_mk_atom("protocol");
    atm_priority = driver_mk_atom("priority");
    atm_type = driver_mk_atom("type");
    atm_weight = driver_mk_atom("weight");
    atm_gateway = driver_mk_atom("gateway");
    atm_realms = driver_mk_atom("realms");
    atm_nexthop = driver_mk_atom("nexthop");


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
    // fprintf(stderr, "nl_drv_finish called!!!\r\n");
}

static void       nl_drv_stop(ErlDrvData d)
{
    drv_data_t* dptr = (drv_data_t*) d;

    // fprintf(stderr, "nl_drv_stop called!!!\r\n");

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
	if (dptr->route_cache) {
	    nl_cache_free(dptr->route_cache);
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

//	if ((err = nl_socket_add_membership(dptr->sock, RTNLGRP_LINK)) < 0) {

	if ((err = nl_socket_add_memberships(dptr->sock,
					     RTNLGRP_LINK,
					     RTNLGRP_IPV4_IFADDR,
					     RTNLGRP_IPV6_IFADDR,
					     RTNLGRP_IPV4_ROUTE,
					     RTNLGRP_IPV6_ROUTE,
					     RTNLGRP_NONE)) < 0) {
	    err = 0;
	    fprintf(stderr, "nl_socket_add_membership: error: %s\r\n",
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

	if (!(dptr->route_cache = nl_cli_route_alloc_cache(dptr->sock,0))) {
	    err = errno;
	    fprintf(stderr, "unable to allocate nl_cli_route_cache\r\n");
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
	// First do the link cache
	struct nl_object* obj = nl_cache_get_first(dptr->link_cache);
	if (obj) {
	    do {
		link_obj_send(dptr, (struct rtnl_link*) obj);
		obj = nl_cache_get_next(obj);
	    } while(obj);
	}
	// Now do the addr cache
	obj = nl_cache_get_first(dptr->addr_cache);
	if (obj) {
	    do {
		addr_obj_send(dptr, (struct rtnl_addr*) obj);
		obj = nl_cache_get_next(obj);
	    } while(obj);
	}
	// And the route cache
	obj = nl_cache_get_first(dptr->route_cache);
	if (obj) {
	    do {
		route_obj_send(dptr, (struct rtnl_route*) obj);
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
    if (dptr->route_cache != NULL) {
	nl_cache_free(dptr->route_cache);
	dptr->route_cache = NULL;
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
    dptr->dport = driver_mk_port(port);

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
