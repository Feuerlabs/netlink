.PHONY: libnl

PRIVDIR := ../priv

all : libnl $(PRIVDIR)/netlink_drv.so

libnl:
	(cd ..; make)

override CFLAGS += -fpic -shared -I../libnl/include

$(PRIVDIR)/netlink_drv.so : netlink_drv.o dterm.o
	$(CC) -shared -fpic $(LDFLAGS) -L../libnl/lib -lnl-3 -lnl-cli-3 -lnl-route-3 -o $@ $^

clean:
	$(RM) -f $(PRIVDIR)/netlink_drv.so netlink_drv.o dterm.o
