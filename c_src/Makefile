PRIVDIR := ../priv

all : $(PRIVDIR)/netlink_drv.so

override CFLAGS += -fpic -shared

$(PRIVDIR)/netlink_drv.so : netlink_drv.o dterm.o
	$(CC) -shared -fpic $(LDFLAGS) -lnl-3 -lnl-cli-3 -lnl-route-3 -o $@ $^

clean:
	$(RM) -f $(PRIVDIR)/netlink_drv.so netlink_drv.o dterm.o
