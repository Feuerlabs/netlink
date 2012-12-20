.PHONY: libnl

PRIVDIR := ../priv

all : libnl $(PRIVDIR)/netlink_drv.so

libnl:
	(cd ..; make X_COMP_TARGET_ARCH=arm-tplino-linux-gnueabi)
	cp ../libnl/lib/lib*.so ../priv
	rm -rf ../libnl

override CFLAGS += -fpic -shared -I../libnl/include/libnl3

$(PRIVDIR)/netlink_drv.so : netlink_drv.o dterm.o
	$(CC) -shared -fpic $(LDFLAGS)  -o $@ $^ ../priv/libnl-3.so ../priv/libnl-cli-3.so ../priv/libnl-route-3.so

clean:
	$(RM) -f $(PRIVDIR)/netlink_drv.so netlink_drv.o dterm.o
