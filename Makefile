## ---- BEGIN COPYRIGHT -------------------------------------------------------
##
## Copyright © 2012 Feuerlabs, Inc. All rights reserved.
##
## This Source Code Form is subject to the terms of the Mozilla Public
## License, v. 2.0. If a copy of the MPL was not distributed with this
## file, You can obtain one at http://mozilla.org/MPL/2.0/.
##
## ---- END COPYRIGHT ---------------------------------------------------------


# Makefile to build libnl
NL_NAME=libnl
NL_VER=3.2.11
NL_DIR=$(NL_NAME)-$(NL_VER)
NL_TAR=$(NL_DIR).tar.gz
NL_INSTALL_DIR=$(PWD)/libnl

UNPACK_TARGET=$(NL_DIR)/ChangeLog

CONFIG_TARGET=$(NL_DIR)/config.log

BUILD_TARGET=$(NL_DIR)/lib/libnl-3.la

INSTALL_TARGET=$(NL_INSTALL_DIR)/lib/libnl-3.a


all: $(INSTALL_TARGET)

$(INSTALL_TARGET): $(BUILD_TARGET)
	(cd $(NL_DIR); make install)

$(BUILD_TARGET): $(CONFIG_TARGET)
	(cd $(NL_DIR); make)

$(CONFIG_TARGET): $(UNPACK_TARGET)
# Check if this is a local build or not
ifeq ($(X_COMP_TARGET_ARCH),)
	(cd $(NL_DIR); 	 LDFLAGS="" ./configure --disable-dependency-tracking --prefix=$(NL_INSTALL_DIR))
else
	(cd $(NL_DIR); \
	 CC=$(CC) \
	 CFLAGS="$(CFLAGS)" \
	 LDFLAGS="" \
	 LD=$(LD) \
	 AR=$(AR) \
	 RANLIB=$(RANLIB) \
	 ./configure --host $(X_COMP_TARGET_ARCH) --disable-dependency-tracking --prefix=$(NL_INSTALL_DIR))
endif
$(UNPACK_TARGET):
	tar xf $(NL_TAR)

clean:
	rm -rf $(NL_DIR) $(NL_INSTALL_DIR)