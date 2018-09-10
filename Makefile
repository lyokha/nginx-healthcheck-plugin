PLUGIN_O := ngx_healthcheck_plugin.o
TARGET   := ngx_healthcheck.so
GARBAGE  := ngx_healthcheck.hi ngx_healthcheck.o ngx_healthcheck_stub.h

HSLIBS         ?= $(shell ldd $(TARGET) | $(FILTER_HS_LIBS))
FILTER_HS_LIBS := sed -r '/^\s*libHS/!d; s/^(.*)\s+=>\s+(\S+).*/\2/'

PATCHELF := patchelf

all : $(TARGET)

$(PLUGIN_O) : ngx_healthcheck_plugin.c
	gcc -Wall -O2 -fPIC -c -o $(PLUGIN_O)                           \
	    -I ${NGX_HOME}/src/core          -I ${NGX_HOME}/src/http    \
	    -I ${NGX_HOME}/src/http/modules  -I ${NGX_HOME}/src/event   \
	    -I ${NGX_HOME}/src/event/modules -I ${NGX_HOME}/src/os/unix \
	    -I ${NGX_HOME}/objs ngx_healthcheck_plugin.c

$(TARGET) : ngx_healthcheck.hs $(PLUGIN_O)
	cabal sandbox init
	cabal install --only-dependencies
	cabal exec --                                         \
	    ghc -Wall -O2 -dynamic -shared -fPIC              \
	        -L$(shell ghc --print-libdir)/rts             \
	        -lHSrts_thr-ghc$(shell ghc --numeric-version) \
	         $(PLUGIN_O) ngx_healthcheck.hs -o $(TARGET)  \
	        -ignore-package regex-pcre -fforce-recomp

hslibs: $(TARGET)
	if [ -n "$(HSLIBS)" ];   \
	then                     \
	    mkdir -p hslibs;     \
	    cp $(HSLIBS) hslibs; \
	fi;                      \
	if [ -n "${HSLIBS_INSTALL_DIR}" ];                                \
	then                                                              \
	    rpath=$$($(PATCHELF) --print-rpath $(TARGET));                \
	    case $$rpath in                                               \
	        ${HSLIBS_INSTALL_DIR}:*)                                  \
	            echo "$(TARGET) has been already patched!";;          \
	        *)                                                        \
	            $(PATCHELF) --set-rpath ${HSLIBS_INSTALL_DIR}:$$rpath \
	                $(TARGET);                                        \
	            echo "$(TARGET) has been patched!";;                  \
	    esac;                                                         \
	fi

.PHONY: clean

clean :
	rm -f $(GARBAGE) $(PLUGIN_O) $(TARGET)
	cabal sandbox delete
	rm -rf hslibs

