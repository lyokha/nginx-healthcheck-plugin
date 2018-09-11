PLUGIN_O := ngx_healthcheck_plugin.o
TARGET   := ngx_healthcheck.so
GARBAGE  := ngx_healthcheck.hi ngx_healthcheck.o ngx_healthcheck_stub.h

HSLIBS         ?= $(shell ldd $(TARGET) | $(FILTER_HS_LIBS))
FILTER_HS_LIBS := sed -r '/^\s*libHS/!d; s/^(.*)\s+=>\s+(\S+).*/\2/'
HSLIBS_DIR     := hslibs

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

$(HSLIBS_DIR) : $(TARGET)
	if [ -n "$(HSLIBS)" ];             \
	then                               \
	    mkdir -p $(HSLIBS_DIR);        \
	    cp -u $(HSLIBS) $(HSLIBS_DIR); \
	    touch $(HSLIBS_DIR);           \
	fi

.PHONY : patchlib clean

patchlib :
	@if [ -n "${HSLIBS_INSTALL_DIR}" ];                                   \
	then                                                                  \
	    if [ -f $(TARGET) ];                                              \
	    then                                                              \
	        rpath=$$($(PATCHELF) --print-rpath $(TARGET));                \
	        case $$rpath in                                               \
	            ${HSLIBS_INSTALL_DIR}:*)                                  \
	                echo "Library $(TARGET) has been already patched!";;  \
	            *)                                                        \
	                $(PATCHELF) --set-rpath ${HSLIBS_INSTALL_DIR}:$$rpath \
	                    $(TARGET);                                        \
	                echo "Library $(TARGET) has been patched!";;          \
	        esac;                                                         \
	    else                                                              \
	        echo "Library $(TARGET) has not been built!";                 \
	    fi;                                                               \
	else                                                                  \
	    echo "Environment variable HSLIBS_INSTALL_DIR is not set!";       \
	fi

clean :
	rm -f $(GARBAGE) $(PLUGIN_O) $(TARGET)
	cabal sandbox delete
	rm -rf $(HSLIBS_DIR)

