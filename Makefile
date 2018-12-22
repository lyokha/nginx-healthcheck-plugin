BASE_NAME := ngx_healthcheck

PLUGIN_C  := $(BASE_NAME)_plugin.c
PLUGIN_O  := $(BASE_NAME)_plugin.o
PLUGIN_HS := $(BASE_NAME).hs
TARGET    := $(BASE_NAME).so
GARBAGE   := $(BASE_NAME).hi $(BASE_NAME).o $(BASE_NAME)_stub.h $(PLUGIN_O)

ifeq ($(shell cabal help v1-sandbox 2>/dev/null; echo $$?),0)
    CABAL_CMD_PREFIX := v1-
else
    CABAL_CMD_PREFIX :=
endif
CABAL_SANDBOX := cabal $(CABAL_CMD_PREFIX)sandbox
CABAL_INSTALL := cabal $(CABAL_CMD_PREFIX)install
CABAL_EXEC    := cabal $(CABAL_CMD_PREFIX)exec

HSLIBS         ?= $(shell ldd $(TARGET) | $(FILTER_HS_LIBS))
FILTER_HS_LIBS := sed -r '/^\s*libHS/!d; s/^(.*)\s+=>\s+(\S+).*/\2/'
HSLIBS_DIR     := hslibs

PATCHELF := patchelf

all : $(TARGET)

$(PLUGIN_O) : check_ngx_home $(PLUGIN_C)
	gcc -Wall -O2 -fPIC -c -o $(PLUGIN_O)                               \
	    -I "${NGX_HOME}"/src/core          -I "${NGX_HOME}"/src/http    \
	    -I "${NGX_HOME}"/src/http/modules  -I "${NGX_HOME}"/src/event   \
	    -I "${NGX_HOME}"/src/event/modules -I "${NGX_HOME}"/src/os/unix \
	    -I "${NGX_HOME}"/objs $(PLUGIN_C)

$(TARGET) : $(PLUGIN_HS) $(PLUGIN_O)
	$(CABAL_SANDBOX) init
	$(CABAL_INSTALL) --only-dependencies
	$(CABAL_EXEC) --                                        \
	    ghc -Wall -O2 -dynamic -shared -fPIC                \
	        -L"$(shell ghc --print-libdir)"/rts             \
	        -lHSrts_thr-ghc"$(shell ghc --numeric-version)" \
	         $(PLUGIN_O) $(PLUGIN_HS) -o $(TARGET)          \
	        -ignore-package regex-pcre -fforce-recomp

.SILENT : $(HSLIBS_DIR) patchlib check_ngx_home

$(HSLIBS_DIR) : $(TARGET)
	if [ -n "$(HSLIBS)" ];                                     \
	then                                                       \
	    mkdir -p "$(HSLIBS_DIR)";                              \
	    cp -uv $(HSLIBS) "$(HSLIBS_DIR)";                      \
	    touch "$(HSLIBS_DIR)";                                 \
	else                                                       \
	    echo "Haskell libraries were not found in $(TARGET)!"; \
	fi

.PHONY : patchlib check_ngx_home clean

patchlib :
	if [ -n "${HSLIBS_INSTALL_DIR}" ];                                   \
	then                                                                 \
	    if [ -f $(TARGET) ];                                             \
	    then                                                             \
	        rpath=$$($(PATCHELF) --print-rpath $(TARGET));               \
	        case "$$rpath" in                                            \
	            "${HSLIBS_INSTALL_DIR}":*)                               \
	                echo "Library $(TARGET) has been already patched!";; \
	            *)                                                       \
	                $(PATCHELF)                                          \
	                    --set-rpath "${HSLIBS_INSTALL_DIR}:$$rpath"      \
	                    $(TARGET);                                       \
	                echo "Library $(TARGET) has been patched:";;         \
	        esac;                                                        \
	        $(PATCHELF) --print-rpath $(TARGET);                         \
	    else                                                             \
	        echo "Library $(TARGET) has not been built!";                \
	    fi;                                                              \
	else                                                                 \
	    echo "Environment variable HSLIBS_INSTALL_DIR is not set!";      \
	fi

check_ngx_home :
	if [ -z "${NGX_HOME}" ];                              \
	then                                                  \
	    echo "Environment variable NGX_HOME is not set!"; \
	    echo "Building C plugin will probably fail!";     \
	    echo;                                             \
	fi;                                                   \

clean :
	rm -f $(GARBAGE) $(TARGET)
	$(CABAL_SANDBOX) delete
	rm -rf "$(HSLIBS_DIR)"

