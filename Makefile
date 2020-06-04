BASE_NAME := ngx_healthcheck

PLUGIN    := $(BASE_NAME)_plugin
PLUGIN_C  := $(PLUGIN).c
PLUGIN_O  := $(PLUGIN).o
PLUGIN_HS := $(BASE_NAME).hs
TARGET    := $(BASE_NAME).so
GARBAGE   := $(BASE_NAME).hi $(BASE_NAME).o $(BASE_NAME)_stub.h $(PLUGIN_O)

ifeq ($(shell cabal help v1-sandbox 2>&1 >/dev/null; echo $$?),0)
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

TARGET_DEPS := $(PLUGIN_HS)
TARGET_LINK := $(PLUGIN_O)

ifdef NGX_MODULE_PATH
    TARGET_LINK := -L${NGX_MODULE_PATH} -l$(PLUGIN)
else
    TARGET_DEPS += $(PLUGIN_O)
endif

all : $(TARGET)

$(PLUGIN_O) : $(PLUGIN_C)
	@if [ -z "${NGX_HOME}" ];                             \
	then                                                  \
	    echo "Environment variable NGX_HOME is not set!"; \
	    echo "Building C plugin will probably fail!";     \
	    echo;                                             \
	fi
	gcc -Wall -O2 -fPIC -c -o $(PLUGIN_O)                               \
	    -I "${NGX_HOME}"/src/core          -I "${NGX_HOME}"/src/http    \
	    -I "${NGX_HOME}"/src/http/modules  -I "${NGX_HOME}"/src/event   \
	    -I "${NGX_HOME}"/src/event/modules -I "${NGX_HOME}"/src/os/unix \
	    -I "${NGX_HOME}"/objs $(PLUGIN_C)

$(TARGET) : $(TARGET_DEPS)
	$(CABAL_SANDBOX) init
	$(CABAL_INSTALL)
	$(CABAL_EXEC) --                                        \
	    ghc -Wall -O2 -dynamic -shared -fPIC                \
	        -lHSrts_thr-ghc"$(shell ghc --numeric-version)" \
	         $(TARGET_LINK) $(PLUGIN_HS) -o $(TARGET)       \
	        -fforce-recomp

.SILENT : $(HSLIBS_DIR) patchlib

$(HSLIBS_DIR) : $(TARGET)
	if [ -n "$(HSLIBS)" ];                                     \
	then                                                       \
	    mkdir -p "$(HSLIBS_DIR)";                              \
	    cp -uv $(HSLIBS) "$(HSLIBS_DIR)";                      \
	    touch "$(HSLIBS_DIR)";                                 \
	else                                                       \
	    echo "Haskell libraries were not found in $(TARGET)!"; \
	fi

.PHONY : patchlib clean lenient-clean

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

clean :
	rm -f $(GARBAGE) $(TARGET)
	$(CABAL_SANDBOX) delete
	rm -rf "$(HSLIBS_DIR)"

lenient-clean :
	rm -f $(GARBAGE) $(TARGET)
	rm -rf "$(HSLIBS_DIR)"

