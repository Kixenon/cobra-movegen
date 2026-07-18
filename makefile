CXX = clang++

FLAGS = -Wall -Wextra -Wshadow -Wmissing-declarations -Wconversion -fno-exceptions -std=c++23

backend ?= row
ifeq ($(backend),col)
FLAGS += -DCOBRA_COL_MAJOR
endif

debug ?= no
optimise ?= yes

ifeq ($(debug),no)
	FLAGS += -DNDEBUG
endif

ifeq ($(optimise),yes)
	FLAGS += -O3 -funroll-loops -march=native -mtune=native -flto -pipe
endif

BUILD_DIR = bin
OBJ_DIR = $(BUILD_DIR)/obj

LIB_SRCS = $(wildcard src/*.cpp)
APP_SRCS = $(wildcard apps/*.cpp)
APP_NAMES = $(basename $(notdir $(APP_SRCS)))
APP_BINS = $(patsubst %, $(BUILD_DIR)/%, $(APP_NAMES))
LIB_OBJS = $(patsubst %.cpp, $(OBJ_DIR)/%.o, $(LIB_SRCS))
APP_OBJS = $(patsubst %.cpp, $(OBJ_DIR)/%.o, $(APP_SRCS))
DEPS = $(LIB_OBJS:.o=.d) $(APP_OBJS:.o=.d)

.PHONY: all clean help $(APP_NAMES)

all: help

$(APP_NAMES): %: $(BUILD_DIR)/%

$(BUILD_DIR)/%: clean $(OBJ_DIR)/apps/%.o $(LIB_OBJS)
	@mkdir -p $(dir $@)
	$(CXX) $(FLAGS) $(filter-out clean,$^) -o $@
	@echo "Built $@"

$(OBJ_DIR)/%.o: %.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(FLAGS) -MMD -MP -c $< -o $@

-include $(DEPS)

clean:
	@echo "Cleaning all build artifacts..."
	rm -rf $(BUILD_DIR)

help:
	@echo ""
	@echo "Cobra Movegen"
	@echo "Usage: make [app]"
	@echo "Example: make bench"
	@echo ""
	@echo "Available apps:"
	@$(foreach app,$(APP_NAMES),echo "  $(app)";)
	@echo ""
	@echo "Other targets:"
	@echo "  help              Show this message (default)"
	@echo "  clean             Remove all build artifacts"
	@echo ""
	@echo "Configuration (pass as arguments):"
	@echo "  backend=col       Use column-major backend (default: row)"
	@echo "  debug=yes         Enable debug build (default: no)"
	@echo "  optimise=no       Disable optimisations (default: yes)"
	@echo ""
	@echo "[run make clean before rebuilding with different configurations]"
	@echo ""