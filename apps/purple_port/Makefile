ERLANG_PATH ?= $(shell erl -eval 'io:format("~s", [code:root_dir()]).' -s init stop -noshell)
CFLAGS_PURPLE= $(shell pkg-config --cflags purple)
LDFLAGS_PURPLE= $(shell pkg-config --libs purple)

CFLAGS += -I$(ERLANG_PATH)/usr/include/ $(CFLAGS_PURPLE)
LDFLAGS += $(LDFLAGS_PURPLE) -L$(ERLANG_PATH)/usr/lib/ -pthread

all: priv/purple_port

port: priv/purple_port

priv/purple_port: c_src/purple_port.c
	gcc $(CFLAGS) $(LDFLAGS) -o priv/purple_port c_src/purple_port.c -lerl_interface -lei

clean:
	rm priv/purple_port
