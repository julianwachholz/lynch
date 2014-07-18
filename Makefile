PROJECT=lynch

DEPS = cowboy lager
dep_cowboy = pkg://cowboy master
dep_lager = https://github.com/basho/lager master

include erlang.mk
