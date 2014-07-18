PROJECT=lynch

DEPS = cowboy lager
dep_cowboy = pkg://cowboy master
dep_lager = https://github.com/basho/lager master

ERLC_OPTS = +debug_info +'{parse_transform, lager_transform}'

include erlang.mk
