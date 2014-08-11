PROJECT=lynch

DEPS = cowboy jiffy lager
dep_cowboy = pkg://cowboy master
dep_jiffy = https://github.com/davisp/jiffy master
dep_lager = https://github.com/basho/lager master

ERLC_OPTS = +debug_info +'{parse_transform, lager_transform}'

default: bower rel

include erlang.mk

.PHONY: bower

BOWER = /usr/bin/bower

bower: $(BOWER)
	cd priv; bower install
