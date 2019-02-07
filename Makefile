PROJECT = test_cowboy
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

ERLC_OPTS = +warn_missing_spec
ERLC_OPTS += +warn_unused_vars +warn_unused_function
ERLC_OPTS += +warn_unused_import +warn_unused_record
ERLC_OPTS += +warn_deprecated_function +warn_deprecated_type
ERLC_OPTS += +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
ERLC_OPTS += +warn_export_all +warn_untyped_record
ERLC_OPTS += +debug_info +bin_opt_info
#ERLC_OPTS += +warnings_as_errors

DEPS = cowboy jiffy
dep_cowboy = git https://github.com/ninenines/cowboy.git 2.6.1
dep_cowboy = https://github.com/davisp/jiffy.git 0.15.2

TEST_DEPS = $(if $(CI_ERLANG_MK),ci.erlang.mk) ct_helper gun proper
dep_ct_helper = git https://github.com/extend/ct_helper master
dep_gun = git https://github.com/ninenines/gun master

include erlang.mk
