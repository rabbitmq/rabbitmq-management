PROJECT = rabbitmq_management
PROJECT_DESCRIPTION = RabbitMQ Management Console
PROJECT_MOD = rabbit_mgmt_app

define PROJECT_ENV
[
	    {http_log_dir,      none},
	    {load_definitions,  none},
	    {management_db_cache_multiplier, 5},
	    {process_stats_gc_timeout, 300000},
	    {stats_event_max_backlog, 250},

	    {cors_allow_origins, []},
	    {cors_max_age, 1800},
	    {content_security_policy, "default-src 'self'"}
	  ]
endef

define PROJECT_APP_EXTRA_KEYS
	{broker_version_requirements, []}
endef

DEPS = rabbit_common rabbit amqp_client cowboy cowlib rabbitmq_web_dispatch rabbitmq_management_agent
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers proper
LOCAL_DEPS += mnesia ranch ssl crypto public_key

# FIXME: Add Ranch as a BUILD_DEPS to be sure the correct version is picked.
# See rabbitmq-components.mk.
BUILD_DEPS += ranch

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk

# --------------------------------------------------------------------
# Distribution.
# --------------------------------------------------------------------

list-dist-deps::
	@echo bin/rabbitmqadmin

prepare-dist::
	$(verbose) sed 's/%%VSN%%/$(PROJECT_VERSION)/' bin/rabbitmqadmin \
		> $(EZ_DIR)/priv/www/cli/rabbitmqadmin
