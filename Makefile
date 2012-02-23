ERLC=erlc
ERLC_OPTS=-v -Wall -Werror +o3 -o ebin

all: ebin/hurricane.beam ebin/hurricane_config_server.beam ebin/hurricane_distribution_manager.beam ebin/hurricane_http_server.beam ebin/hurricane_log_server.beam ebin/hurricane_message_delegate.beam ebin/hurricane_stdio_server.beam ebin/hurricane_supervisor.beam ebin/hurricane_tcp_server.beam ebin/hurricane_utils.beam

all-native: ERLC_OPTS += +native
all-native: all

clean:
	/usr/bin/env rm -f ebin/*.beam

ebin/hurricane.beam: erl_modules/hurricane.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane.erl

ebin/hurricane_config_server.beam: erl_modules/hurricane_config_server.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane_config_server.erl

ebin/hurricane_distribution_manager.beam: erl_modules/hurricane_distribution_manager.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane_distribution_manager.erl

ebin/hurricane_http_server.beam: erl_modules/hurricane_http_server.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane_http_server.erl

ebin/hurricane_log_server.beam: erl_modules/hurricane_log_server.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane_log_server.erl

ebin/hurricane_message_delegate.beam: erl_modules/hurricane_message_delegate.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane_message_delegate.erl

ebin/hurricane_stdio_server.beam: erl_modules/hurricane_stdio_server.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane_stdio_server.erl

ebin/hurricane_supervisor.beam: erl_modules/hurricane_supervisor.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane_supervisor.erl

ebin/hurricane_tcp_server.beam: erl_modules/hurricane_tcp_server.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane_tcp_server.erl

ebin/hurricane_utils.beam: erl_modules/hurricane_utils.erl
	$(ERLC) $(ERLC_OPTS) erl_modules/hurricane_utils.erl
