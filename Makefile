REBAR=./rebar -C rebar.test.config

all: test

test: clean compile
	@erl -noshell -pa ebin/ \
					deps/*/ebin/ \
		-eval 'application:start(uuid)' \
		-eval 'application:start(alley_dto)' \
		-eval 'eunit:test("ebin",[verbose])' \
		-s init stop

get-deps:
	@$(REBAR) get-deps

compile: get-deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean

dev: compile
	@erl -noshell -pa ebin/ \
					deps/*/ebin/ \
		-eval 'application:start(uuid)' \
		-eval 'application:start(alley_dto)' \
		-eval 'adto_just_tests:just_sms_response_test()' \
		-s init stop
