REBAR=./rebar -C rebar.test.config

all: test

test: clean compile
	@erl -noshell -pa ebin/ \
					dep/*/ebin/ \
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
					dep/*/ebin/ \
		-eval 'application:start(alley_dto)' \
		-eval 'adto_just_tests:just_incoming_sms_test()' \
		-s init stop
