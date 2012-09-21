-module(adto_k1api_tests).

-include("adto.hrl").
-include_lib("eunit/include/eunit.hrl").

start_uuid() ->
	ok = application:start(uuid).
stop_uuid(_) ->
	application:stop(uuid).


k1api_dto_test_()->
	{setup,
	fun start_uuid/0,
	fun stop_uuid/1,
	[?_test(sms_delivery_status_request()),
	?_test(k1api_sms_delivery_status_response())]}.

%% ===================================================================
%% k1api Sms Delivery Status Request Tests
%% ===================================================================

sms_delivery_status_request() ->
	DTO = #k1api_sms_delivery_status_request_dto{
		id = uuid:newid(),
		customer_id = uuid:newid(),
		user_id = <<"user">>,
		sms_request_id = uuid:newid(),
		address = #addr_dto{addr = <<"375269090909">>, ton = 1, npi = 1}
	},
	?assertEqual(DTO, decode(#k1api_sms_delivery_status_request_dto{}, encode(DTO))).

%% ===================================================================
%% k1api Sms Delivery Status Response Tests
%% ===================================================================

statuses() ->
	[submitted,
	success_waiting_delivery,
	success_no_delivery,
	failure,
	enroute,
	delivered,
	expired,
	deleted,
	undeliverable,
	accepted,
	unknown,
	rejected,
	unrecognized].

k1api_sms_delivery_status_response() ->
	StatusesDTO = [#k1api_sms_status_dto{
		address = #addr_dto{addr = <<"375269090909">>, ton = 1, npi = 1},
		status = Status
	} || Status <- statuses()],
	DTO = #k1api_sms_delivery_status_response_dto{
		id = uuid:newid(),
		statuses = StatusesDTO
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#k1api_sms_delivery_status_response_dto{}, Bin).

%% ===================================================================
%% Internals
%% ===================================================================

encode(DTO) ->
	{ok, Bin} = adto:encode(DTO),
	Bin.

decode(Type, Bin) ->
	{ok, DTO} = adto:decode(Type, Bin),
	DTO.
