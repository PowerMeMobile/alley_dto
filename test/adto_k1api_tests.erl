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
	?_test(sms_delivery_status_response()),
	?_test(retrieve_sms_request()),
	?_test(retrieve_sms_request2()),
	?_test(retrieve_sms_response()),
	?_test(remove_retrieved_sms_request()),
	?_test(bad_type_encode()),
	?_test(bad_type_decode())]}.

%% ===================================================================
%% Sms Delivery Status Request Tests
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
%% Sms Delivery Status Response Tests
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

sms_delivery_status_response() ->
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
%% Retrieve Sms Request Tests
%% ===================================================================

%% test with defined optional parameter batch_size
retrieve_sms_request() ->
	DTO = #k1api_retrieve_sms_request_dto{
		id = uuid:newid(),
		customer_id = uuid:newid(),
		user_id = <<"user">>,
		dest_addr = #addr_dto{addr = <<"375269090909">>, ton = 1, npi = 1},
		batch_size = 5
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#k1api_retrieve_sms_request_dto{}, Bin).

%% test with UNdefined optional parameter batch_size
retrieve_sms_request2() ->
	DTO = #k1api_retrieve_sms_request_dto{
		id = uuid:newid(),
		customer_id = uuid:newid(),
		user_id = <<"user">>,
		dest_addr = #addr_dto{addr = <<"375269090909">>, ton = 1, npi = 1},
		batch_size = undefined
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#k1api_retrieve_sms_request_dto{}, Bin).


%% ===================================================================
%% Retrieve Sms Response Tests
%% ===================================================================

retrieve_sms_response() ->
	MessageDTO = #k1api_retrieved_sms_dto{
		datetime = 1348574534,
		sender_addr = #addr_dto{addr = <<"375269090909">>, ton = 1, npi = 1},
		message_id = <<"123">>,
		message = <<"message">>
	},
	DTO = #k1api_retrieve_sms_response_dto{
		id = uuid:newid(),
		messages = [MessageDTO],
		total = 5
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#k1api_retrieve_sms_response_dto{}, Bin).


%% ===================================================================
%% Remove Retrieved Sms Request Tests
%% ===================================================================

remove_retrieved_sms_request() ->
	DTO = #k1api_remove_retrieved_sms_request_dto{
		id = uuid:newid(),
		message_ids = [<<"123">>, <<"456">>]
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#k1api_remove_retrieved_sms_request_dto{}, Bin).

%% ===================================================================
%% Bad Type Request
%% ===================================================================

bad_type_encode() ->
	BadDTO = unsupported_type,
	?assertError({k1api_encode_not_supported,unsupported_type} , adto_k1api:encode(BadDTO)).

bad_type_decode() ->
	BadDTO = unsupported_type,
	?assertError({k1api_decode_not_supported,unsupported_type} , adto_k1api:decode(BadDTO, <<>>)).

%% ===================================================================
%% Internals
%% ===================================================================

encode(DTO) ->
	{ok, Bin} = adto:encode(DTO),
	Bin.

decode(Type, Bin) ->
	{ok, DTO} = adto:decode(Type, Bin),
	DTO.
