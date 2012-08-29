-module(adto_funnel_tests).

-include("adto.hrl").
-include("FunnelAsn.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Variables
%% ===================================================================

bin_uuid() -> <<138,192,76,106,232,107,75,217,141,162,249,70,128,211,111,2>>.

str_uuid() -> "1ad95af3-b33c-4eba-be89-a31ff0b14d39".

precise_time_dto() ->
	#precise_time_dto{
		time = utc_time(),
		milliseconds = 573
	}.
utc_time() ->
	"120823124151".

addr_dto() ->
	#addr_dto{
		addr = <<"addr">>,
		ton = 0,
		npi = 1
	}.

network_dto() ->
	#network_dto{
	  id = bin_uuid(),
	  country_code = <<"country_code">>,
	  numbers_len = 12,
	  prefixes = [<<"12">>, <<"34">>],
	  provider_id = bin_uuid()
	 }.

provider_dto() ->
	#provider_dto{
		id = bin_uuid(),
		gateway = bin_uuid(),
		bulk_gateway  = bin_uuid(),
		receipts_supported = true
	}.

%% ===================================================================
%% Funnel Auth Tests
%% ===================================================================

funnel_auth_request_test() ->
	DTO = #funnel_auth_request_dto{
		connection_id = adto_uuid:newid(),
		ip = <<"127.0.0.1">>,
		customer_id = <<"test-sys-id">>,
		user_id = <<"user">>,
		password = <<"password">>,
		type = transmitter,
		is_cached = true,
		timestamp = #precise_time_dto{time = <<"120827114232">>, milliseconds = 1},
		expiration = #precise_time_dto{time = <<"120827114232">>, milliseconds = 1}
	},
	{ok, Bin} = adto:encode(DTO),
	{ok, DTO} = adto:decode(#funnel_auth_request_dto{}, Bin).

%% funnel_auth_request_encode_test() ->
%% 	erlang:error(not_implemented).

%% funnel_auth_request_decode_test() ->
%% 	%% create test asn bin message
%%    	AsnReq = #'BindRequest'{
%% 		connectionId = str_uuid(),
%% 		remoteIp = "127.0.0.1",
%% 		customerId = "system_id",
%% 		userId = "user_id",
%% 		password = "password",
%% 		type = transmitter,
%% 		isCached = true,
%% 		timestamp = {'PreciseTime',"120823124152",573},
%% 		expiration =  {'PreciseTime',"120823124152",573}
%% 	},
%% 	{ok, List} = 'FunnelAsn':encode('BindRequest', AsnReq),
%% 	Binary = list_to_binary(List),
%% 	io:format("Binary: ~p", [Binary]),

%% 	%% try to decode test asn bin message
%% 	{ok, DTO} = adto:decode(#funnel_auth_request_dto{}, Binary),
%% 	io:format("DTO: ~p", [DTO]),
%% 	#funnel_auth_request_dto{
%% 		connection_id = ConnectionID,
%% 		customer_id = CustomerID,
%% 		user_id = UserID,
%% 		password = Password,
%% 		type = SMPPType
%% 	} = DTO,
%% 	io:format("ConnectionID: ~p", [ConnectionID]),
%% 	true = adto_uuid:is_valid(ConnectionID),
%% 	true = is_binary(ConnectionID),
%% 	true = is_binary(CustomerID),
%% 	true = is_binary(UserID),
%% 	true = is_binary(Password),
%% 	true = is_atom(SMPPType).

%% funnel_auth_response_encode_test() ->
%% 	CustomerDTO = #funnel_auth_response_customer_dto{
%% 		  id = <<"customer_system_id">>,
%% 		  uuid = bin_uuid(),
%% 		  priority = 1,
%% 		  rps = undefined,
%% 		  allowed_sources = [addr_dto()],
%% 		  default_source = undefined,
%% 		  networks = [network_dto()],
%% 		  providers = [provider_dto()],
%% 		  default_provider_id = undefined,
%% 		  receipts_allowed  = true,
%% 		  no_retry  = true,
%% 		  default_validity  = <<"string">>,
%% 		  max_validity = 12345
%% 	},
%% 	DTO = #funnel_auth_response_dto{
%% 		connection_id = bin_uuid(),
%% 		result = {customer, CustomerDTO}
%% 	},
%% 	{ok, Bin} = adto:encode(DTO),
%% 	true = is_binary(Bin).

%% funnel_auth_error_response_encode_test() ->
%% 	DTO = #funnel_auth_response_dto{
%% 		connection_id = bin_uuid(),
%% 		result = {error, "test"}
%% 	},
%% 	{ok, Bin} = adto:encode(DTO),
%% 	true = is_binary(Bin).

%% funnel_auth_response_decode_test() ->
%% 	erlang:error(not_implemented).

%% %% ===================================================================
%% %% Funnel Events Test
%% %% ===================================================================

%% funnel_client_offline_event_encode_test() ->
%% 	erlang:error(not_implemented).

%% funnel_client_offline_event_decode_test() ->
%% 	%% create test asn binary message
%% 	Asn = #'ConnectionDownEvent'{
%% 		connectionId = str_uuid(),
%% 		customerId = "system_id",
%% 		userId = "user",
%% 		type = transmitter,
%% 		connectedAt = utc_time(),
%% 		msgsReceived = 1,
%% 		msgsSent = 1,
%% 		errors = [],
%% 		reason = normal,
%% 		timestamp = utc_time()
%% 	},
%% 	{ok, List} = 'FunnelAsn':encode('ConnectionDownEvent', Asn),
%% 	Message = list_to_binary(List),

%% 	%% try to decode test message
%% 	{ok, DTO} = adto:decode(#funnel_client_offline_event_dto{}, Message),
%% 	#funnel_client_offline_event_dto{
%% 		connection_id = ConnectionID,
%% 		customer_id = CustomerID,
%% 		user_id = UserID
%% 	} = DTO,
%% 	true = adto_uuid:is_valid(ConnectionID),
%% 	true = is_binary(ConnectionID),
%% 	true = is_binary(CustomerID),
%% 	true = is_binary(UserID).

%% funnel_client_online_event_encode_test() ->
%% 	erlang:error(not_implemented).

%% funnel_client_online_event_decode_test() ->
%% 	%% create test message
%% 	Asn = #'ConnectionUpEvent'{
%% 		connectionId = str_uuid(),
%% 		customerId = "system_id",
%% 		userId = "user",
%% 		type = transmitter,
%% 		connectedAt = utc_time(),
%% 		timestamp = utc_time()
%% 	},
%% 	{ok, List} = 'FunnelAsn':encode('ConnectionUpEvent', Asn),
%% 	Message = list_to_binary(List),

%% 	%% try to debcode test message
%% 	{ok, DTO} = adto:decode(#funnel_client_online_event_dto{}, Message),
%% 	#funnel_client_online_event_dto{
%% 		connection_id = ConnectionID,
%% 		customer_id = CustomerID,
%% 		user_id = UserID,
%% 		type = Type
%% 	} = DTO,
%% 	true = adto_uuid:is_valid(ConnectionID),
%% 	true = is_binary(ConnectionID),
%% 	true = is_binary(CustomerID),
%% 	true = is_binary(UserID),
%% 	true = is_atom(Type).

%% %% ===================================================================
%% %% Funnel Incoming Sms Test
%% %% ===================================================================

%% funnel_incoming_sms_encode_test() ->
%% 	DTO = #funnel_incoming_sms_dto{
%% 		id = "id",
%% 		source = #addr_dto{addr= <<"addr">>, ton=0, npi=0},
%% 		dest = #addr_dto{addr= <<"addr">>, ton=0, npi=0},
%% 		message = "message",
%% 		datacoding = {text, 'gsm0338'}
%% 	},
%% 	{ok, Bin} = adto:encode(DTO),
%% 	true = is_binary(Bin),
%% 	erlang:error(error).


%% funnel_incoming_sms_decode_test() ->
%% 	erlang:error(not_implemented).

%% %% ===================================================================
%% %% Funnel Delivery Receipt Test
%% %% ===================================================================

%% funnel_delivery_receipt_encode_test() ->
%% 	erlang:error(error).

%% funnel_delivery_receipt_decode_test() ->
%% 	erlang:error(not_implemented).

%% %% ===================================================================
%% %% Funnel Ack Test
%% %% ===================================================================

%% funnel_ack_encode_test() ->
%% 	erlang:error(not_implemented).

%% funnel_ack_decode_test() ->
%% 	erlang:error(error).
