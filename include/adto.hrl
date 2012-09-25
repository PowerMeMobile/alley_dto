-ifndef(adto_hrl).
-define(adto_hrl, included).

%% ===================================================================
%% Generic Entities
%% ===================================================================

-type client_type() :: k1api | funnel.

-record(addr_dto, {
	addr :: string(),
	ton :: integer(),
	npi :: integer()
}).
-type addr_dto() :: #addr_dto{}.

-record(addr_ref_num_dto, {
	full_addr :: #addr_dto{},
	ref_num :: integer()
}).


-record(precise_time_dto, {
	time :: binary(), %% <<"120827114232">>
	milliseconds :: integer()
}).

-type bin_uuid() :: binary(). %% <<"12fd794d-9e32-4cf6-b421-b797196b60e3">>
-type utc_time_dto() :: string().
-type smpp_type_dto() :: receiver | transciever | transmitter.

%% ===================================================================
%% Funnel Auth Request
%% ===================================================================

-record(funnel_auth_request_dto, {
	connection_id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	ip :: binary(), %% <<"127.0.0.1">>
	customer_id :: binary(), %% <<"system-id">>
	user_id :: binary(), %% <<"user">>
	password :: binary(), %% <<"password">>
	type :: smpp_type_dto(), %% atom()
	is_cached :: boolean(),
	timestamp :: #precise_time_dto{},
	expiration :: #precise_time_dto{}
}).

%% ===================================================================
%% Funnel Auth Response
%% ===================================================================

-record(network_dto, {
	id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	country_code :: binary(), %% <<"375">>
	numbers_len :: integer(), %%
	prefixes :: [binary()], %% [<<"44">>, <<"33">>]
	provider_id :: binary() %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
}).

-record(provider_dto, {
	id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	gateway :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	bulk_gateway :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	receipts_supported :: boolean()
}).

-record(funnel_auth_response_customer_dto, {
	id :: binary(), %% <<"system-id">>
	uuid :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	priority :: integer(),
	rps :: integer() | undefined,
	allowed_sources :: [#addr_dto{}],
	default_source :: #addr_dto{} | undefined,
	networks :: [#network_dto{}],
	providers :: [#provider_dto{}],
	default_provider_id :: binary() | undefined, %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	receipts_allowed :: boolean(),
	no_retry :: boolean(),
	default_validity :: binary(), %% <<"000003000000000R">>
	max_validity :: integer()
}).

-record(funnel_auth_response_dto, {
	connection_id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	result ::
		{error, string()} | %% "error"
		{customer, #funnel_auth_response_customer_dto{}}

}).

%% ===================================================================
%% Funnel Events Entities
%% ===================================================================

-record(funnel_started_event_dto, {
	timestamp :: binary() %% <<"120827114232">>
}).

-record(funnel_stopped_event_dto, {
	timestamp :: binary() %% <<"120827114232">>
}).

-record(funnel_client_online_event_dto, {
	connection_id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	customer_id :: binary(), %% <<"system_id">>
	user_id :: binary(), %% <<"user_id>>
	type :: smpp_type_dto(),
	connected_at :: binary(), %% <<"120827114232">>
	timestamp :: binary() %% <<"120827114232">>
}).

-record(error_dto, {
	error_code :: integer(),
	timestamp :: binary() %% <<"120827114232">>
}).

-record(funnel_client_offline_event_dto, {
	connection_id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	customer_id :: binary(), %% <<"system_id">>
	user_id :: binary(), %% <<"user_id">>
	type :: smpp_type_dto(),
	connected_at :: binary(), %% <<"120827114232">>
	msgs_received :: integer(),
	msgs_sent :: integer(),
	errors :: [#error_dto{}],
	reason :: normal | closed | unbound | other,
	timestamp :: binary() %% <<"120827114232">>
}).

%% ===================================================================
%% Sms Request & Response Entities
%% ===================================================================

-type jsms_req_encoding() ::
	{text, default} |
	{text, gsm0338} |
	{text, ascii} |
	{text, latin1} |
	{text, ucs2} |
	{other, integer()}.
-record(just_sms_request_param_dto, {
	name :: binary(), % <<"registered_delivery">>
	value ::
		{integer, integer()} |
		{string, binary()} |
		{boolean, boolean()}
}).
-type jsms_req_params() :: [#just_sms_request_param_dto{}].

-record(just_sms_request_dto, {
	id :: bin_uuid(),
	gateway_id :: bin_uuid(),
	customer_id :: bin_uuid(),
	client_type :: client_type(),
	type :: regular | part,
	message :: binary(),
	encoding :: jsms_req_encoding(),
	params :: jsms_req_params(),
	source_addr, %% :: full_addr(),
	dest_addrs, %% :: jsms_req_dest_addrs(),
	message_ids :: [string()]
}).

%% ===================================================================
%% Incoming Sms Entities
%% ===================================================================

-record(funnel_incoming_sms_message_dto, {
	source :: addr_dto(),
	dest :: addr_dto(),
	message :: binary(),
	data_coding ::
		{text, gsm0338} |
		{text, ucs2} |
		{other, integer()}
}).

-record(funnel_incoming_sms_dto, {
	id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	messages :: [#funnel_incoming_sms_message_dto{}]
}).

%% ===================================================================
%% Delivery Receipt Entities
%% ===================================================================

-type message_state_dto() ::
	delivered |
	expired |
	deleted |
	undeliverable |
	accepted |
	unknown |
	rejected.

-record(funnel_delivery_receipt_container_dto, {
	message_id :: binary(), %% <<"614">>
	submit_date :: binary(),  %% <<"20827114232">>
	done_date :: binary(), %% <<"20827114232">>
	message_state :: message_state_dto(),
	source :: addr_dto(),
	dest :: addr_dto()
}).

-record(funnel_delivery_receipt_dto, {
	id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	receipts :: [#funnel_delivery_receipt_dto{}]
}).

%% ===================================================================
%% Funnel Ack Entity
%% ===================================================================

-record(funnel_ack_dto, {
	id :: binary() %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
}).

%% ===================================================================
%% Just Sms Response
%% ===================================================================

-record(just_sms_status_dto, {
	original_id :: binary(), %% <<"634">>
	dest_addr :: #addr_dto{},
	status :: success | failure,
	parts_total :: integer(),
	part_index :: integer() | undefined,
	message_id :: binary() | undefined, %% <<"614">>
	error_code :: integer() | undefined
}).

-record(just_sms_response_dto, {
	id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	gateway_id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	customer_id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	client_type :: client_type(),
	statuses :: [] | [#just_sms_status_dto{}],
	timestamp :: binary() %%  <<"120827114305">>
}).

%% ===================================================================
%% Just Incoming Sms
%% ===================================================================

-record(just_incoming_sms_dto, {
	gateway_id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	source :: #addr_dto{},
	dest :: #addr_dto{},
	message :: binary(),
	data_coding :: integer(),
	parts_ref_num :: integer() | undefined,
	parts_count :: integer() | undefined,
	part_index :: integer() | undefined,
	timestamp :: binary() %% <<"120827114305">>
}).

%% ===================================================================
%% Just Delivery Receipt
%% ===================================================================

-record(just_receipt_dto, {
	message_id :: binary(), %% <<"614">>
	message_state ::
		enroute |
		delivered |
		deleted |
		undeliverable |
		accepted |
		unknown |
		rejected |
		unrecognized,
	source :: #addr_dto{}
}).

-record(just_delivery_receipt_dto, {
	gateway_id :: binary(), %% <<18,253,121,77,158,50,76,246,180,33,183,151,25,107,96,227>>
	receipts :: [#just_receipt_dto{}],
	timestamp :: integer() %% 1346067785681000
}).

%% ===================================================================
%% k1api Sms Delivery Status Request
%% ===================================================================

-record(k1api_sms_delivery_status_request_dto, {
	id :: binary(), %% uuid
	customer_id :: binary(), %% uuid
	user_id :: bitstring(),
	sms_request_id :: binary(), %% uuid
	address :: #addr_dto{}
}).

%% ===================================================================
%% k1api Sms Delivery Status Response
%% ===================================================================

-type k1api_sms_status() :: submitted |
							success_waiting_delivery |
							success_no_delivery |
							failure |
							enroute |
							delivered |
							expired |
							deleted |
							undeliverable |
							accepted |
							unknown |
							rejected |
							unrecognized.

-record(k1api_sms_status_dto, {
	address :: #addr_dto{},
	status :: k1api_sms_status()
}).

-record(k1api_sms_delivery_status_response_dto, {
	id :: binary(), %% uuid
	statuses :: [#k1api_sms_status_dto{}]
}).

%% ===================================================================
%% k1api Retrieve Sms Request
%% ===================================================================

-record(k1api_retrieve_sms_request_dto, {
	id :: binary(), %% uuid <<12,34..
	customer_id :: binary(), %% uuid <<12,34...
	user_id :: bitstring(), %% <<"user_id">>
	dest_addr :: #addr_dto{},
	batch_size :: undefined | integer()
}).

%% ===================================================================
%% k1api Retrieve Sms Response
%% ===================================================================

-record(k1api_retrieved_sms_dto, {
	datetime :: bitstring(), %% ???
	sender_addr :: #addr_dto{},
	message_id :: bitstring(), %% <<"123">>
	message :: bitstring() %% <<"message">>
}).

-record(k1api_retrieve_sms_response_dto, {
	id :: binary(), %% uuid <<12,34...
	messages :: [#k1api_retrieved_sms_dto{}],
	total :: integer()
}).

%% ===================================================================
%% k1api Remove Retrieved Sms Request
%% ===================================================================

-record(k1api_remove_retrieved_sms_request_dto, {
	id :: binary(), %% uuid <<12,34..
	message_ids :: [bitstring()] %% [<<"123">>]
}).

%% ===================================================================
%% Misc
%% ===================================================================

-type message_type_dto() ::
	#funnel_auth_request_dto{} |
	#funnel_auth_response_dto{} |
	#funnel_started_event_dto{} |
 	#funnel_stopped_event_dto{} |
	#funnel_client_online_event_dto{} |
	#funnel_client_offline_event_dto{} |

	#funnel_incoming_sms_dto{} |
	#funnel_delivery_receipt_dto{} |
	#funnel_ack_dto{} |

	#just_sms_request_dto{} |
	#just_sms_response_dto{} |
	#just_incoming_sms_dto{} |
	#just_delivery_receipt_dto{} |

	#k1api_sms_delivery_status_request_dto{} |
	#k1api_sms_delivery_status_response_dto{} |
	#k1api_retrieve_sms_request_dto{} |
	#k1api_retrieve_sms_response_dto{} |
	#k1api_remove_retrieved_sms_request_dto{}.

-endif. % adto_hrl


