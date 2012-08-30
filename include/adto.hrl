-ifndef(adto_hrl).
-define(adto_hrl, included).

%% ===================================================================
%% Generic Entities
%% ===================================================================

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
		{error, binary()} | %% <<"error">>
		{customer, #funnel_auth_response_customer_dto{}}

}).

%% ===================================================================
%% Funnel Events Entities
%% ===================================================================

-record(funnel_started_event_dto, {
	timestamp :: utc_time_dto()
}).

-record(funnel_stopped_event_dto, {
	timestamp :: utc_time_dto()
}).

-record(funnel_client_online_event_dto, {
	connection_id :: string(),
	customer_id :: string(),
	user_id :: string(),
	type :: smpp_type_dto()
	%% connected_at :: utc_time_dto(),
	%% timestamp :: utc_time_dto()
}).

-record(error_dto, {
	error_code :: integer(),
	timestamp :: utc_time_dto()
}).
-type error_dto() :: #error_dto{}.

-record(funnel_client_offline_event_dto, {
	connection_id :: string(),
	customer_id :: string(),
	user_id :: string()
	%% type :: smpp_type_dto(),
	%% connected_at :: utc_time_dto(),
	%% msgs_received :: integer(),
	%% msgs_sent :: integer(),
	%% errors :: [error_dto()],
	%% reason :: normal | closed | unbound | other,
	%% timestamp :: utc_time_dto()
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

-record(funnel_incoming_sms_dto, {
	id :: string(),
	source :: addr_dto(),
	dest :: addr_dto(),
	message :: string(),
	datacoding :: any()
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

-record(funnel_delivey_receipt_dto, {
	id :: string(),
	message_id :: string(),
	submit_date :: utc_time_dto(),
	done_date :: utc_time_dto(),
	message_state :: message_state_dto(),
	source :: addr_dto(),
	dest :: addr_dto()
}).

%% ===================================================================
%% Funnel Ack Entity
%% ===================================================================

-record(funnel_ack_dto, {
	id :: string()
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
%% Misc
%% ===================================================================

-type message_type_dto() ::
	#funnel_auth_request_dto{} |
	#funnel_auth_response_dto{} |
	#funnel_started_event_dto{} |
 	#funnel_stopped_event_dto{} |
	#funnel_client_online_event_dto{} |
	#funnel_client_offline_event_dto{} |
	#just_sms_request_dto{} |
	#just_sms_response_dto{} |
	#just_incoming_sms_dto{} |
	#funnel_incoming_sms_dto{} |
	#just_delivery_receipt_dto{} |
	%% #funnel_delivery_receipt_dto{} |
	#funnel_ack_dto{}.

-endif. % adto_hrl


