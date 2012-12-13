-ifndef(adto_hrl).
-define(adto_hrl, included).

-include("addr.hrl").

%% ===================================================================
%% Generic DTO
%% ===================================================================

-type client_type() 	:: k1api | funnel.
-type billing_type_() 	:: prepaid | postpaid.
-type uuid_() 			:: binary(). 	%% <<"12fd794d-9e32-4cf6...
-type smpp_type_dto() 	:: receiver | transciever | transmitter.

%% ===================================================================
%% Funnel
%% ===================================================================

-type fun_utc_time() 	:: binary(). %% <<"120827114232">>

-record(fun_precise_time_dto, {
	time 				:: fun_utc_time(),
	milliseconds 		:: integer()
}).

%% ===================================================================
%% Funnel Auth Request
%% ===================================================================

-record(funnel_auth_request_dto, {
	connection_id 		:: uuid_(),
	ip 					:: binary(), %% <<"127.0.0.1">>
	customer_id 		:: binary(), %% <<"system-id">>
	user_id 			:: binary(), %% <<"user">>
	password 			:: binary(), %% <<"password">>
	type 				:: smpp_type_dto(),
	is_cached 			:: boolean(),
	timestamp 			:: #fun_precise_time_dto{},
	expiration 			:: #fun_precise_time_dto{}
}).

%% ===================================================================
%% Funnel Auth Response
%% ===================================================================

-record(network_dto, {
	id 					:: uuid_(),
	country_code 		:: binary(), %% <<"375">>
	numbers_len 		:: integer(),
	prefixes 			:: [binary()], %% [<<"44">>, <<"33">>]
	provider_id 		:: uuid_()
}).

-record(provider_dto, {
	id 					:: uuid_(),
	gateway 			:: uuid_(),
	bulk_gateway 		:: uuid_(),
	receipts_supported 	:: boolean()
}).

-record(funnel_auth_response_customer_dto, {
	id 					:: binary(), %% <<"system-id">>
	uuid 				:: uuid_(),
	priority 			:: integer(),
	rps 				:: integer() | undefined,
	allowed_sources 	:: [addr()],
	default_source 		:: addr() | undefined,
	networks 			:: [#network_dto{}],
	providers 			:: [#provider_dto{}],
	default_provider_id :: uuid_() | undefined,
	receipts_allowed 	:: boolean(),
	no_retry 			:: boolean(),
	default_validity 	:: binary(), %% <<"000003000000000R">>
	max_validity 		:: integer(), 	%% in seconds (relative)
	billing_type 		:: billing_type_()
}).

-type funnel_auth_response_result() ::
	{error, binary()} |
	{customer, #funnel_auth_response_customer_dto{}}.

-record(funnel_auth_response_dto, {
	connection_id 		:: uuid_(),
	result 				:: funnel_auth_response_result()
}).

%% ===================================================================
%% Funnel Events
%% ===================================================================

-record(funnel_started_event_dto, {
	timestamp :: fun_utc_time()
}).

-record(funnel_stopped_event_dto, {
	timestamp :: fun_utc_time()
}).

-record(funnel_client_online_event_dto, {
	connection_id 	:: uuid_(),
	customer_id 	:: binary(), %% <<"system_id">>
	user_id 		:: binary(), %% <<"user_id>>
	type 			:: smpp_type_dto(),
	connected_at 	:: fun_utc_time(),
	timestamp 		:: fun_utc_time()
}).

-record(error_dto, {
	error_code 		:: integer(),
	timestamp 		:: fun_utc_time()
}).

-record(funnel_client_offline_event_dto, {
	connection_id 	:: uuid_(),
	customer_id 	:: binary(), %% <<"system_id">>
	user_id 		:: binary(), %% <<"user_id">>
	type 			:: smpp_type_dto(),
	connected_at 	:: fun_utc_time(),
	msgs_received 	:: integer(),
	msgs_sent 		:: integer(),
	errors 			:: [#error_dto{}],
	reason 			:: normal | closed | unbound | other,
	timestamp 		:: fun_utc_time()
}).

%% ===================================================================
%% Just Sms Request & Response Entities
%% ===================================================================

-type jsms_req_encoding() ::
	{text, default} |
	{text, gsm0338} |
	{text, ascii} |
	{text, latin1} |
	{text, ucs2} |
	{other, integer()}.

-type just_sms_request_param_value() ::
	{integer, integer()} |
	{string, binary()} |
	{boolean, boolean()}.

-record(just_sms_request_param_dto, {
	name 			:: binary(), % <<"registered_delivery">>
	value 			:: just_sms_request_param_value()
}).

-type jsms_req_params() :: [#just_sms_request_param_dto{}].

-record(just_sms_request_dto, {
	id 				:: uuid_(),
	gateway_id 		:: uuid_(),
	customer_id 	:: uuid_(),
	client_type 	:: client_type(),
	type 			:: regular | part,
	message 		:: binary(),
	encoding 		:: jsms_req_encoding(),
	params 			:: jsms_req_params(),
	source_addr 	:: addr(),
	dest_addrs	 	:: {regular, addr()} | {part, addr()},
	message_ids 	:: [binary()]
}).

%% ===================================================================
%% Funnel Incoming Sms
%% ===================================================================

-type funnel_incoming_sms_datacoding() ::
	{text, gsm0338} |
	{text, ucs2} |
	{other, integer()}.

-record(funnel_incoming_sms_message_dto, {
	source 			:: addr(),
	dest 			:: addr(),
	message 		:: binary(),
	data_coding 	:: funnel_incoming_sms_datacoding()
}).

-record(funnel_incoming_sms_dto, {
	id 				:: uuid_(),
	messages 		:: [#funnel_incoming_sms_message_dto{}]
}).

%% ===================================================================
%% Funnel Delivery Receipt
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
	message_id 		:: binary(), %% <<"614">>
	submit_date 	:: integer(),  %% utc unix epoch
	done_date 		:: integer(), %% utc unix epoch
	message_state 	:: message_state_dto(),
	source 			:: addr(),
	dest 			:: addr()
}).

-record(funnel_delivery_receipt_dto, {
	id 				:: uuid_(),
	receipts 		:: [#funnel_delivery_receipt_dto{}]
}).

%% ===================================================================
%% Funnel Ack
%% ===================================================================

-record(funnel_ack_dto, {
	id 				:: uuid_()
}).

%% ===================================================================
%% Funnel Connections
%% ===================================================================

-record(funnel_connections_request_dto, {
}).

-record(funnel_connection_dto, {
	connection_id 	:: uuid_(),
	remote_ip 		:: binary(), %% <<"127.0.0.1">>
	customer_id 	:: binary(), %% system (smpp) id
	user_id 		:: binary(),
	connected_at 	:: binary(),
	type 			:: smpp_type_dto(),
	msgs_received 	:: integer(),
	msgs_sent 		:: integer(),
	errors 			:: [#error_dto{}]
}).

-record(funnel_connections_response_dto, {
	connections = [#funnel_connection_dto{}]
}).

%% ===================================================================
%% Just Sms Response
%% ===================================================================

-record(just_sms_status_dto, {
	original_id 	:: binary(), %% <<"634">>
	dest_addr 		:: addr(),
	status 			:: success | failure,
	parts_total 	:: integer(),
	part_index 		:: integer() | undefined,
	message_id 		:: binary() | undefined, %% <<"614">>
	error_code 		:: integer() | undefined
}).

-record(just_sms_response_dto, {
	id 				:: uuid_(),
	gateway_id 		:: uuid_(),
	customer_id 	:: uuid_(),
	client_type 	:: client_type(),
	statuses 		:: [#just_sms_status_dto{}],
	timestamp 		:: fun_utc_time()
}).

%% ===================================================================
%% Just Incoming Sms
%% ===================================================================

-record(just_incoming_sms_dto, {
	gateway_id 		:: uuid_(),
	source 			:: addr(),
	dest 			:: addr(),
	message 		:: binary(),
	data_coding 	:: integer(),
	parts_ref_num 	:: integer() | undefined,
	parts_count 	:: integer() | undefined,
	part_index 		:: integer() | undefined,
	timestamp 		:: fun_utc_time()
}).

%% ===================================================================
%% Just Delivery Receipt
%% ===================================================================

-type just_receipt_message_state() ::
	enroute |
	delivered |
	deleted |
	undeliverable |
	accepted |
	unknown |
	rejected |
	unrecognized.

-record(just_receipt_dto, {
	message_id 		:: binary(), %% <<"614">>
	message_state 	:: just_receipt_message_state(),
	source 			:: addr()
}).

-record(just_delivery_receipt_dto, {
	gateway_id 		:: uuid_(),
	receipts 		:: [#just_receipt_dto{}],
	timestamp 		:: integer() %% 1346067785681000
}).

%% ===================================================================
%% k1api Sms Delivery Status Request
%% ===================================================================

-record(k1api_sms_delivery_status_request_dto, {
	id 				:: uuid_(),
	customer_id 	:: uuid_(),
	user_id 		:: binary(),
	sms_request_id 	:: uuid_(),
	address 		:: addr()
}).

%% ===================================================================
%% k1api Sms Delivery Status Response
%% ===================================================================

-type k1api_sms_status() ::
	submitted |
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
	address 	:: addr(),
	status 		:: k1api_sms_status()
}).

-record(k1api_sms_delivery_status_response_dto, {
	id 			:: uuid_(),
	statuses 	:: [#k1api_sms_status_dto{}]
}).

%% ===================================================================
%% k1api Retrieve Sms Request
%% ===================================================================

-record(k1api_retrieve_sms_request_dto, {
	id 			:: uuid_(),
	customer_id :: uuid_(),
	user_id 	:: binary(), %% <<"user_id">>
	dest_addr 	:: addr(),
	batch_size 	:: undefined | integer()
}).

%% ===================================================================
%% k1api Retrieve Sms Response
%% ===================================================================

-record(k1api_retrieved_sms_dto, {
	datetime 	:: integer(), %% unix epoch seconds
	sender_addr :: addr(),
	message_id 	:: binary(), %% <<"123">>
	message 	:: binary() %% <<"message">>
}).

-record(k1api_retrieve_sms_response_dto, {
	id 			:: uuid_(),
	messages 	:: [#k1api_retrieved_sms_dto{}],
	total 		:: integer()
}).

%% ===================================================================
%% k1api Remove Retrieved Sms Request
%% ===================================================================

-record(k1api_remove_retrieved_sms_request_dto, {
	id 			:: uuid_(),
	message_ids :: [binary()] %% [<<"123">>]
}).

%% ===================================================================
%% k1api Subscribe Incoming Sms
%% ===================================================================

-record(k1api_subscribe_incoming_sms_request_dto, {
	id 					:: uuid_(),
	customer_id 		:: uuid_(),
	user_id 			:: binary(),
	dest_addr 			:: addr(),
	notify_url 			:: binary(),
	criteria 			:: undefined | binary(),
	notification_format :: undefined | binary(), %% <<"json">>
	correlator 			:: undefined | binary(),
	callback_data 		:: undefined | binary()
}).

-record(k1api_subscribe_incoming_sms_response_dto, {
	id 					:: uuid_(),
	subscription_id 	:: uuid_()
}).

%% ===================================================================
%% k1api Unsubscribe Incoming Sms
%% ===================================================================

-record(k1api_unsubscribe_incoming_sms_request_dto, {
	id 					:: uuid_(),
	customer_id 		:: uuid_(),
	user_id 			:: binary(),
	subscription_id 	:: uuid_()
}).

-record(k1api_unsubscribe_incoming_sms_response_dto, {
	id 					:: uuid_()
}).

%% ===================================================================
%% k1api Incoming Sms Notification
%% ===================================================================

-record(k1api_sms_notification_request_dto, {
	callback_data 		:: binary(),
	datetime 			:: integer(), %% unix epoch seconds
	dest_addr 			:: addr(),
	message_id 			:: binary(),
	message 			:: binary(),
	sender_addr 		:: addr(),
	notify_url 			:: binary()
}).

%% ===================================================================
%% k1api Auth
%% ===================================================================

-record(k1api_auth_request_dto, {
	id 					:: uuid_(),
	customer_id 		:: binary(), %% <<"system_id">>
	user_id 			:: binary(),
	password 			:: binary()
}).

-record(k1api_auth_response_dto, {
	id 					:: uuid_(),
	system_id 			:: binary(), %% <<"system-id">>
	uuid 				:: uuid_(),
	billing_type 		:: billing_type_(),
	allowed_sources 	:: [addr()],
	default_source 		:: addr() | undefined,
	networks 			:: [#network_dto{}],
	providers 			:: [#provider_dto{}],
	default_provider_id :: uuid_() | undefined,
	receipts_allowed 	:: boolean(),
	no_retry 			:: boolean(),
	default_validity 	:: integer(), %% seconds
	max_validity 		:: integer() %% seconds
}).

%% ===================================================================
%% k1api Sms Receipts Subscriptions
%% ===================================================================

-record(k1api_subscribe_sms_receipts_request_dto, {
	id 				:: uuid_(),
	customer_id 	:: uuid_(),
	user_id 		:: binary(), %% <<"user">>
	url 			:: binary(),
	dest_addr 		:: addr(),
	callback_data 	:: binary() %% <<"callback">>
}).

-record(k1api_subscribe_sms_receipts_response_dto, {
	id 				:: uuid_()
}).

-record(k1api_unsubscribe_sms_receipts_request_dto, {
	id 				:: uuid_(),
	customer_id 	:: uuid_(),
	user_id 		:: binary(),
	subscription_id :: uuid_()
}).

-record(k1api_unsubscribe_sms_receipts_response_dto, {
	id 				:: uuid_()
}).

-record(k1api_sms_delivery_receipt_notification_dto, {
	id 				:: uuid_(),
	dest_addr 		:: addr(),
	status 			:: k1api_sms_status(),
	callback_data 	:: binary(),
	url 			:: binary()
}).

%% ===================================================================
%% Misc
%% ===================================================================

-type message_type_dto() ::
%% funnel dto
	#funnel_auth_request_dto{} |
	#funnel_auth_response_dto{} |
	#funnel_started_event_dto{} |
 	#funnel_stopped_event_dto{} |
	#funnel_client_online_event_dto{} |
	#funnel_client_offline_event_dto{} |

	#funnel_incoming_sms_dto{} |
	#funnel_delivery_receipt_dto{} |
	#funnel_ack_dto{} |

%% just dto
	#just_sms_request_dto{} |
	#just_sms_response_dto{} |
	#just_incoming_sms_dto{} |
	#just_delivery_receipt_dto{} |

%% k1api dto
	%% k1api auth
	#k1api_auth_request_dto{} |
	#k1api_auth_response_dto{} |

	%% k1api sms delivery status
	#k1api_sms_delivery_status_request_dto{} |
	#k1api_sms_delivery_status_response_dto{} |

	%% k1api retrieve sms
	#k1api_retrieve_sms_request_dto{} |
	#k1api_retrieve_sms_response_dto{} |
	#k1api_remove_retrieved_sms_request_dto{} |

	%% k1api incoming sms subscription
	#k1api_subscribe_incoming_sms_request_dto{} |
	#k1api_subscribe_incoming_sms_response_dto{} |
	#k1api_unsubscribe_incoming_sms_request_dto{} |
	#k1api_unsubscribe_incoming_sms_response_dto{} |
	#k1api_sms_notification_request_dto{} |

	%% k1api delivery receipt subscription
	#k1api_subscribe_sms_receipts_request_dto{} |
	#k1api_subscribe_sms_receipts_response_dto{} |
	#k1api_unsubscribe_sms_receipts_request_dto{} |
	#k1api_unsubscribe_sms_receipts_response_dto{} |
	#k1api_sms_delivery_receipt_notification_dto{}.

-endif. % adto_hrl
