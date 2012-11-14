-ifndef(adto_hrl).
-define(adto_hrl, included).

%% ===================================================================
%% Generic DTO
%% ===================================================================

-type client_type() 	:: k1api | funnel.

-record(addr_dto, {
	addr 				:: bitstring(),
	ton 				:: integer(),
	npi 				:: integer()
}).
-type addr_dto() 		:: #addr_dto{}.

-record(addr_ref_num_dto, {
	full_addr 			:: #addr_dto{},
	ref_num 			:: integer()
}).


-type billing_type() 	:: prepaid | postpaid.
-type bitstr_uuid()		:: bitstring(). %% <<"12fd794d-9e32-4cf6...
-type uuid() 			:: binary(). 	%% <<18,253,121,77,158,50...
-type smpp_type_dto() 	:: receiver | transciever | transmitter.

%% ===================================================================
%% Funnel
%% ===================================================================

-type fun_utc_time() 	:: bitstring(). %% <<"120827114232">>

-record(fun_precise_time_dto, {
	time 				:: fun_utc_time(),
	milliseconds 		:: integer()
}).

%% ===================================================================
%% Funnel Auth Request
%% ===================================================================

-record(funnel_auth_request_dto, {
	connection_id 		:: uuid(),
	ip 					:: bitstring(), %% <<"127.0.0.1">>
	customer_id 		:: bitstring(), %% <<"system-id">>
	user_id 			:: bitstring(), %% <<"user">>
	password 			:: bitstring(), %% <<"password">>
	type 				:: smpp_type_dto(),
	is_cached 			:: boolean(),
	timestamp 			:: #fun_precise_time_dto{},
	expiration 			:: #fun_precise_time_dto{}
}).

%% ===================================================================
%% Funnel Auth Response
%% ===================================================================

-record(network_dto, {
	id 					:: uuid(),
	country_code 		:: bitstring(), %% <<"375">>
	numbers_len 		:: integer(),
	prefixes 			:: [bitstring()], %% [<<"44">>, <<"33">>]
	provider_id 		:: uuid()
}).

-record(provider_dto, {
	id 					:: uuid(),
	gateway 			:: uuid(),
	bulk_gateway 		:: uuid(),
	receipts_supported 	:: boolean()
}).

-record(funnel_auth_response_customer_dto, {
	id 					:: bitstring(), %% <<"system-id">>
	uuid 				:: uuid(),
	priority 			:: integer(),
	rps 				:: integer() | undefined,
	allowed_sources 	:: [#addr_dto{}],
	default_source 		:: #addr_dto{} | undefined,
	networks 			:: [#network_dto{}],
	providers 			:: [#provider_dto{}],
	default_provider_id :: uuid() | undefined,
	receipts_allowed 	:: boolean(),
	no_retry 			:: boolean(),
	default_validity 	:: bitstring(), %% <<"000003000000000R">>
	max_validity 		:: integer(), 	%% in seconds (relative)
	billing_type 		:: billing_type()
}).

-type funnel_auth_response_result() ::
	{error, bitstring()} |
	{customer, #funnel_auth_response_customer_dto{}}.

-record(funnel_auth_response_dto, {
	connection_id 		:: uuid(),
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
	connection_id 	:: uuid(),
	customer_id 	:: bitstring(), %% <<"system_id">>
	user_id 		:: bitstring(), %% <<"user_id>>
	type 			:: smpp_type_dto(),
	connected_at 	:: fun_utc_time(),
	timestamp 		:: fun_utc_time()
}).

-record(error_dto, {
	error_code 		:: integer(),
	timestamp 		:: fun_utc_time()
}).

-record(funnel_client_offline_event_dto, {
	connection_id 	:: uuid(),
	customer_id 	:: bitstring(), %% <<"system_id">>
	user_id 		:: bitstring(), %% <<"user_id">>
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
	name 			:: bitstring(), % <<"registered_delivery">>
	value 			:: just_sms_request_param_value()
}).

-type jsms_req_params() :: [#just_sms_request_param_dto{}].

-record(just_sms_request_dto, {
	id 				:: bitstr_uuid(),
	gateway_id 		:: bitstr_uuid(),
	customer_id 	:: bitstr_uuid(),
	client_type 	:: client_type(),
	type 			:: regular | part,
	message 		:: binary(),
	encoding 		:: jsms_req_encoding(),
	params 			:: jsms_req_params(),
	source_addr, 	%% :: full_addr(),
	dest_addrs, 	%% :: jsms_req_dest_addrs(),
	message_ids 	:: [bitstring()]
}).

%% ===================================================================
%% Funnel Incoming Sms
%% ===================================================================

-type funnel_incoming_sms_datacoding() ::
	{text, gsm0338} |
	{text, ucs2} |
	{other, integer()}.

-record(funnel_incoming_sms_message_dto, {
	source 			:: addr_dto(),
	dest 			:: addr_dto(),
	message 		:: binary(),
	data_coding 	:: funnel_incoming_sms_datacoding()
}).

-record(funnel_incoming_sms_dto, {
	id 				:: uuid(),
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
	message_id 		:: bitstring(), %% <<"614">>
	submit_date 	:: integer(),  %% utc unix epoch
	done_date 		:: integer(), %% utc unix epoch
	message_state 	:: message_state_dto(),
	source 			:: addr_dto(),
	dest 			:: addr_dto()
}).

-record(funnel_delivery_receipt_dto, {
	id 				:: uuid(),
	receipts 		:: [#funnel_delivery_receipt_dto{}]
}).

%% ===================================================================
%% Funnel Ack
%% ===================================================================

-record(funnel_ack_dto, {
	id 				:: uuid()
}).

%% ===================================================================
%% Funnel Connections
%% ===================================================================

-record(funnel_connections_request_dto, {
}).

-record(funnel_connection_dto, {
	connection_id 	:: uuid(),
	remote_ip 		:: bitstring(), %% <<"127.0.0.1">>
	customer_id 	:: bitstring(), %% system (smpp) id
	user_id 		:: bitstring(),
	connected_at 	:: bitstring(),
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
	original_id 	:: bitstring(), %% <<"634">>
	dest_addr 		:: #addr_dto{},
	status 			:: success | failure,
	parts_total 	:: integer(),
	part_index 		:: integer() | undefined,
	message_id 		:: bitstring() | undefined, %% <<"614">>
	error_code 		:: integer() | undefined
}).

-record(just_sms_response_dto, {
	id 				:: uuid(),
	gateway_id 		:: uuid(),
	customer_id 	:: uuid(),
	client_type 	:: client_type(),
	statuses 		:: [#just_sms_status_dto{}],
	timestamp 		:: fun_utc_time()
}).

%% ===================================================================
%% Just Incoming Sms
%% ===================================================================

-record(just_incoming_sms_dto, {
	gateway_id 		:: uuid(),
	source 			:: #addr_dto{},
	dest 			:: #addr_dto{},
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
	message_id 		:: bitstring(), %% <<"614">>
	message_state 	:: just_receipt_message_state(),
	source 			:: #addr_dto{}
}).

-record(just_delivery_receipt_dto, {
	gateway_id 		:: uuid(),
	receipts 		:: [#just_receipt_dto{}],
	timestamp 		:: integer() %% 1346067785681000
}).

%% ===================================================================
%% k1api Sms Delivery Status Request
%% ===================================================================

-record(k1api_sms_delivery_status_request_dto, {
	id 				:: uuid(),
	customer_id 	:: uuid(),
	user_id 		:: bitstring(),
	sms_request_id 	:: uuid(),
	address 		:: #addr_dto{}
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
	address 	:: #addr_dto{},
	status 		:: k1api_sms_status()
}).

-record(k1api_sms_delivery_status_response_dto, {
	id 			:: uuid(),
	statuses 	:: [#k1api_sms_status_dto{}]
}).

%% ===================================================================
%% k1api Retrieve Sms Request
%% ===================================================================

-record(k1api_retrieve_sms_request_dto, {
	id 			:: uuid(),
	customer_id :: uuid(),
	user_id 	:: bitstring(), %% <<"user_id">>
	dest_addr 	:: #addr_dto{},
	batch_size 	:: undefined | integer()
}).

%% ===================================================================
%% k1api Retrieve Sms Response
%% ===================================================================

-record(k1api_retrieved_sms_dto, {
	datetime 	:: integer(), %% unix epoch seconds
	sender_addr :: #addr_dto{},
	message_id 	:: bitstring(), %% <<"123">>
	message 	:: bitstring() %% <<"message">>
}).

-record(k1api_retrieve_sms_response_dto, {
	id 			:: uuid(),
	messages 	:: [#k1api_retrieved_sms_dto{}],
	total 		:: integer()
}).

%% ===================================================================
%% k1api Remove Retrieved Sms Request
%% ===================================================================

-record(k1api_remove_retrieved_sms_request_dto, {
	id 			:: uuid(),
	message_ids :: [bitstring()] %% [<<"123">>]
}).

%% ===================================================================
%% k1api Subscribe Incoming Sms
%% ===================================================================

-record(k1api_subscribe_incoming_sms_request_dto, {
	id 					:: uuid(),
	customer_id 		:: uuid(),
	user_id 			:: bitstring(),
	dest_addr 			:: #addr_dto{},
	notify_url 			:: bitstring(),
	criteria 			:: undefined | bitstring(),
	notification_format :: undefined | bitstring(), %% <<"json">>
	correlator 			:: undefined | bitstring(),
	callback_data 		:: undefined | bitstring()
}).

-record(k1api_subscribe_incoming_sms_response_dto, {
	id 					:: uuid(),
	subscription_id 	:: uuid()
}).

%% ===================================================================
%% k1api Unsubscribe Incoming Sms
%% ===================================================================

-record(k1api_unsubscribe_incoming_sms_request_dto, {
	id 					:: uuid(),
	customer_id 		:: uuid(),
	user_id 			:: bitstring(),
	subscription_id 	:: uuid()
}).

-record(k1api_unsubscribe_incoming_sms_response_dto, {
	id 					:: uuid()
}).

%% ===================================================================
%% k1api Incoming Sms Notification
%% ===================================================================

-record(k1api_sms_notification_request_dto, {
	callback_data 		:: bitstring(),
	datetime 			:: integer(), %% unix epoch seconds
	dest_addr 			:: #addr_dto{},
	message_id 			:: bitstring(),
	message 			:: bitstring(),
	sender_addr 		:: #addr_dto{},
	notify_url 			:: bitstring()
}).

%% ===================================================================
%% k1api Auth
%% ===================================================================

-record(k1api_auth_request_dto, {
	id 					:: uuid(),
	customer_id 		:: bitstring(), %% <<"system_id">>
	user_id 			:: bitstring(),
	password 			:: bitstring()
}).

-record(k1api_auth_response_dto, {
	id 					:: uuid(),
	system_id 			:: bitstring(), %% <<"system-id">>
	uuid 				:: uuid(),
	billing_type 		:: billing_type(),
	allowed_sources 	:: [#addr_dto{}],
	default_source 		:: #addr_dto{} | undefined,
	networks 			:: [#network_dto{}],
	providers 			:: [#provider_dto{}],
	default_provider_id :: uuid() | undefined,
	receipts_allowed 	:: boolean(),
	no_retry 			:: boolean(),
	default_validity 	:: integer(), %% seconds
	max_validity 		:: integer() %% seconds
}).

%% ===================================================================
%% k1api Sms Receipts Subscriptions
%% ===================================================================

-record(k1api_subscribe_sms_receipts_request_dto, {
	id 				:: uuid(),
	customer_id 	:: uuid(),
	user_id 		:: bitstring(), %% <<"user">>
	url 			:: bitstring(),
	dest_addr 		:: #addr_dto{},
	callback_data 	:: bitstring() %% <<"callback">>
}).

-record(k1api_subscribe_sms_receipts_response_dto, {
	id 				:: uuid()
}).

-record(k1api_unsubscribe_sms_receipts_request_dto, {
	id 				:: uuid(),
	customer_id 	:: uuid(),
	user_id 		:: bitstring(),
	subscription_id :: uuid()
}).

-record(k1api_unsubscribe_sms_receipts_response_dto, {
	id 				:: uuid()
}).

-record(k1api_sms_delivery_receipt_notification_dto, {
	id 				:: uuid(),
	dest_addr 		:: #addr_dto{},
	status 			:: k1api_sms_status(),
	callback_data 	:: bitstring(),
	url 			:: bitstring()
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



