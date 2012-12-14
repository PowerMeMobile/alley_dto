-ifndef(just_dto_hrl).
-define(just_dto_hrl, included).

-include("adto_types.hrl").

%% ===================================================================
%% Just Sms Request & Response Entities
%% ===================================================================

-type jsms_req_encoding() ::
	default |
	gsm0338 |
	ascii 	|
	latin1 	|
	ucs2 	|
	integer().

-type just_sms_request_param_value() ::
	{integer, integer()} 	|
	{string, binary()} 		|
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
	timestamp 		:: utc_time()
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
	timestamp 		:: utc_time()
}).

%% ===================================================================
%% Just Delivery Receipt
%% ===================================================================

-type just_receipt_message_state() ::
	enroute 		|
	delivered 		|
	deleted 		|
	undeliverable 	|
	accepted 		|
	unknown 		|
	rejected 		|
	unrecognized.

-record(just_receipt_dto, {
	message_id 		:: binary(), %% <<"614">>
	message_state 	:: just_receipt_message_state(),
	source 			:: addr()
}).

-record(just_delivery_receipt_dto, {
	gateway_id 		:: uuid_(),
	receipts 		:: [#just_receipt_dto{}],
	timestamp 		:: integer() % % 1346067785681000
}).

-type just_dto() ::
	#just_sms_request_dto{} 	|
	#just_sms_response_dto{} 	|
	#just_incoming_sms_dto{} 	|
	#just_delivery_receipt_dto{}.

-endif. % just_dto_hrl
