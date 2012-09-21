-module(adto_k1api).

-export([
	encode/1,
	decode/2
]).

-include("adto.hrl").
-include("k1api_pb.hrl").

%% ===================================================================
%% Decode Functions
%% ===================================================================

-spec decode(message_type_dto(), binary()) ->
	{ok, message_type_dto()} |
	{error, Reason :: any()}.
decode(#k1api_sms_delivery_status_request_dto{}, Bin) ->
	PB = k1api_pb:decode_oneapismsdeliverystatusreq(Bin),
	#oneapismsdeliverystatusreq{
		id = ID,
		customer_id = CustomerID,
		user_id = UserID,
		sms_request_id = RequestID,
		address = Addr
	} = PB,
	DTO = #k1api_sms_delivery_status_request_dto{
		id = ID,
		customer_id = CustomerID,
		user_id = UserID,
		sms_request_id = RequestID,
		address = addr_pb_to_dto(Addr)
	},
	{ok, DTO};

decode(#k1api_sms_delivery_status_response_dto{}, Bin) ->
	PB = k1api_pb:decode_oneapismsdeliverystatusresp(Bin),
	#oneapismsdeliverystatusresp{
		id = ID,
		statuses = Statuses
	} = PB,
	DTO = #k1api_sms_delivery_status_response_dto{
		id = ID,
		statuses = sms_statuses_pb_to_dto(Statuses)
	},
	{ok, DTO};

decode(Type, _Message) ->
	erlang:error({k1api_decode_not_supported, Type}).

%% ===================================================================
%% Encode Functions
%% ===================================================================

-spec encode(message_type_dto()) ->
	{ok, Payload :: binary()} |
	{error, Reason :: any()}.

encode(DTO = #k1api_sms_delivery_status_request_dto{}) ->
	#k1api_sms_delivery_status_request_dto{
		id = ID,
		customer_id = CustomerID,
		user_id = UserID,
		sms_request_id = RequestID,
		address = Addr
	} = DTO,
	PB = #oneapismsdeliverystatusreq{
		id = ID,
		customer_id = CustomerID,
		user_id = UserID,
		sms_request_id = RequestID,
		address = addr_dto_to_pb(Addr)
	},
	Bin = k1api_pb:encode_oneapismsdeliverystatusreq(PB),
	{ok, Bin};

encode(DTO = #k1api_sms_delivery_status_response_dto{}) ->
	#k1api_sms_delivery_status_response_dto{
		id = ID,
		statuses = Statuses
	} = DTO,
	PB = #oneapismsdeliverystatusresp{
		id = ID,
		statuses = sms_statuses_dto_to_pb(Statuses)
	},
	Bin = k1api_pb:encode_oneapismsdeliverystatusresp(PB),
	{ok, Bin};

encode(Message) ->
	erlang:error({k1api_encode_not_supported, Message}).


%% ===================================================================
%% Internal
%% ===================================================================

addr_dto_to_pb(AddrDTO) ->
	#addr_dto{
		addr = Addr,
		ton = TON,
		npi = NPI
	} = AddrDTO,
	#k1apiaddr{
		addr = Addr,
		ton = TON,
		npi = NPI
	}.

addr_pb_to_dto(AddrPB) ->
	#k1apiaddr{
		addr = Addr,
		ton = TON,
		npi = NPI
	} = AddrPB,
	#addr_dto{
		addr = Addr,
		ton = TON,
		npi = NPI
	}.

sms_statuses_dto_to_pb(StatusDTO = #k1api_sms_status_dto{}) ->
	#k1api_sms_status_dto{
		address = Addr,
		status = Status
	} = StatusDTO,
	#smsstatus{
		address = addr_dto_to_pb(Addr),
		status = status_name_dto_to_pb(Status)
	};
sms_statuses_dto_to_pb(Statuses) ->
	[sms_statuses_dto_to_pb(Status) || Status <- Statuses].

sms_statuses_pb_to_dto(StatusesPB = #smsstatus{}) ->
	#smsstatus{
		address = Addr,
		status = Status
	} = StatusesPB,
	#k1api_sms_status_dto{
		address = addr_pb_to_dto(Addr),
		status = status_name_pb_to_dto(Status)
	};
sms_statuses_pb_to_dto(Statuses) ->
	[sms_statuses_pb_to_dto(Status) || Status <- Statuses].

status_name_dto_to_pb(submitted) -> 				'SUBMITTED';
status_name_dto_to_pb(success_waiting_delivery) ->	'SUCCESS_WAITING_DELIVERY';
status_name_dto_to_pb(success_no_delivery) ->		'SUCCESS_NO_DELIVERY';
status_name_dto_to_pb(failure) ->					'FAILURE';
status_name_dto_to_pb(enroute) ->					'ENROUTE';
status_name_dto_to_pb(delivered) ->					'DELIVERED';
status_name_dto_to_pb(expired) ->					'EXPIRED';
status_name_dto_to_pb(deleted) ->					'DELETED';
status_name_dto_to_pb(undeliverable) ->				'UNDELIVERABLE';
status_name_dto_to_pb(accepted) ->					'ACCEPTED';
status_name_dto_to_pb(unknown) ->					'UNKNOWN';
status_name_dto_to_pb(rejected) ->					'REJECTED';
status_name_dto_to_pb(unrecognized) ->				'UNRECOGNIZED'.


status_name_pb_to_dto('SUBMITTED') -> 					submitted;
status_name_pb_to_dto('SUCCESS_WAITING_DELIVERY') -> 	success_waiting_delivery;
status_name_pb_to_dto('SUCCESS_NO_DELIVERY') -> 		success_no_delivery;
status_name_pb_to_dto('FAILURE') -> 					failure;
status_name_pb_to_dto('ENROUTE') -> 					enroute;
status_name_pb_to_dto('DELIVERED') -> 					delivered;
status_name_pb_to_dto('EXPIRED') -> 					expired;
status_name_pb_to_dto('DELETED') -> 					deleted;
status_name_pb_to_dto('UNDELIVERABLE') -> 				undeliverable;
status_name_pb_to_dto('ACCEPTED') -> 					accepted;
status_name_pb_to_dto('UNKNOWN') -> 					unknown;
status_name_pb_to_dto('REJECTED') -> 					rejected;
status_name_pb_to_dto('UNRECOGNIZED') -> 				unrecognized.

