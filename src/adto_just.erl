-module(adto_just).

-export([
    encode/1,
    decode/2
]).

-include("adto.hrl").
-include("JustAsn.hrl").
-include("helpers.hrl").

%% ===================================================================
%% Encode Functions
%% ===================================================================

-spec encode(message_type_dto()) ->
    {ok, binary()} |
    {error, Reason :: any()}.
encode(DTO = #just_sms_request_dto{}) ->
    #just_sms_request_dto{
        id = ID,
        gateway_id = GtwID,
        customer_id = CustomerID,
        user_id = UserID,
        type = Type,
        message = Message,
        encoding = Encoding,
        params = Params,
        source_addr = SourceAddr,
        dest_addrs = DestAddrs,
        message_ids = MessageIDs,
        network_ids = NetworkIDs,
        prices = Prices
    } = DTO,
    Asn = #'SmsRequest'{
        id = binary_to_list(ID),
        gatewayId = binary_to_list(GtwID),
        customerId = binary_to_list(CustomerID),
        userId = binary_to_list(UserID),
        type = Type,
        message = binary_to_list(Message),
        encoding = sms_req_enc_to_asn(Encoding),
        params = sms_req_params_to_asn(Params),
        sourceAddr = full_addr_to_asn(SourceAddr),
        destAddrs = dest_addrs_to_asn(DestAddrs),
        messageIds = MessageIDs,
        networkIds = [binary_to_list(I) || I <- NetworkIDs],
        prices = [float_to_binary(P, [{decimals,2}, compact]) || P <- Prices]
    },
    case 'JustAsn':encode('SmsRequest', Asn) of
        {ok, DeepList} -> {ok, DeepList};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #just_sms_response_dto{}) ->
    #just_sms_response_dto{
        id = ID,
        gateway_id = GtwID,
        customer_id = CustomerID,
        statuses = Statuses,
        timestamp = Timestamp
    } = DTO,
    Asn = #'SmsResponse'{
        id = binary_to_list(ID),
        gatewayId = binary_to_list(GtwID),
        customerId = binary_to_list(CustomerID),
        statuses = sms_statuses_to_asn(Statuses),
        timestamp = binary_to_list(Timestamp)
    },
    case 'JustAsn':encode('SmsResponse', Asn) of
        {ok, DeepList} -> {ok, DeepList};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #just_incoming_sms_dto{}) ->
    #just_incoming_sms_dto{
        gateway_id = GtwID,
        source = Source,
        dest = Dest,
        message = Message,
        data_coding = DataCoding,
        parts_ref_num = PartsRefNum,
        parts_count = PartsCount,
        part_index = PartIndex,
        timestamp = Timestamp
    } = DTO,
    Asn = #'IncomingSm'{
        gatewayId = binary_to_list(GtwID),
        source = full_addr_to_asn(Source),
        dest = full_addr_to_asn(Dest),
        message = binary_to_list(Message),
        dataCoding = inc_sms_enc_to_asn(DataCoding),
        partsRefNum = to_optional_asn(PartsRefNum),
        partsCount = to_optional_asn(PartsCount),
        partIndex = to_optional_asn(PartIndex),
        timestamp = binary_to_list(Timestamp)
    },
    case 'JustAsn':encode('IncomingSm', Asn) of
        {ok, DeepList} -> {ok, DeepList};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #just_delivery_receipt_dto{}) ->
    #just_delivery_receipt_dto{
        gateway_id = GtwID,
        receipts = Receipts,
        timestamp = Timestamp
    } = DTO,
    Asn = #'ReceiptBatch'{
        gatewayId = binary_to_list(GtwID),
        receipts = just_receipt_to_asn(Receipts),
        timestamp = binary_to_list(Timestamp)
    },
    case 'JustAsn':encode('ReceiptBatch', Asn) of
        {ok, DeepList} -> {ok, DeepList};
        {error, Error} -> {error, Error}
    end;

encode(_) ->
    erlang:error(badarg).

%% ===================================================================
%% Decode Functions
%% ===================================================================

-spec decode(message_type_dto(), binary()) ->
    {ok, message_type_dto()} |
    {error, Reason :: any()}.
decode(#just_sms_request_dto{}, Bin) ->
    case 'JustAsn':decode('SmsRequest', Bin) of
        {ok, SmsRequest = #'SmsRequest'{}} ->
            #'SmsRequest'{
                id = ID,
                gatewayId = GtwID,
                customerId = CustomerID,
                userId = UserID,
                type = Type,
                message = Message,
                encoding = Encoding,
                params = Params,
                sourceAddr = SourceAddr,
                destAddrs = DestAddrs,
                messageIds = MessageIDs,
                networkIds = NetworkIds,
                prices = Prices
            } = SmsRequest,
        DTO = #just_sms_request_dto{
            id = list_to_binary(ID),
            gateway_id = list_to_binary(GtwID),
            customer_id = list_to_binary(CustomerID),
            user_id = list_to_binary(UserID),
            client_type = funnel,
            type = Type,
            message = list_to_binary(Message),
            encoding = sms_req_enc_to_dto(Encoding),
            params = sms_req_params_to_dto(Params),
            source_addr = full_addr_to_dto(SourceAddr),
            dest_addrs = dest_addrs_to_dto(DestAddrs),
            message_ids = [list_to_binary(I) || I <- MessageIDs],
            network_ids = [list_to_binary(I) || I <- NetworkIds],
            prices = [list_to_float(P) || P <- Prices]
        },
        {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#just_sms_response_dto{}, Bin) ->
    case 'JustAsn':decode('SmsResponse', Bin) of
        {ok, SmsResponse = #'SmsResponse'{}} ->
            #'SmsResponse'{
                id = ID,
                gatewayId = GtwID,
                customerId = CustomerID,
                statuses = Statuses,
                timestamp = Timestamp
            } = SmsResponse,
            StatusesDTO = sms_statuses_to_dto(Statuses),
            DTO = #just_sms_response_dto{
                id = list_to_binary(ID),
                gateway_id = list_to_binary(GtwID),
                customer_id = list_to_binary(CustomerID),
                statuses = StatusesDTO,
                timestamp = list_to_binary(Timestamp)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#just_incoming_sms_dto{}, Bin) ->
    case 'JustAsn':decode('IncomingSm', Bin) of
        {ok, IncomingSms = #'IncomingSm'{}} ->
            #'IncomingSm'{
                gatewayId = GtwID,
                source = Source,
                dest = Dest,
                message = Message,
                dataCoding = DataCoding,
                partsRefNum = PartsRefNum,
                partsCount = PartsCount,
                partIndex = PartIndex,
                timestamp = Timestamp
            } = IncomingSms,
            DTO = #just_incoming_sms_dto{
                gateway_id = list_to_binary(GtwID),
                source = full_addr_to_dto(Source),
                dest = full_addr_to_dto(Dest),
                message = list_to_binary(Message),
                data_coding = inc_sms_enc_to_dto(DataCoding),
                parts_ref_num = from_optional_asn(PartsRefNum),
                parts_count = from_optional_asn(PartsCount),
                part_index = from_optional_asn(PartIndex),
                timestamp = list_to_binary(Timestamp)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#just_delivery_receipt_dto{}, Bin) ->
    case 'JustAsn':decode('ReceiptBatch', Bin) of
        {ok, Asn} ->
            #'ReceiptBatch'{
                gatewayId = GtwID,
                receipts = Receipts,
                timestamp = Timestamp
            } = Asn,
            DTO = #just_delivery_receipt_dto{
                gateway_id = list_to_binary(GtwID),
                receipts = just_receipt_to_dto(Receipts),
                timestamp = list_to_binary(Timestamp)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(_, _) ->
    erlang:error(badarg).

%% ===================================================================
%% Local Functions
%% ===================================================================

%% Params

sms_req_params_to_asn(Param = #just_sms_request_param_dto{}) ->
    #just_sms_request_param_dto{
        name = Name,
        value = Value
    } = Param,
    ConvertedValue =
        case Value of
            {string, String} -> {string, binary_to_list(String)};
            _ -> Value
        end,
    #'Param'{
        name = binary_to_list(Name),
        value = ConvertedValue
    };
sms_req_params_to_asn(Params) ->
    [sms_req_params_to_asn(Param) || Param <- Params].

sms_req_params_to_dto(Param = #'Param'{}) ->
    #'Param'{
        name = Name,
        value = Value
    } = Param,
    ConvertedValue =
        case Value of
            {string, String} -> {string, list_to_binary(String)};
            _ -> Value
        end,
    #just_sms_request_param_dto{
        name = list_to_binary(Name),
        value = ConvertedValue
    };
sms_req_params_to_dto(Params) ->
    [sms_req_params_to_dto(Param) || Param <- Params].


%% FullAddr

full_addr_to_asn(FAddr = #addr{ref_num = undefined}) ->
    #addr{
        addr = Addr,
        ton = TON,
        npi = NPI
    } = FAddr,
    #'FullAddr'{
        addr = binary_to_list(Addr),
        ton = TON,
        npi = NPI
    };
full_addr_to_asn(FAddr = #addr{}) ->
    #addr{
        addr = Addr,
        ton = TON,
        npi = NPI,
        ref_num = RefNum
    } = FAddr,
    #'FullAddrAndRefNum'{
        fullAddr = #'FullAddr'{addr = binary_to_list(Addr), ton = TON, npi = NPI},
        refNum = RefNum
    }.


full_addr_to_dto(FAddr = #'FullAddr'{}) ->
    #'FullAddr'{
        addr = Addr,
        ton = TON,
        npi = NPI
    } = FAddr,
    #addr{
        addr = list_to_binary(Addr),
        ton = TON,
        npi = NPI
    };
full_addr_to_dto(FAddr = #'FullAddrAndRefNum'{}) ->
    #'FullAddrAndRefNum'{
        fullAddr = #'FullAddr'{addr = Addr, ton = TON, npi = NPI},
        refNum = RefNum
    } = FAddr,
    #addr{
        addr = list_to_binary(Addr),
        ton = TON,
        npi = NPI,
        ref_num = RefNum
    }.

%% DestAddr

dest_addrs_to_dto({Type, Addresses}) ->
    {Type, [full_addr_to_dto(Addr) || Addr <- Addresses]}.

dest_addrs_to_asn({Type, Addresses}) ->
    {Type, [full_addr_to_asn(Addr) || Addr <- Addresses]}.

%% Sms Statuses

sms_statuses_to_asn(DTO = #just_sms_status_dto{}) ->
    #just_sms_status_dto{
        original_id = OriginalID,
        dest_addr = DestAddr,
        status = Status,
        parts_total = PartsTotal,
        part_index = PartIndex,
        message_id = MessageID,
        error_code = ErrorCode
    } = DTO,
    #'SmStatus'{
        originalId = binary_to_list(OriginalID),
        destAddr = full_addr_to_asn(DestAddr),
        status = Status,
        partsTotal = PartsTotal,
        partIndex = to_optional_asn(PartIndex),
        messageId = to_optional_asn(MessageID, fun binary_to_list/1),
        errorCode = to_optional_asn(ErrorCode)
    };
sms_statuses_to_asn(Statuses) ->
    [sms_statuses_to_asn(Status) || Status <- Statuses].

sms_statuses_to_dto(Statuses) ->
    sms_statuses_to_dto(Statuses, []).
sms_statuses_to_dto([], Acc) ->
    lists:reverse(Acc);
sms_statuses_to_dto([SmsStatus | RestStatuses], Acc) ->
    #'SmStatus'{
        originalId = OriginalID,
        destAddr = DestAddr,
        status = Status,
        partsTotal = PartsTotal,
        partIndex = PartIndex,
        messageId = MessageID,
        errorCode = ErrorCode
    } = SmsStatus,
    DTO = #just_sms_status_dto{
        original_id = list_to_binary(OriginalID),
        dest_addr = full_addr_to_dto(DestAddr),
        status = Status,
        parts_total = PartsTotal,
        part_index = from_optional_asn(PartIndex),
        message_id = from_optional_asn(MessageID, fun list_to_binary/1),
        error_code = from_optional_asn(ErrorCode)
    },
    sms_statuses_to_dto(RestStatuses, [DTO | Acc]).


%% Optional Asn Values

to_optional_asn(OptionalValue) ->
    to_optional_asn(OptionalValue, fun(Value) -> Value end).
to_optional_asn(OptionalValue, Fun) ->
    case OptionalValue of
            undefined ->
                asn1_NOVALUE;
            Value ->
                Fun(Value)
    end.


from_optional_asn(OptionalValue) ->
    from_optional_asn(OptionalValue, fun(Value) -> Value end).
from_optional_asn(OptionalValue, Fun) ->
    case OptionalValue of
            asn1_NOVALUE ->
                undefined;
            Value ->
                Fun(Value)
    end.


%% Just Delivery Receipts

just_receipt_to_asn(DTO = #just_receipt_dto{}) ->
    #just_receipt_dto{
        message_id = MessageID,
        message_state = MessageState,
        source = Source
    } = DTO,
    #'DeliveryReceipt'{
        messageId = binary_to_list(MessageID),
        messageState = MessageState,
        source = full_addr_to_asn(Source)
    };
just_receipt_to_asn(Receipts) ->
    [just_receipt_to_asn(Receipt) || Receipt <- Receipts].


just_receipt_to_dto(Asn = #'DeliveryReceipt'{}) ->
    #'DeliveryReceipt'{
        messageId = MessageID,
        messageState = MessageState,
        source = Source
    } = Asn,
    #just_receipt_dto{
        message_id = list_to_binary(MessageID),
        message_state = MessageState,
        source = full_addr_to_dto(Source)
    };
just_receipt_to_dto(Receipts) ->
    [just_receipt_to_dto(Receipt) || Receipt <- Receipts].

sms_req_enc_to_asn(Encoding) when
                        Encoding =:= gsm0338
                orelse  Encoding =:= default
                orelse  Encoding =:= ascii
                orelse  Encoding =:= latin1
                orelse  Encoding =:= ucs2  ->
    {text, Encoding};
sms_req_enc_to_asn(Encoding) when is_integer(Encoding) ->
    {other, Encoding}.

sms_req_enc_to_dto({_, Encoding}) ->
    Encoding.

inc_sms_enc_to_dto(Enc) when is_integer(Enc) ->
    if
        Enc =:= 0; Enc =:= 16; Enc =:= 240 -> gsm0338;
        Enc =:= 1 -> ascii;
        Enc =:= 3 -> latin1;
        Enc =:= 8; Enc =:= 24 -> ucs2;
        true -> Enc
    end.

inc_sms_enc_to_asn(gsm0338) -> 0;
inc_sms_enc_to_asn(ascii)   -> 1;
inc_sms_enc_to_asn(latin1)  -> 3;
inc_sms_enc_to_asn(ucs2)    -> 8;
inc_sms_enc_to_asn(Enc) when is_integer(Enc) -> Enc.
