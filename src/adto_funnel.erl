-module(adto_funnel).

-export([
    encode/1,
    decode/2
]).

-include("adto.hrl").
-include("FunnelAsn.hrl").
-include("helpers.hrl").

%% ===================================================================
%% Decode Functions
%% ===================================================================

-spec decode(message_type_dto(), binary()) ->
    {ok, message_type_dto()} |
    {error, Reason :: any()}.

decode(#funnel_auth_request_dto{}, Bin) ->
    case 'FunnelAsn':decode('BindRequest', Bin) of
        {ok, Asn} ->
            #'BindRequest'{
                connectionId = ConnectionID,
                remoteIp = IP,
                customerId = CustomerID,
                userId = UserID,
                password = Password,
                type = Type,
                isCached = IsCached,
                timestamp = Timestamp,
                expiration = Expiration
            } = Asn,
            DTO = #funnel_auth_request_dto{
                connection_id = list_to_binary(ConnectionID),
                ip = list_to_binary(IP),
                customer_id = list_to_binary(CustomerID),
                user_id = list_to_binary(UserID),
                password = list_to_binary(Password),
                type = Type,
                is_cached = IsCached,
                timestamp = precise_time_to_dto(Timestamp),
                expiration = precise_time_to_dto(Expiration)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#funnel_auth_response_dto{}, Bin) ->
    case 'FunnelAsn':decode('BindResponse', Bin) of
        {ok, Asn} ->
            #'BindResponse'{
                connectionId = ID,
                result = ResultAsn
            } = Asn,
            DTO = #funnel_auth_response_dto{
                connection_id = list_to_binary(ID),
                result = funnel_auth_response_result_to_dto(ResultAsn)
            },
            {ok, DTO};
        {error, Error} ->
            {error, Error}
    end;

decode(#funnel_started_event_dto{}, Bin) ->
    case 'FunnelAsn':decode('ServerUpEvent', Bin) of
        {ok, Asn} ->
            #'ServerUpEvent'{
                timestamp = Timestamp
            } = Asn,
            DTO = #funnel_started_event_dto{
                timestamp = list_to_binary(Timestamp)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#funnel_stopped_event_dto{}, Bin) ->
    case 'FunnelAsn':decode('ServerDownEvent', Bin) of
        {ok, Asn} ->
            #'ServerDownEvent'{
                timestamp = Timestamp
            } = Asn,
            DTO = #funnel_stopped_event_dto{
                timestamp = list_to_binary(Timestamp)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#funnel_client_online_event_dto{}, Bin) ->
    case 'FunnelAsn':decode('ConnectionUpEvent', Bin) of
        {ok, Asn} ->
            #'ConnectionUpEvent'{
                connectionId = ConnectionID,
                customerId = CustomerID,
                userId = UserID,
                type = Type,
                connectedAt = ConnectedAt,
                timestamp = Timestamp
            } = Asn,
            DTO = #funnel_client_online_event_dto{
                connection_id = list_to_binary(ConnectionID),
                customer_id = list_to_binary(CustomerID),
                user_id = list_to_binary(UserID),
                type = Type,
                connected_at = list_to_binary(ConnectedAt),
                timestamp = list_to_binary(Timestamp)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#funnel_client_offline_event_dto{}, Bin) ->
    case 'FunnelAsn':decode('ConnectionDownEvent', Bin) of
        {ok, Asn} ->
            #'ConnectionDownEvent'{
                connectionId = ConnectionID,
                customerId = CustomerID,
                userId = UserID,
                type = Type,
                connectedAt = ConnectedAt,
                msgsReceived = MsgsReceived,
                msgsSent = MsgsSent,
                errors = Errors,
                reason = Reason,
                timestamp = Timestamp
            } = Asn,
            DTO = #funnel_client_offline_event_dto{
                connection_id = list_to_binary(ConnectionID),
                customer_id = list_to_binary(CustomerID),
                user_id = list_to_binary(UserID),
                type = Type,
                connected_at = list_to_binary(ConnectedAt),
                msgs_received = MsgsReceived,
                msgs_sent = MsgsSent,
                errors = [errors_to_dto(Error) || Error <- Errors],
                reason = Reason,
                timestamp = list_to_binary(Timestamp)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#funnel_incoming_sms_dto{}, Bin) ->
    case 'FunnelAsn':decode('OutgoingBatch', Bin) of
        {ok, Asn} ->
            #'OutgoingBatch'{
                id = ID,
                messages = Messages
            } = Asn,
            DTO = #funnel_incoming_sms_dto{
                id = list_to_binary(ID),
                messages = incoming_messages_to_dto(Messages)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#funnel_delivery_receipt_dto{}, Bin) ->
    case 'FunnelAsn':decode('ReceiptBatch', Bin) of
        {ok, Asn} ->
            #'ReceiptBatch'{
                id = ID,
                receipts = Receipts
            } = Asn,
            DTO = #funnel_delivery_receipt_dto{
                id = list_to_binary(ID),
                receipts = receipts_to_dto(Receipts)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#funnel_ack_dto{}, Bin) ->
    case 'FunnelAsn':decode('BatchAck', Bin) of
        {ok, Asn} ->
            #'BatchAck'{
                batchId = ID
            } = Asn,
            DTO = #funnel_ack_dto{
                id = list_to_binary(ID)
            },
            {ok, DTO};
        {error, Error} -> {error, Error}
    end;

decode(#funnel_connections_request_dto{}, _Bin) ->
    {ok, #funnel_connections_request_dto{}};

decode(#funnel_connections_response_dto{}, Bin) ->
    {ok, Asn} = 'FunnelAsn':decode('ConnectionsResponse', Bin),
     #'ConnectionsResponse'{
        connections = Connections
    } = Asn,
    ConvertConnection = fun(ConnectionASN = #'Connection'{}) ->
        #'Connection'{
            connectionId = UUID,
            remoteIp = IP,
            customerId = SystemID,
            userId = UserID,
            connectedAt = ConnectedAt,
            type = Type,
            msgsReceived = Received,
            msgsSent = Sent,
            errors = Errors
        } = ConnectionASN,
        #funnel_connection_dto{
            connection_id = list_to_binary(UUID),
            remote_ip = list_to_binary(IP),
            customer_id = list_to_binary(SystemID),
            user_id = list_to_binary(UserID),
            connected_at = list_to_binary(ConnectedAt),
            type = Type,
            msgs_received = Received,
            msgs_sent = Sent,
            errors = lists:map(fun errors_to_dto/1, Errors)
        }
    end,
    DTO = #funnel_connections_response_dto{
        connections = lists:map(ConvertConnection, Connections)
    },
    {ok, DTO};

decode(Type, _Message) ->
    erlang:error({funnel_decode_not_supported, Type}).

%% ===================================================================
%% Encode Functions
%% ===================================================================

-spec encode(message_type_dto()) ->
    {ok, Payload :: binary()} |
    {error, Reason :: any()}.

encode(DTO = #funnel_auth_request_dto{}) ->
    #funnel_auth_request_dto{
        connection_id = ConnectionID,
        ip = IP,
        customer_id = CustomerID,
        user_id = UserID,
        password = Password,
        type = Type,
        is_cached = IsCached,
        timestamp = Timestamp,
        expiration = Expiration
    } = DTO,
    Asn = #'BindRequest'{
        connectionId = binary_to_list(ConnectionID),
        remoteIp = binary_to_list(IP),
        customerId = binary_to_list(CustomerID),
        userId = binary_to_list(UserID),
        password = binary_to_list(Password),
        type = Type,
        isCached = IsCached,
        timestamp = precise_time_to_asn(Timestamp),
        expiration = precise_time_to_asn(Expiration)
    },
    case 'FunnelAsn':encode('BindRequest', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_auth_response_dto{result = {customer, _}}) ->
    #funnel_auth_response_dto{
        connection_id = ConnectionID,
        result = {customer, CustomerDTO}
    } = DTO,
    #funnel_auth_response_customer_dto{
        id = SystemID,
        uuid = UUID,
        priority = Priority,
        rps = RPS,
        allowed_sources = AllowedSources,
        default_source = DefaultSource,
        networks = Networks,
        providers = Providers,
        default_provider_id = DefaultProviderID,
        receipts_allowed = ReceiptsAllowed,
        no_retry = NoRetry,
        default_validity = DefaultValidity,
        max_validity = MaxValidity,
        pay_type = PayType
    } = CustomerDTO,
    CustomerAsn = #'Customer'{
        id = binary_to_list(SystemID),
        uuid = binary_to_list(UUID),
        priority = Priority,
        rps = to_optional_asn(RPS),
        allowedSources = [addr_to_asn(Source) || Source <- AllowedSources],
        defaultSource = to_optional_asn(DefaultSource, fun addr_to_asn/1),
        networks = networks_to_asn(Networks),
        providers = providers_to_asn(Providers),
        defaultProviderId = to_optional_asn(DefaultProviderID, fun erlang:binary_to_list/1),
        receiptsAllowed = ReceiptsAllowed,
        noRetry = NoRetry,
        defaultValidity = binary_to_list(DefaultValidity),
        maxValidity = MaxValidity,
        payType = PayType
    },
    Asn = #'BindResponse'{
        connectionId = binary_to_list(ConnectionID),
        result = {customer, CustomerAsn}
    },
    case 'FunnelAsn':encode('BindResponse', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_auth_response_dto{result = {error, _}}) ->
    #funnel_auth_response_dto{
        connection_id = ConnectionID,
        result = {error, Error}
    } = DTO,
    Asn = #'BindResponse'{
        connectionId = binary_to_list(ConnectionID),
        result = {error, Error}
    },
    case 'FunnelAsn':encode('BindResponse', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_started_event_dto{}) ->
    #funnel_started_event_dto{
        timestamp = Timestamp
    } = DTO,
    Asn = #'ServerUpEvent'{
        timestamp = binary_to_list(Timestamp)
    },
    case 'FunnelAsn':encode('ServerUpEvent', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_stopped_event_dto{}) ->
    #funnel_stopped_event_dto{
        timestamp = Timestamp
    } = DTO,
    Asn = #'ServerDownEvent'{
        timestamp = binary_to_list(Timestamp)
    },
    case 'FunnelAsn':encode('ServerDownEvent', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_client_online_event_dto{}) ->
    #funnel_client_online_event_dto{
        connection_id = ConnectionID,
        customer_id = CustomerID,
        user_id = UserID,
        type = Type,
        connected_at = ConnectedAt,
        timestamp = Timestamp
    } = DTO,
    Asn = #'ConnectionUpEvent'{
        connectionId = binary_to_list(ConnectionID),
        customerId = binary_to_list(CustomerID),
        userId = binary_to_list(UserID),
        type = Type,
        connectedAt = binary_to_list(ConnectedAt),
        timestamp = binary_to_list(Timestamp)
    },
    case 'FunnelAsn':encode('ConnectionUpEvent', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_client_offline_event_dto{}) ->
    #funnel_client_offline_event_dto{
        connection_id = ConnectionID,
        customer_id = CustomerID,
        user_id = UserID,
        type = Type,
        connected_at = ConnectedAt,
        msgs_received = MsgsReceived,
        msgs_sent = MsgsSent,
        errors = Errors,
        reason = Reason,
        timestamp = Timestamp
    } = DTO,
    Asn = #'ConnectionDownEvent'{
        connectionId = binary_to_list(ConnectionID),
        customerId = binary_to_list(CustomerID),
        userId = binary_to_list(UserID),
        type = Type,
        connectedAt = binary_to_list(ConnectedAt),
        msgsReceived = MsgsReceived,
        msgsSent = MsgsSent,
        errors = [errors_to_asn(Error) || Error <- Errors],
        reason = Reason,
        timestamp = binary_to_list(Timestamp)
    },
    case 'FunnelAsn':encode('ConnectionDownEvent', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_incoming_sms_dto{}) ->
    #funnel_incoming_sms_dto{
        id = ID,
        messages = Messages
    } = DTO,
    Asn = #'OutgoingBatch'{
        id = binary_to_list(ID),
        messages = incoming_messages_to_asn(Messages)
    },
    case 'FunnelAsn':encode('OutgoingBatch', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_delivery_receipt_dto{}) ->
    #funnel_delivery_receipt_dto{
        id = ID,
        receipts = Receipts
    } = DTO,
    Asn = #'ReceiptBatch'{
        id = binary_to_list(ID),
        receipts = receipts_to_asn(Receipts)
    },
    case 'FunnelAsn':encode('ReceiptBatch', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_ack_dto{}) ->
    #funnel_ack_dto{
        id = ID
    } = DTO,
    Asn = #'BatchAck'{
        batchId = binary_to_list(ID)
    },
    case 'FunnelAsn':encode('BatchAck', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(_DTO = #funnel_connections_request_dto{}) ->
    Asn = #'ConnectionsRequest'{},
    case 'FunnelAsn':encode('ConnectionsRequest', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(DTO = #funnel_connections_response_dto{}) ->
    ConvertConnection = fun(ConnectionDTO = #funnel_connection_dto{}) ->
        #funnel_connection_dto{
            connection_id = UUID,
            remote_ip = IP,
            customer_id = SystemID,
            user_id = UserID,
            connected_at = ConnectedAt,
            type = Type,
            msgs_received = Received,
            msgs_sent = Sent,
            errors = Errors
        } = ConnectionDTO,
        #'Connection'{
            connectionId = binary_to_list(UUID),
            remoteIp = binary_to_list(IP),
            customerId = binary_to_list(SystemID),
            userId = binary_to_list(UserID),
            connectedAt = binary_to_list(ConnectedAt),
            type = Type,
            msgsReceived = Received,
            msgsSent = Sent,
            errors = lists:map(fun errors_to_asn/1, Errors)
        }
    end,
    #funnel_connections_response_dto{
        connections = Connections
    } = DTO,
    Asn = #'ConnectionsResponse'{
        connections = lists:map(ConvertConnection, Connections)
    },
    case 'FunnelAsn':encode('ConnectionsResponse', Asn) of
        {ok, DeepList} -> {ok, list_to_binary(DeepList)};
        {error, Error} -> {error, Error}
    end;

encode(Message) ->
    erlang:error({funnel_encode_not_supported, Message}).


%% ===================================================================
%% Local Functions
%% ===================================================================

%% PreciseTime

precise_time_to_asn(DTO = #fun_precise_time_dto{}) ->
    #fun_precise_time_dto{
        time = Time,
        milliseconds = Milliseconds
    } = DTO,
    #'PreciseTime'{
        time = binary_to_list(Time),
        milliseconds = Milliseconds
    }.

precise_time_to_dto(Asn = #'PreciseTime'{}) ->
    #'PreciseTime'{
        time = Time,
        milliseconds = Milliseconds
    } = Asn,
    #fun_precise_time_dto{
        time = list_to_binary(Time),
        milliseconds = Milliseconds
    }.

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

%% Addr

addr_to_asn(FullAddr = #addr{}) ->
    #addr{
        addr = Addr,
        ton = TON,
        npi = NPI
    } = FullAddr,
    #'Addr'{
        addr = binary_to_list(Addr),
        ton = TON,
        npi = NPI
    };
addr_to_asn(Addrs) ->
    [addr_to_asn(Addr) || Addr <- Addrs].

addr_to_dto(FullAddr = #'Addr'{}) ->
    #'Addr'{
        addr = Addr,
        ton = TON,
        npi = NPI
    } = FullAddr,
    #addr{
        addr = list_to_binary(Addr),
        ton = TON,
        npi = NPI
    };
addr_to_dto(Addrs) ->
    [addr_to_dto(Addr) || Addr <- Addrs].

%% Networks

networks_to_asn(Network = #network_dto{}) ->
    #network_dto{
        id = ID,
        country_code = CountryCode,
        numbers_len = NumbersLength,
        prefixes = Prefixes,
        provider_id = ProviderID
    } = Network,
    #'Network'{
        id = binary_to_list(ID),
        countryCode = binary_to_list(CountryCode),
        numbersLen = NumbersLength,
        prefixes = [binary_to_list(Prefix) || Prefix <- Prefixes],
        providerId = binary_to_list(ProviderID)
    };
networks_to_asn(Networks) ->
    [networks_to_asn(Network) || Network <- Networks].

networks_to_dto(Network = #'Network'{}) ->
    #'Network'{
        id = ID,
        countryCode = CountryCode,
        numbersLen = NumbersLength,
        prefixes = Prefixes,
        providerId = ProviderID
    } = Network,
    #network_dto{
        id = list_to_binary(ID),
        country_code = list_to_binary(CountryCode),
        numbers_len = NumbersLength,
        prefixes = [list_to_binary(Prefix) || Prefix <- Prefixes],
        provider_id = list_to_binary(ProviderID)
    };
networks_to_dto(Networks) ->
    [networks_to_dto(Network) || Network <- Networks].

%% Providers

providers_to_asn(Provider = #provider_dto{}) ->
    #provider_dto{
        id = ID,
        gateway_id = GtwID,
        bulk_gateway_id = BulkGtwID,
        receipts_supported = ReceiptsSupported
    } = Provider,
    #'Provider'{
        id = binary_to_list(ID),
        gatewayId = binary_to_list(GtwID),
        bulkGatewayId = binary_to_list(BulkGtwID),
        receiptsSupported = ReceiptsSupported
    };
providers_to_asn(Providers) ->
    [providers_to_asn(Provider) || Provider <- Providers].

providers_to_dto(Provider = #'Provider'{}) ->
    #'Provider'{
        id = ID,
        gatewayId = GtwID,
        bulkGatewayId = BulkGtwID,
        receiptsSupported = ReceiptsSupported
    } = Provider,
    #provider_dto{
        id = list_to_binary(ID),
        gateway_id = list_to_binary(GtwID),
        bulk_gateway_id = list_to_binary(BulkGtwID),
        receipts_supported = ReceiptsSupported
    };
providers_to_dto(Providers) ->
    [providers_to_dto(Provider) || Provider <- Providers].

%% Funnel Auth Result

funnel_auth_response_result_to_dto({customer, CustomerAsn}) ->
    #'Customer'{
        id = SystemID,
        uuid = UUID,
        priority = Priority,
        rps = RPS,
        allowedSources = AllowedSources,
        defaultSource = DefaultSource,
        networks = Networks,
        providers = Providers,
        defaultProviderId = DefaultProviderID,
        receiptsAllowed = ReceiptsAllowed,
        noRetry = NoRetry,
        defaultValidity = DefaultValidity,
        maxValidity = MaxValidity,
        payType = PayType
    } = CustomerAsn,
    CustomerDTO = #funnel_auth_response_customer_dto{
        id = list_to_binary(SystemID),
        uuid = list_to_binary(UUID),
        priority = Priority,
        rps = from_optional_asn(RPS),
        allowed_sources = [addr_to_dto(Source) || Source <- AllowedSources],
        default_source = from_optional_asn(DefaultSource, fun addr_to_dto/1),
        networks = networks_to_dto(Networks),
        providers = providers_to_dto(Providers),
        default_provider_id = from_optional_asn(DefaultProviderID, fun erlang:list_to_binary/1),
        receipts_allowed = ReceiptsAllowed,
        no_retry = NoRetry,
        default_validity = list_to_binary(DefaultValidity),
        max_validity = MaxValidity,
        pay_type = PayType
    },
    {customer, CustomerDTO};

funnel_auth_response_result_to_dto({error, Error}) ->
    {error, Error}.

%% Client Errors

errors_to_asn(Error = #error_dto{}) ->
    #error_dto{
        error_code = ErrorCode,
        timestamp = Timestamp
    } = Error,
    #'Error'{
        errorCode = ErrorCode,
        timestamp = binary_to_list(Timestamp)
    }.

errors_to_dto(Error = #'Error'{}) ->
    #'Error'{
        errorCode = ErrorCode,
        timestamp = Timestamp
    } = Error,
    #error_dto{
        error_code = ErrorCode,
        timestamp = list_to_binary(Timestamp)
    }.

%% Incoming Messages

incoming_messages_to_asn(Message = #funnel_incoming_sms_message_dto{}) ->
    #funnel_incoming_sms_message_dto{
        source  = Source,
        dest = Dest,
        message = MessageBody,
        data_coding = DataCoding
    } = Message,
    #'OutgoingMessage'{
        source = addr_to_asn(Source),
        dest = addr_to_asn(Dest),
        message = binary_to_list(MessageBody),
        dataCoding = message_encoding_to_asn(DataCoding)
    };
incoming_messages_to_asn(Messages) ->
    [incoming_messages_to_asn(Message) || Message <- Messages].

incoming_messages_to_dto(Message = #'OutgoingMessage'{}) ->
    #'OutgoingMessage'{
        source = Source,
        dest = Dest,
        message = MessageBody,
        dataCoding = DataCoding
    } = Message,
    #funnel_incoming_sms_message_dto{
        source  = addr_to_dto(Source),
        dest = addr_to_dto(Dest),
        message = list_to_binary(MessageBody),
        data_coding = message_encoding_to_dto(DataCoding)
    };
incoming_messages_to_dto(Messages) ->
    [incoming_messages_to_dto(Message) || Message <- Messages].

%% Receipts

receipts_to_asn(Receipt = #funnel_delivery_receipt_container_dto{}) ->
    #funnel_delivery_receipt_container_dto{
        message_id = MessageID,
        submit_date = SubmitDate,
        done_date = DoneDate,
        message_state = MessageState,
        source = SourceAddr,
        dest  = DestAddr
    } = Receipt,
    #'DeliveryReceipt'{
        messageId = binary_to_list(MessageID),
        submitDate = binary_to_list(SubmitDate),
        doneDate = binary_to_list(DoneDate),
        messageState = MessageState,
        source = addr_to_asn(SourceAddr),
        dest = addr_to_asn(DestAddr)
    };
receipts_to_asn(Receipts) ->
    [receipts_to_asn(Receipt) || Receipt <- Receipts].

receipts_to_dto(Receipt = #'DeliveryReceipt'{}) ->
    #'DeliveryReceipt'{
        messageId = MessageID,
        submitDate = SubmitDate,
        doneDate = DoneDate,
        messageState = MessageState,
        source = SourceAddr,
        dest = DestAddr
    } = Receipt,
    #funnel_delivery_receipt_container_dto{
        message_id = list_to_binary(MessageID),
        submit_date = list_to_binary(SubmitDate),
        done_date = list_to_binary(DoneDate),
        message_state = MessageState,
        source = addr_to_dto(SourceAddr),
        dest  = addr_to_dto(DestAddr)
    };
receipts_to_dto(Receipts) ->
    [receipts_to_dto(Receipt) || Receipt <- Receipts].

message_encoding_to_dto({_, Encoding}) ->
    Encoding.
message_encoding_to_asn(Encoding) when
               Encoding =:= gsm0338
        orelse Encoding =:= ucs2 ->
    {text, Encoding};
message_encoding_to_asn(Encoding) when is_integer(Encoding) ->
    {other, Encoding};
message_encoding_to_asn(_) ->
    erlang:error(badarg).
