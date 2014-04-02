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
    PB = k1api_pb:decode_smsdeliverystatusreq(Bin),
    #smsdeliverystatusreq{
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
    PB = k1api_pb:decode_smsdeliverystatusresp(Bin),
    #smsdeliverystatusresp{
        id = ID,
        statuses = Statuses
    } = PB,
    DTO = #k1api_sms_delivery_status_response_dto{
        id = ID,
        statuses = sms_statuses_pb_to_dto(Statuses)
    },
    {ok, DTO};

decode(#k1api_retrieve_sms_request_dto{}, Bin) ->
    PB = k1api_pb:decode_retrievesmsreq(Bin),
    #retrievesmsreq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        dest_addr = Address,
        batch_size = BatchSize
    } = PB,
    DTO = #k1api_retrieve_sms_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        dest_addr = addr_pb_to_dto(Address),
        batch_size = BatchSize
    },
    {ok, DTO};

decode(#k1api_retrieve_sms_response_dto{}, Bin) ->
    PB = k1api_pb:decode_retrievesmsresp(Bin),
    #retrievesmsresp{
        id = ID,
        messages = RetrievedMessagesPB,
        total = TotalMessages
    } = PB,
    DTO = #k1api_retrieve_sms_response_dto{
        id = ID,
        messages = retrieved_messages_to_dto(RetrievedMessagesPB),
        total = TotalMessages
    },
    {ok, DTO};

decode(#k1api_remove_retrieved_sms_request_dto{}, Bin) ->
    PB = k1api_pb:decode_removeretrievedmessages(Bin),
    #removeretrievedmessages{
        id = ID,
        message_ids = MessageIDs
    } = PB,
    DTO = #k1api_remove_retrieved_sms_request_dto{
        id = ID,
        message_ids = MessageIDs
    },
    {ok, DTO};

decode(#k1api_subscribe_incoming_sms_request_dto{}, Bin) ->
    PB = k1api_pb:decode_subscribeincomingsmsreq(Bin),
    #subscribeincomingsmsreq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        dest_addr = DestAddr,
        notify_url = NotifyURL,
        criteria = Criteria,
        notification_format = Format,
        correlator = Correlator,
        callback_data = Callback
    } = PB,
    DTO = #k1api_subscribe_incoming_sms_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        dest_addr = addr_pb_to_dto(DestAddr),
        notify_url = NotifyURL,
        criteria = Criteria,
        notification_format = Format,
        correlator = Correlator,
        callback_data = Callback
    },
    {ok, DTO};

decode(#k1api_subscribe_incoming_sms_response_dto{}, Bin) ->
    PB = k1api_pb:decode_subscribeincomingsmsresp(Bin),
    #subscribeincomingsmsresp{
        id = ID,
        subscription_id = SubscriptionID
    } = PB,
    DTO = #k1api_subscribe_incoming_sms_response_dto{
        id = ID,
        subscription_id = SubscriptionID
    },
    {ok, DTO};

decode(#k1api_unsubscribe_incoming_sms_request_dto{}, Bin) ->
    PB = k1api_pb:decode_unsubscribeincomingsmsreq(Bin),
    #unsubscribeincomingsmsreq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        subscription_id = SubscriptionID
    } = PB,
    DTO = #k1api_unsubscribe_incoming_sms_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        subscription_id = SubscriptionID
    },
    {ok, DTO};

decode(#k1api_unsubscribe_incoming_sms_response_dto{}, Bin) ->
    PB = k1api_pb:decode_unsubscribeincomingsmsresp(Bin),
    #unsubscribeincomingsmsresp{
        id = ID
    } = PB,
    DTO = #k1api_unsubscribe_incoming_sms_response_dto{
        id = ID
    },
    {ok, DTO};

decode(#k1api_sms_notification_request_dto{}, Bin) ->
    PB = k1api_pb:decode_smsnotificationreq(Bin),
    #smsnotificationreq{
        callback_data = Callback,
        datetime = DateTime,
        dest_addr = DestAddr,
        message_id = MessageID,
        message = Message,
        sender_addr = SenderAddr,
        notify_url = NotifyURL
    } = PB,
    DTO = #k1api_sms_notification_request_dto{
        callback_data = Callback,
        datetime = date_to_dto(DateTime),
        dest_addr = addr_pb_to_dto(DestAddr),
        message_id = MessageID,
        message = Message,
        sender_addr = addr_pb_to_dto(SenderAddr),
        notify_url = NotifyURL
    },
    {ok, DTO};

decode(#k1api_auth_request_dto{}, Bin) ->
    PB = k1api_pb:decode_authreq(Bin),
    #authreq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        password = Password,
        connection_type = ConnType
    } = PB,
    DTO = #k1api_auth_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        password = Password,
        connection_type = ConnType
    },
    {ok, DTO};

decode(#k1api_auth_response_dto{}, Bin) ->
    PB = k1api_pb:decode_authresp(Bin),
     #authresp{
        id = ID,
        result = Result,
        customer = Customer,
        error = Error
    } = PB,
    DTO = case Result of
        customer ->
            #authresp_customer{
                id = CustomerID,
                uuid = UUID,
                pay_type = PayType,
                allowed_sources = AllowedSources,
                default_source = DefaultSource,
                networks = Networks,
                providers = Providers,
                default_provider_id = DefProviderID,
                receipts_allowed = ReceiptsAllowed,
                no_retry = NoRetry,
                default_validity = DefValidity,
                max_validity = MaxValidity
            } = Customer,
            #k1api_auth_response_dto{
                id = ID,
                result = {customer, #k1api_auth_response_customer_dto{
                    id = CustomerID,
                    uuid = UUID,
                    pay_type = PayType,
                    allowed_sources = addr_pb_to_dto(AllowedSources),
                    default_source = addr_pb_to_dto(DefaultSource),
                    networks = network_pb_to_dto(Networks),
                    providers = provider_pb_to_dto(Providers),
                    default_provider_id = DefProviderID,
                    receipts_allowed = ReceiptsAllowed,
                    no_retry = NoRetry,
                    default_validity = DefValidity,
                    max_validity = MaxValidity
                }}
            };
        error ->
            #authresp_error{
                message = Message
            } = Error,
            #k1api_auth_response_dto{
                id = ID,
                result = {error, Message}
            }
    end,
    {ok, DTO};

decode(#k1api_subscribe_sms_receipts_request_dto{}, Bin) ->
    PB = k1api_pb:decode_smsreceiptssubscribereq(Bin),
    #smsreceiptssubscribereq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        url = Url,
        dest_addr = DestAddr,
        callback_data = Callback
    } = PB,
    DTO = #k1api_subscribe_sms_receipts_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        url = Url,
        dest_addr = addr_pb_to_dto(DestAddr),
        callback_data = Callback
    },
    {ok, DTO};

decode(#k1api_subscribe_sms_receipts_response_dto{}, Bin) ->
    PB = k1api_pb:decode_smsreceiptssubscriberesp(Bin),
    #smsreceiptssubscriberesp{
        id = ID
    } = PB,
    DTO = #k1api_subscribe_sms_receipts_response_dto{
        id = ID
    },
    {ok, DTO};

decode(#k1api_unsubscribe_sms_receipts_request_dto{}, Bin) ->
    PB = k1api_pb:decode_smsreceiptsunsubscribereq(Bin),
    #smsreceiptsunsubscribereq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        subscription_id = SubscriptionID
    } = PB,
    DTO = #k1api_unsubscribe_sms_receipts_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        subscription_id = SubscriptionID
    },
    {ok, DTO};

decode(#k1api_unsubscribe_sms_receipts_response_dto{}, Bin) ->
    PB = k1api_pb:decode_smsreceiptsunsubscriberesp(Bin),
    #smsreceiptsunsubscriberesp{
        id = ID
    } = PB,
    DTO = #k1api_unsubscribe_sms_receipts_response_dto{
        id = ID
    },
    {ok, DTO};

decode(#k1api_sms_delivery_receipt_notification_dto{}, Bin) ->
    PB = k1api_pb:decode_smsdeliveryreceiptnotification(Bin),
    #smsdeliveryreceiptnotification{
        id = ID,
        dest_addr = DestAddr,
        status = Status,
        callback_data = CallbackData,
        url = Url
    } = PB,
    DTO = #k1api_sms_delivery_receipt_notification_dto{
        id = ID,
        dest_addr = addr_pb_to_dto(DestAddr),
        status = status_name_pb_to_dto(Status),
        callback_data = CallbackData,
        url = Url
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
    PB = #smsdeliverystatusreq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        sms_request_id = RequestID,
        address = addr_to_pb(Addr)
    },
    Bin = k1api_pb:encode_smsdeliverystatusreq(PB),
    {ok, Bin};

encode(DTO = #k1api_sms_delivery_status_response_dto{}) ->
    #k1api_sms_delivery_status_response_dto{
        id = ID,
        statuses = Statuses
    } = DTO,
    PB = #smsdeliverystatusresp{
        id = ID,
        statuses = sms_statuses_dto_to_pb(Statuses)
    },
    Bin = k1api_pb:encode_smsdeliverystatusresp(PB),
    {ok, Bin};

encode(DTO = #k1api_retrieve_sms_request_dto{}) ->
    #k1api_retrieve_sms_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        dest_addr = Address,
        batch_size = BatchSize
    } = DTO,
    PB = #retrievesmsreq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        dest_addr = addr_to_pb(Address),
        batch_size = BatchSize
    },
    Bin = k1api_pb:encode_retrievesmsreq(PB),
    {ok, Bin};

encode(DTO = #k1api_retrieve_sms_response_dto{}) ->
    #k1api_retrieve_sms_response_dto{
        id = ID,
        messages = RetrievedMessagesDTO,
        total = TotalMessages
    } = DTO,
    PB = #retrievesmsresp{
        id = ID,
        messages = retrieved_messages_to_pb(RetrievedMessagesDTO),
        total = TotalMessages
    },
    Bin = k1api_pb:encode_retrievesmsresp(PB),
    {ok, Bin};

encode(DTO = #k1api_remove_retrieved_sms_request_dto{}) ->
    #k1api_remove_retrieved_sms_request_dto{
        id = ID,
        message_ids = MessageIDs
    } = DTO,
    PB = #removeretrievedmessages{
        id = ID,
        message_ids = MessageIDs
    },
    Bin = k1api_pb:encode_removeretrievedmessages(PB),
    {ok, Bin};

encode(DTO = #k1api_subscribe_incoming_sms_request_dto{}) ->
    #k1api_subscribe_incoming_sms_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        dest_addr = DestAddr,
        notify_url = NotifyURL,
        criteria = Criteria,
        notification_format = Format,
        correlator = Correlator,
        callback_data = Callback
    } = DTO,
    PB = #subscribeincomingsmsreq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        dest_addr = addr_to_pb(DestAddr),
        notify_url = NotifyURL,
        criteria = Criteria,
        notification_format = Format,
        correlator = Correlator,
        callback_data = Callback
    },
    Bin = k1api_pb:encode_subscribeincomingsmsreq(PB),
    {ok, Bin};

encode(DTO = #k1api_subscribe_incoming_sms_response_dto{}) ->
    #k1api_subscribe_incoming_sms_response_dto{
        id = ID,
        subscription_id = SubscriptionID
    } = DTO,
    PB = #subscribeincomingsmsresp{
        id = ID,
        subscription_id = SubscriptionID
    },
    Bin = k1api_pb:encode_subscribeincomingsmsresp(PB),
    {ok, Bin};

encode(DTO = #k1api_unsubscribe_incoming_sms_request_dto{}) ->
    #k1api_unsubscribe_incoming_sms_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        subscription_id = SubscriptionID
    } = DTO,
    PB = #unsubscribeincomingsmsreq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        subscription_id = SubscriptionID
    },
    Bin = k1api_pb:encode_unsubscribeincomingsmsreq(PB),
    {ok, Bin};

encode(DTO = #k1api_unsubscribe_incoming_sms_response_dto{}) ->
    #k1api_unsubscribe_incoming_sms_response_dto{
        id = ID
    } = DTO,
    PB = #unsubscribeincomingsmsresp{
        id = ID
    },
    Bin = k1api_pb:encode_unsubscribeincomingsmsresp(PB),
    {ok, Bin};

encode(DTO = #k1api_sms_notification_request_dto{}) ->
    #k1api_sms_notification_request_dto{
        callback_data = Callback,
        datetime = DateTime,
        dest_addr = DestAddr,
        message_id = MessageID,
        message = Message,
        sender_addr = SenderAddr,
        notify_url = NotifyURL
    } = DTO,
    PB = #smsnotificationreq{
        callback_data = Callback,
        datetime = date_to_pb(DateTime),
        dest_addr = addr_to_pb(DestAddr),
        message_id = MessageID,
        message = Message,
        sender_addr = addr_to_pb(SenderAddr),
        notify_url = NotifyURL
    },
    Bin = k1api_pb:encode_smsnotificationreq(PB),
    {ok, Bin};

encode(DTO = #k1api_auth_request_dto{}) ->
    #k1api_auth_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        password = Password,
        connection_type = ConnType
    } = DTO,
    PB = #authreq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        password = Password,
        connection_type = ConnType
    },
    Bin = k1api_pb:encode_authreq(PB),
    {ok, Bin};

encode(#k1api_auth_response_dto{
    id = ID,
    result = {customer, CustomerDTO}
}) ->
    #k1api_auth_response_customer_dto{
        id = SystemID,
        uuid = UUID,
        pay_type = PayType,
        allowed_sources = AllowedSources,
        default_source = DefaultSource,
        networks = Networks,
        providers = Providers,
        default_provider_id = DefProviderID,
        receipts_allowed = ReceiptsAllowed,
        no_retry = NoRetry,
        default_validity = DefValidity,
        max_validity = MaxValidity
    } = CustomerDTO,
    CustomerPB = #authresp_customer{
        id = SystemID,
        uuid = UUID,
        pay_type = PayType,
        allowed_sources = addr_to_pb(AllowedSources),
        default_source = addr_to_pb(DefaultSource),
        networks = network_dto_to_pb(Networks),
        providers = provider_dto_to_pb(Providers),
        default_provider_id = DefProviderID,
        receipts_allowed = ReceiptsAllowed,
        no_retry = NoRetry,
        default_validity = DefValidity,
        max_validity = MaxValidity
    },
    PB = #authresp{
        id = ID,
        result = customer,
        customer = CustomerPB
    },
    Bin = k1api_pb:encode_authresp(PB),
    {ok, Bin};
encode(#k1api_auth_response_dto{
    id = ID,
    result = {error, Message}
}) ->
    ErrorPB = #authresp_error{
        message = Message
    },
    PB = #authresp{
        id = ID,
        result = error,
        error = ErrorPB
    },
    Bin = k1api_pb:encode_authresp(PB),
    {ok, Bin};

encode(DTO = #k1api_subscribe_sms_receipts_request_dto{}) ->
    #k1api_subscribe_sms_receipts_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        url = Url,
        dest_addr = DestAddr,
        callback_data = Callback
    } = DTO,
    PB = #smsreceiptssubscribereq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        url = Url,
        dest_addr = addr_to_pb(DestAddr),
        callback_data = Callback
    },
    Bin = k1api_pb:encode_smsreceiptssubscribereq(PB),
    {ok, Bin};

encode(DTO = #k1api_subscribe_sms_receipts_response_dto{}) ->
    #k1api_subscribe_sms_receipts_response_dto{
        id = ID
    } = DTO,
    PB = #smsreceiptssubscriberesp{
        id = ID
    },
    Bin = k1api_pb:encode_smsreceiptssubscriberesp(PB),
    {ok, Bin};

encode(DTO = #k1api_unsubscribe_sms_receipts_request_dto{}) ->
    #k1api_unsubscribe_sms_receipts_request_dto{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        subscription_id = SubscriptionID
    } = DTO,
    PB = #smsreceiptsunsubscribereq{
        id = ID,
        customer_id = CustomerID,
        user_id = UserID,
        subscription_id = SubscriptionID
    },
    Bin = k1api_pb:encode_smsreceiptsunsubscribereq(PB),
    {ok, Bin};

encode(DTO = #k1api_unsubscribe_sms_receipts_response_dto{}) ->
    #k1api_unsubscribe_sms_receipts_response_dto{
        id = ID
    } = DTO,
    PB = #smsreceiptsunsubscriberesp{
        id = ID
    },
    Bin = k1api_pb:encode_smsreceiptsunsubscriberesp(PB),
    {ok, Bin};

encode(DTO = #k1api_sms_delivery_receipt_notification_dto{}) ->
    #k1api_sms_delivery_receipt_notification_dto{
        id = ID,
        dest_addr = DestAddr,
        status = Status,
        callback_data = CallbackData,
        url = Url
    } = DTO,
    PB = #smsdeliveryreceiptnotification{
        id = ID,
        dest_addr = addr_to_pb(DestAddr),
        status = status_name_dto_to_pb(Status),
        callback_data = CallbackData,
        url = Url
    },
    Bin = k1api_pb:encode_smsdeliveryreceiptnotification(PB),
    {ok, Bin};

encode(Message) ->
    erlang:error({k1api_encode_not_supported, Message}).


%% ===================================================================
%% Internal
%% ===================================================================

addr_to_pb(undefined) ->
    undefined;
addr_to_pb(AddrDTO = #addr{}) ->
    #addr{
        addr = Addr,
        ton = TON,
        npi = NPI
    } = AddrDTO,
    #fulladdr{
        addr = Addr,
        ton = TON,
        npi = NPI
    };
addr_to_pb(List) ->
    [addr_to_pb(Item) || Item <- List].

addr_pb_to_dto(undefined) ->
    undefined;
addr_pb_to_dto(AddrPB = #fulladdr{}) ->
    #fulladdr{
        addr = Addr,
        ton = TON,
        npi = NPI
    } = AddrPB,
    #addr{
        addr = Addr,
        ton = TON,
        npi = NPI
    };
addr_pb_to_dto(List) ->
    [addr_pb_to_dto(Item) || Item <- List].

sms_statuses_dto_to_pb(StatusDTO = #k1api_sms_status_dto{}) ->
    #k1api_sms_status_dto{
        address = Addr,
        status = Status
    } = StatusDTO,
    #smsstatus{
        address = addr_to_pb(Addr),
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


status_name_dto_to_pb(AtomStateName) when is_atom(AtomStateName) ->
    status_name_dto_to_pb(atom_to_binary(AtomStateName, utf8));
status_name_dto_to_pb(<<"sent">>) ->                        'SUBMITTED';
status_name_dto_to_pb(<<"pending">>) ->                     'SUCCESS_NO_DELIVERY';
status_name_dto_to_pb(<<"submitted">>) ->                   'SUBMITTED';
status_name_dto_to_pb(<<"success_waiting_delivery">>) ->    'SUCCESS_WAITING_DELIVERY';
status_name_dto_to_pb(<<"success_no_delivery">>) ->         'SUCCESS_NO_DELIVERY';
status_name_dto_to_pb(<<"failure">>) ->                     'FAILURE';
status_name_dto_to_pb(<<"enroute">>) ->                     'ENROUTE';
status_name_dto_to_pb(<<"delivered">>) ->                   'DELIVERED';
status_name_dto_to_pb(<<"expired">>) ->                     'EXPIRED';
status_name_dto_to_pb(<<"deleted">>) ->                     'DELETED';
status_name_dto_to_pb(<<"undeliverable">>) ->               'UNDELIVERABLE';
status_name_dto_to_pb(<<"accepted">>) ->                    'ACCEPTED';
status_name_dto_to_pb(<<"unknown">>) ->                     'UNKNOWN';
status_name_dto_to_pb(<<"rejected">>) ->                    'REJECTED';
status_name_dto_to_pb(<<"unrecognized">>) ->                'UNRECOGNIZED'.


status_name_pb_to_dto('SUBMITTED') ->                   <<"submitted">>;
status_name_pb_to_dto('SUCCESS_WAITING_DELIVERY') ->    <<"success_waiting_delivery">>;
status_name_pb_to_dto('SUCCESS_NO_DELIVERY') ->         <<"success_no_delivery">>;
status_name_pb_to_dto('FAILURE') ->                     <<"failure">>;
status_name_pb_to_dto('ENROUTE') ->                     <<"enroute">>;
status_name_pb_to_dto('DELIVERED') ->                   <<"delivered">>;
status_name_pb_to_dto('EXPIRED') ->                     <<"expired">>;
status_name_pb_to_dto('DELETED') ->                     <<"deleted">>;
status_name_pb_to_dto('UNDELIVERABLE') ->               <<"undeliverable">>;
status_name_pb_to_dto('ACCEPTED') ->                    <<"accepted">>;
status_name_pb_to_dto('UNKNOWN') ->                     <<"unknown">>;
status_name_pb_to_dto('REJECTED') ->                    <<"rejected">>;
status_name_pb_to_dto('UNRECOGNIZED') ->                <<"unrecognized">>.


retrieved_messages_to_pb(DTO = #k1api_retrieved_sms_dto{}) ->
    #k1api_retrieved_sms_dto{
        datetime = DateTime,
        sender_addr = DestAddr,
        message_id = MessageID,
        message = Message
    } = DTO,
    #retrievedmessage{
        datetime = date_to_pb(DateTime),
        sender_addr = addr_to_pb(DestAddr),
        message_id = MessageID,
        message = Message
    };
retrieved_messages_to_pb(MessagesDTO) ->
    [retrieved_messages_to_pb(DTO) || DTO <- MessagesDTO].

retrieved_messages_to_dto(PB = #retrievedmessage{}) ->
    #retrievedmessage{
        datetime = DateTime,
        sender_addr = DestAddr,
        message_id = MessageID,
        message = Message
    } = PB,
    #k1api_retrieved_sms_dto{
        datetime = date_to_dto(DateTime),
        sender_addr = addr_pb_to_dto(DestAddr),
        message_id = MessageID,
        message = Message
    };
retrieved_messages_to_dto(MessagesPB) ->
    [retrieved_messages_to_dto(PB) || PB <- MessagesPB].

%% ===================================================================
%% Network
%% ===================================================================

network_dto_to_pb(Network = #network_dto{}) ->
    #network_dto{
        id = ID,
        country_code = CountryCode,
        numbers_len = NumberLen,
        prefixes = Prefixes,
        provider_id = ProviderID
    } = Network,
    #network{
        id = ID,
        country_code = CountryCode,
        numbers_len = NumberLen,
        prefixes = Prefixes,
        provider_id = ProviderID
    };
network_dto_to_pb(List) ->
    [network_dto_to_pb(Item) || Item <- List].

network_pb_to_dto(Network = #network{}) ->
    #network{
        id = ID,
        country_code = CountryCode,
        numbers_len = NumberLen,
        prefixes = Prefixes,
        provider_id = ProviderID
    } = Network,
    #network_dto{
        id = ID,
        country_code = CountryCode,
        numbers_len = NumberLen,
        prefixes = Prefixes,
        provider_id = ProviderID
    };
network_pb_to_dto(List) ->
    [network_pb_to_dto(Item) || Item <- List].

%% ===================================================================
%% Provider
%% ===================================================================

provider_dto_to_pb(Provider = #provider_dto{}) ->
    #provider_dto{
        id = ID,
        gateway = GtwID,
        bulk_gateway = BuldGtwID,
        receipts_supported = Receipts
    } = Provider,
    #provider{
        id = ID,
        gateway = GtwID,
        bulk_gateway = BuldGtwID,
        receipts_supported = Receipts
    };
provider_dto_to_pb(List) ->
    [provider_dto_to_pb(Item) || Item <- List].

provider_pb_to_dto(Provider = #provider{}) ->
    #provider{
        id = ID,
        gateway = GtwID,
        bulk_gateway = BuldGtwID,
        receipts_supported = Receipts
    } = Provider,
    #provider_dto{
        id = ID,
        gateway = GtwID,
        bulk_gateway = BuldGtwID,
        receipts_supported = Receipts
    };
provider_pb_to_dto(List) ->
    [provider_pb_to_dto(Item) || Item <- List].

date_to_pb(TimeStamp) ->
    {{YY, MM, DD}, {H, M, S}} = calendar:now_to_universal_time(TimeStamp),
    ReferenceDate = {{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds({{YY, MM, DD}, {H, M, S}}) -
        calendar:datetime_to_gregorian_seconds(ReferenceDate).

date_to_dto(UTCUnixEpoch) ->
    MegaSecs = trunc(UTCUnixEpoch / 1000000),
    Secs = (UTCUnixEpoch - MegaSecs * 1000000),
    {MegaSecs, Secs, 0}.
