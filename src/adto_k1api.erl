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
        status = Status,
        callback_data = CallbackData,
        url = Url
    },
    {ok, DTO};

decode(#k1api_process_inbox_request_dto{}, Bin) ->
    PB = k1api_pb:decode_processinboxreq(Bin),
    #processinboxreq{
        id = Id,
        customer_id = CustomerId,
        user_id = UserId,
        operation = Operation,
        message_ids = MessageIds
    } = PB,
    DTO = #k1api_process_inbox_request_dto{
        id = Id,
        customer_id = CustomerId,
        user_id = UserId,
        operation = Operation,
        message_ids = MessageIds
    },
    {ok, DTO};

decode(#k1api_process_inbox_response_dto{}, Bin) ->
    PB = k1api_pb:decode_processinboxresp(Bin),
    #processinboxresp{
        id = Id,
        result = Result,
        messages = Messages,
        deleted = Deleted,
        error = Error
    } = PB,
    DTO = case Result of
        messages ->
            #k1api_process_inbox_response_dto{
                id = Id,
                result = {messages, [inbox_message_pb_to_dto(M) || M <- Messages]}
            };
        deleted ->
            #k1api_process_inbox_response_dto{
                id = Id,
                result = {deleted, Deleted}
            };
        error ->
            #processinboxresp_error{
                message = Message
            } = Error,
            #k1api_process_inbox_response_dto{
                id = Id,
                result = {error, Message}
            }
    end,
    {ok, DTO};

decode(Type, _Message) ->
    erlang:error({k1api_decode_not_supported, Type}).

%% ===================================================================
%% Encode Functions
%% ===================================================================

-spec encode(message_type_dto()) ->
    {ok, Payload :: binary()} |
    {error, Reason :: any()}.

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
        dest_addr = addr_dto_to_pb(DestAddr),
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
        dest_addr = addr_dto_to_pb(DestAddr),
        message_id = MessageID,
        message = Message,
        sender_addr = addr_dto_to_pb(SenderAddr),
        notify_url = NotifyURL
    },
    Bin = k1api_pb:encode_smsnotificationreq(PB),
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
        dest_addr = addr_dto_to_pb(DestAddr),
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
        dest_addr = addr_dto_to_pb(DestAddr),
        status = Status,
        callback_data = CallbackData,
        url = Url
    },
    Bin = k1api_pb:encode_smsdeliveryreceiptnotification(PB),
    {ok, Bin};

encode(DTO = #k1api_process_inbox_request_dto{}) ->
    #k1api_process_inbox_request_dto{
        id = Id,
        customer_id = CustomerId,
        user_id = UserId,
        operation = Operation,
        message_ids = MessageIds
    } = DTO,
    PB = #processinboxreq{
        id = Id,
        customer_id = CustomerId,
        user_id = UserId,
        operation = Operation,
        message_ids = MessageIds
    },
    Bin = k1api_pb:encode_processinboxreq(PB),
    {ok, Bin};

encode(DTO = #k1api_process_inbox_response_dto{}) ->
    #k1api_process_inbox_response_dto{
        id = Id,
        result = Result
    } = DTO,
    PB = case Result of
        {messages, Messages} ->
            #processinboxresp{
                id = Id,
                result = messages,
                messages = [inbox_message_dto_to_pb(M) || M <- Messages]
            };
        {deleted, Deleted} ->
            #processinboxresp{
                id = Id,
                result = deleted,
                deleted = Deleted
            };
        {error, Error} ->
            #processinboxresp{
                id = Id,
                result = error,
                error = #processinboxresp_error{
                    message = Error
                }
            }
    end,
    Bin = k1api_pb:encode_processinboxresp(PB),
    {ok, Bin};

encode(Message) ->
    erlang:error({k1api_encode_not_supported, Message}).

%% ===================================================================
%% Internal
%% ===================================================================

addr_dto_to_pb(undefined) ->
    undefined;
addr_dto_to_pb(AddrDTO = #addr{}) ->
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
addr_dto_to_pb(List) ->
    [addr_dto_to_pb(Item) || Item <- List].

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

%% ===================================================================
%% Date
%% ===================================================================

date_to_pb(TimeStamp) ->
    {{YY, MM, DD}, {H, M, S}} = calendar:now_to_universal_time(TimeStamp),
    ReferenceDate = {{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds({{YY, MM, DD}, {H, M, S}}) -
        calendar:datetime_to_gregorian_seconds(ReferenceDate).

date_to_dto(UTCUnixEpoch) ->
    MegaSecs = trunc(UTCUnixEpoch / 1000000),
    Secs = (UTCUnixEpoch - MegaSecs * 1000000),
    {MegaSecs, Secs, 0}.

%% ===================================================================
%% Inbox
%% ===================================================================

inbox_message_pb_to_dto(Msg = #processinboxresp_message{}) ->
    #processinboxresp_message{
        id = Id,
        new = New,
        from = From,
        to = To,
        timestamp = Timestamp,
        size = Size,
        text = Text
    } = Msg,
    #k1api_process_inbox_response_message_dto{
        id = Id,
        new = New,
        from = addr_pb_to_dto(From),
        to = addr_pb_to_dto(To),
        timestamp = date_to_dto(Timestamp),
        size = Size,
        text = Text
    }.

inbox_message_dto_to_pb(Msg = #k1api_process_inbox_response_message_dto{}) ->
    #k1api_process_inbox_response_message_dto{
        id = Id,
        new = New,
        from = From,
        to = To,
        timestamp = Timestamp,
        size = Size,
        text = Text
    } = Msg,
    #processinboxresp_message{
        id = Id,
        new = New,
        from = addr_dto_to_pb(From),
        to = addr_dto_to_pb(To),
        timestamp = date_to_pb(Timestamp),
        size = Size,
        text = Text
    }.
