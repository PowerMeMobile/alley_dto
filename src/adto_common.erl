-module(adto_common).

-export([
    encode/1,
    decode/2
]).

-include("adto.hrl").

%% ===================================================================
%% Decode Functions
%% ===================================================================

-spec decode(message_type_dto(), binary()) ->
    {ok, message_type_dto()} |
    {error, Reason::any()}.

decode(#auth_req_v1{}, Bin) ->
    DTO = binary_to_term(Bin),
    {ok, DTO};

decode(#auth_resp_v1{}, Bin) ->
    DTO = binary_to_term(Bin),
    {ok, DTO};

decode(#sms_status_req_v1{}, Bin) ->
    DTO = binary_to_term(Bin),
    {ok, DTO};

decode(#sms_status_resp_v1{}, Bin) ->
    DTO = binary_to_term(Bin),
    {ok, DTO};

decode(#credit_req_v1{}, Bin) ->
    DTO = binary_to_term(Bin),
    {ok, DTO};

decode(#credit_resp_v1{}, Bin) ->
    DTO = binary_to_term(Bin),
    {ok, DTO};

decode(#blacklist_req_v1{}, Bin) ->
    DTO = binary_to_term(Bin),
    {ok, DTO};

decode(#blacklist_resp_v1{}, Bin) ->
    DTO = binary_to_term(Bin),
    {ok, DTO};

decode(Type, _Message) ->
    erlang:error({unknown_decode_common_type, Type}).

%% ===================================================================
%% Encode Functions
%% ===================================================================

-spec encode(message_type_dto()) ->
    {ok, Payload::binary()} |
    {error, Reason::any()}.

encode(DTO = #auth_req_v1{}) ->
    Bin = term_to_binary(DTO),
    {ok, Bin};

encode(DTO = #auth_resp_v1{}) ->
    Bin = term_to_binary(DTO),
    {ok, Bin};

encode(DTO = #sms_status_req_v1{}) ->
    Bin = term_to_binary(DTO),
    {ok, Bin};

encode(DTO = #sms_status_resp_v1{}) ->
    Bin = term_to_binary(DTO),
    {ok, Bin};

encode(DTO = #credit_req_v1{}) ->
    Bin = term_to_binary(DTO),
    {ok, Bin};

encode(DTO = #credit_resp_v1{}) ->
    Bin = term_to_binary(DTO),
    {ok, Bin};

encode(DTO = #blacklist_req_v1{}) ->
    Bin = term_to_binary(DTO),
    {ok, Bin};

encode(DTO = #blacklist_resp_v1{}) ->
    Bin = term_to_binary(DTO),
    {ok, Bin};

encode(DTO) ->
    erlang:error({unknown_encode_common_type, DTO}).
