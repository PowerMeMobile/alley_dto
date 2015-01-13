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

decode(Type, Bin) ->
    bin_to_term(Type, Bin).

%% ===================================================================
%% Encode Functions
%% ===================================================================

-spec encode(message_type_dto()) ->
    {ok, Payload::binary()} |
    {error, Reason::any()}.

encode(DTO) ->
    Bin = term_to_binary(DTO),
    {ok, Bin}.

%% ===================================================================
%% Internal
%% ===================================================================

bin_to_term(Rec, Bin) ->
    RecName = element(1, Rec),
    RecSize = size(Rec),
    try binary_to_term(Bin) of
        DTO when is_tuple(DTO) andalso
                 element(1, DTO) =:= RecName andalso
                 size(DTO) =:= RecSize ->
            {ok, DTO};
        BadDTO ->
            {error, {incorrect_rec, BadDTO}}
    catch
        error:badarg ->
            {error, {invalid_bin, Bin}}
    end.
