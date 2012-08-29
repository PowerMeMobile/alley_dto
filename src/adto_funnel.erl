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
				connection_id = adto_uuid:to_binary(ConnectionID),
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
		connectionId = adto_uuid:to_string(ConnectionID),
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

encode(Message) ->
	erlang:error({funnel_encode_not_supported, Message}).

%% ===================================================================
%% Local Functions
%% ===================================================================

%% PreciseTime

precise_time_to_asn(DTO = #precise_time_dto{}) ->
	#precise_time_dto{
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
	#precise_time_dto{
		time = list_to_binary(Time),
		milliseconds = Milliseconds
	}.

%% validate_optional_asn_value(OptionValue) ->
%% 	case OptionValue of
%% 			undefined ->
%% 				asn1_NOVALUE;
%% 			Value ->
%% 				Value
%% 	end.

%% convert_network_fields(Networks) when is_list(Networks) ->
%% 	[convert_network_fields(Network) || Network <- Networks];
%% convert_network_fields(Network = #network_dto{}) ->
%% 	#network_dto{
%% 		id = ID,
%% 		country_code = CC,
%% 		numbers_len = NL,
%% 		prefixes = Pref,
%% 		provider_id = ProviderId
%% 		} = Network,
%% 	#'Network'{
%% 		id = ?bin_uuid_to_str(ID),
%% 		countryCode = ?bin_to_str(CC),
%% 		numbersLen = NL,
%% 		prefixes = ?bins_to_strs(Pref),
%% 		providerId = ?bin_uuid_to_str(ProviderId)
%% 	}.

%% convert_provider_fields(Providers) when is_list(Providers) ->
%% 	[convert_provider_fields(Provider) || Provider <- Providers];
%% convert_provider_fields(Provider = #provider_dto{}) ->
%% 	#provider_dto{
%% 		id = ID,
%% 		gateway = Gateway,
%% 		bulk_gateway = BGateway,
%% 		receipts_supported = RS
%% 	} = Provider,
%% 	#'Provider'{
%% 		id = ?bin_uuid_to_str(ID),
%% 		gateway = ?bin_uuid_to_str(Gateway),
%% 		bulkGateway = ?bin_uuid_to_str(BGateway),
%% 		receiptsSupported = RS
%% 	}.
