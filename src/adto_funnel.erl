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

decode(#funnel_auth_response_dto{}, Bin) ->
	case 'FunnelAsn':decode('BindResponse', Bin) of
		{ok, Asn} ->
			#'BindResponse'{
				connectionId = ID,
				result = ResultAsn
			} = Asn,
			DTO = #funnel_auth_response_dto{
				connection_id = adto_uuid:to_binary(ID),
				result = funnel_auth_response_result_to_dto(ResultAsn)
			},
			{ok, DTO};
		{error, Error} ->
			{error, Error}
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
		max_validity = MaxValidity
	} = CustomerDTO,
	CustomerAsn = #'Customer'{
		id = binary_to_list(SystemID),
		uuid = adto_uuid:to_string(UUID),
		priority = Priority,
		rps = to_optional_asn(RPS),
		allowedSources = [addr_to_asn(Source) || Source <- AllowedSources],
		defaultSource = to_optional_asn(DefaultSource, fun addr_to_asn/1),
		networks = networks_to_asn(Networks),
		providers = providers_to_asn(Providers),
		defaultProviderId = to_optional_asn(DefaultProviderID, fun adto_uuid:to_string/1),
		receiptsAllowed = ReceiptsAllowed,
		noRetry = NoRetry,
		defaultValidity = binary_to_list(DefaultValidity),
		maxValidity = MaxValidity
	},
	Asn = #'BindResponse'{
		connectionId = adto_uuid:to_string(ConnectionID),
		result = {customer, CustomerAsn}
	},
	case 'FunnelAsn':encode('BindResponse', Asn) of
		{ok, DeepList} -> {ok, list_to_binary(DeepList)};
		{error, Error} -> {error, Error}
	end;

encode(#funnel_auth_response_dto{result = {error, _}}) ->
	erlang:error({funnel_encode_not_implemented, funnel_auth_response_dto});

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

addr_to_asn(FullAddr = #addr_dto{}) ->
	#addr_dto{
		addr = Addr,
		ton = TON,
		npi = NPI
	} = FullAddr,
	#'Addr'{
		addr = binary_to_list(Addr),
		ton = TON,
		npi = NPI
	}.

addr_to_dto(FullAddr = #'Addr'{}) ->
	#'Addr'{
		addr = Addr,
		ton = TON,
		npi = NPI
	} = FullAddr,
	#addr_dto{
		addr = list_to_binary(Addr),
		ton = TON,
		npi = NPI
	}.

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
		id = adto_uuid:to_string(ID),
		countryCode = binary_to_list(CountryCode),
		numbersLen = NumbersLength,
		prefixes = [binary_to_list(Prefix) || Prefix <- Prefixes],
		providerId = adto_uuid:to_string(ProviderID)
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
		id = adto_uuid:to_binary(ID),
		country_code = list_to_binary(CountryCode),
		numbers_len = NumbersLength,
		prefixes = [list_to_binary(Prefix) || Prefix <- Prefixes],
		provider_id = adto_uuid:to_binary(ProviderID)
	};
networks_to_dto(Networks) ->
	[networks_to_dto(Network) || Network <- Networks].

%% Providers

providers_to_asn(Provider = #provider_dto{}) ->
	#provider_dto{
		id = ID,
		gateway = GtwID,
		bulk_gateway = BulkGtwID,
		receipts_supported = ReceiptsSupported
	} = Provider,
	#'Provider'{
		id = adto_uuid:to_string(ID),
		gateway = adto_uuid:to_string(GtwID),
		bulkGateway = adto_uuid:to_string(BulkGtwID),
		receiptsSupported = ReceiptsSupported
	};
providers_to_asn(Providers) ->
	[providers_to_asn(Provider) || Provider <- Providers].

providers_to_dto(Provider = #'Provider'{}) ->
	#'Provider'{
		id = ID,
		gateway = GtwID,
		bulkGateway = BulkGtwID,
		receiptsSupported = ReceiptsSupported
	} = Provider,
	#provider_dto{
		id = adto_uuid:to_binary(ID),
		gateway = adto_uuid:to_binary(GtwID),
		bulk_gateway = adto_uuid:to_binary(BulkGtwID),
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
		maxValidity = MaxValidity
	} = CustomerAsn,
	CustomerDTO = #funnel_auth_response_customer_dto{
		id = list_to_binary(SystemID),
		uuid = adto_uuid:to_binary(UUID),
		priority = Priority,
		rps = from_optional_asn(RPS),
		allowed_sources = [addr_to_dto(Source) || Source <- AllowedSources],
		default_source = from_optional_asn(DefaultSource, fun addr_to_dto/1),
		networks = networks_to_dto(Networks),
		providers = providers_to_dto(Providers),
		default_provider_id = from_optional_asn(DefaultProviderID, fun adto_uuid:to_binary/1),
		receipts_allowed = ReceiptsAllowed,
		no_retry = NoRetry,
		default_validity = list_to_binary(DefaultValidity),
		max_validity = MaxValidity
	},
	{customer, CustomerDTO};

funnel_auth_response_result_to_dto({error, Error}) ->
	{error, list_to_binary(Error)}.
