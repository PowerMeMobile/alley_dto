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
decode(#funnel_client_offline_event_dto{}, Message) ->
	case 'FunnelAsn':decode('ConnectionDownEvent', Message) of
		{ok, #'ConnectionDownEvent'{
			connectionId = ConnectionID,
			customerId = CustomerID,
			userId = UserID }} ->
			DTO = #funnel_client_offline_event_dto{
				connection_id = adto_uuid:to_binary(ConnectionID),
				customer_id = list_to_binary(CustomerID),
				user_id = list_to_binary(UserID)
			},
			{ok, DTO};
		{error, Error} ->
			{error, Error}
	end;

decode(#funnel_client_online_event_dto{}, Message) ->
	case 'FunnelAsn':decode('ConnectionUpEvent', Message) of
		{ok, #'ConnectionUpEvent'{
			connectionId = ConnectionID,
			customerId = CustomerID,
			userId = UserID,
			type = Type }} ->
			DTO = #funnel_client_online_event_dto{
				connection_id = adto_uuid:to_binary(ConnectionID),
				customer_id = list_to_binary(CustomerID),
				user_id = list_to_binary(UserID),
				type = Type
			},
			{ok, DTO};
		{error, Error} ->
			{error, Error}
	end;

decode(#funnel_auth_request_dto{}, Message) ->
	case 'FunnelAsn':decode('BindRequest', Message) of
		{ok,#'BindRequest'{
			connectionId = ConnectionID,
			customerId = CustomerID,
			userId = UserID,
			password = Password,
			type = Type }} ->
			DTO = #funnel_auth_request_dto{
				connection_id = adto_uuid:to_binary(ConnectionID),
				customer_id = list_to_binary(CustomerID),
				user_id = list_to_binary(UserID),
				password = list_to_binary(Password),
				type = Type
			},
			{ok, DTO};
		{error, Error} ->
			{error, Error}
	end;

decode(_Type, _Message) ->
	%% ?log_fatal("HALT. Unexpected decode message type: ~p", [Type]),
	erlang:halt().

%% ===================================================================
%% Encode Functions
%% ===================================================================

-spec encode(message_type_dto()) ->
	{ok, Payload :: binary()} |
	{error, Reason :: any()}.
encode(DTO = #funnel_auth_response_dto{result = {error, Error}}) ->
	#funnel_auth_response_dto{
		connection_id = ConnectionID,
		result = Result
	} = DTO,
	Asn = #'BindResponse'{
		connectionId = adto_uuid:to_string(ConnectionID),
		result = Result
	},
	case 'FunnelAsn':encode('BindResponse', Asn) of
		{ok, List} ->
			{ok, list_to_binary(List)};
		{error, Error} ->
			{error, Error}
	end;

encode(DTO = #funnel_auth_response_dto{result = {customer, _}}) ->
	#funnel_auth_response_dto{
		connection_id = ConnectionID,
		result = {customer, CustomerDTO}
	} = DTO,
	#funnel_auth_response_customer_dto{
		  id = ID,
		  uuid = UUID,
		  priority = Priority,
		  rps = RPS,
		  allowed_sources = AllowedSources,
		  default_source = DefaultSource,
		  networks = Networks,
		  providers = Providers,
		  default_provider_id = DefaultProvideID,
		  receipts_allowed  = ReceiptsAllowed,
		  no_retry  = NoRetry,
		  default_validity  = DefaultValidity,
		  max_validity = MaxValidity
	} = CustomerDTO,

	CustomerASN = #'Customer'{
		id = ?bin_to_str(ID),
		uuid = ?bin_uuid_to_str(UUID),
		priority = Priority,
		rps = validate_optional_asn_value(RPS),
		allowedSources = ?msisdn_addrs_to_str(AllowedSources),
		defaultSource = validate_optional_asn_value(?msisdn_addr_to_str(DefaultSource)),
		networks = convert_network_fields(Networks),
		providers = convert_provider_fields(Providers),
		defaultProviderId = validate_optional_asn_value(DefaultProvideID),
		receiptsAllowed = ReceiptsAllowed,
		noRetry = NoRetry,
		defaultValidity = ?bin_to_str(DefaultValidity),
		maxValidity = MaxValidity
	},
	Asn = #'BindResponse'{
		connectionId = ConnectionID,
		result = {customer, CustomerASN}
	},
	case 'FunnelAsn':encode('BindResponse', Asn) of
		{ok, List} ->
			{ok, list_to_binary(List)};
		{error, Error} ->
			{error, Error}
	end;

encode(DTO = #funnel_incoming_sms_dto{}) ->
	#funnel_incoming_sms_dto{
		id = ID,
		source = SourceAddr,
		dest = DestAddr,
		message = MessageBody,
		datacoding = DataCoding
	} = DTO,
	Msg = #'OutgoingMessage'{
		source = ?msisdn_addr_to_str(SourceAddr),
		dest = ?msisdn_addr_to_str(DestAddr),
		dataCoding = DataCoding,
		message = MessageBody
	},
	Batch = #'OutgoingBatch'{
		id = ID,
		messages = [Msg]
	},
	case 'FunnelAsn':encode('OutgoingBatch', Batch) of
		{ok, List} ->
			{ok, list_to_binary(List)};
		{error, Error} ->
			{error, Error}
	end;


encode(_Message) ->
	%% ?log_fatal("HALT. Unexpected encode message type: ~p", [Message]),
	erlang:halt().

%% ===================================================================
%% Local Functions
%% ===================================================================

validate_optional_asn_value(OptionValue) ->
	case OptionValue of
			undefined ->
				asn1_NOVALUE;
			Value ->
				Value
	end.

convert_network_fields(Networks) when is_list(Networks) ->
	[convert_network_fields(Network) || Network <- Networks];
convert_network_fields(Network = #network_dto{}) ->
	#network_dto{
		id = ID,
		country_code = CC,
		numbers_len = NL,
		prefixes = Pref,
		provider_id = ProviderId
		} = Network,
	#'Network'{
		id = ?bin_uuid_to_str(ID),
		countryCode = ?bin_to_str(CC),
		numbersLen = NL,
		prefixes = ?bins_to_strs(Pref),
		providerId = ?bin_uuid_to_str(ProviderId)
	}.

convert_provider_fields(Providers) when is_list(Providers) ->
	[convert_provider_fields(Provider) || Provider <- Providers];
convert_provider_fields(Provider = #provider_dto{}) ->
	#provider_dto{
		id = ID,
		gateway = Gateway,
		bulk_gateway = BGateway,
		receipts_supported = RS
	} = Provider,
	#'Provider'{
		id = ?bin_uuid_to_str(ID),
		gateway = ?bin_uuid_to_str(Gateway),
		bulkGateway = ?bin_uuid_to_str(BGateway),
		receiptsSupported = RS
	}.
