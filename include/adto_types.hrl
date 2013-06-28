-ifndef(adto_types_hrl).
-define(adto_types_hrl, included).

-include("addr.hrl").

-type client_type_dto()	 :: funnel | k1api.
-type billing_type_dto() :: prepaid | postpaid.
-type uuid_dto() 		 :: binary(). %% <<"12fd794d-9e32-4cf6...
-type utc_time_dto()	 :: binary(). %% <<"120827114232">>
-type bind_type_dto()	 :: receiver | transceiver | transmitter.

-record(error_dto, {
	error_code	 		:: integer(),
	timestamp 			:: utc_time_dto()
}).
-type error_dto() 		:: #error_dto{}.

-record(network_dto, {
	id 					:: uuid_dto(),
	country_code 		:: binary(), %% <<"375">>
	numbers_len 		:: integer(),
	prefixes 			:: [binary()], %% [<<"44">>, <<"33">>]
	provider_id 		:: uuid_dto()
}).
-type network_dto() 	:: #network_dto{}.

-record(provider_dto, {
	id 					:: uuid_dto(),
	gateway 			:: uuid_dto(),
	bulk_gateway 		:: uuid_dto(),
	receipts_supported 	:: boolean()
}).
-type provider_dto() 	:: #provider_dto{}.

-endif. % adto_types_hrl
