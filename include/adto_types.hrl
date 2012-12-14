-ifndef(adto_types_hrl).
-define(adto_types_hrl, included).

-include("addr.hrl").

-type client_type() 	:: k1api | funnel.
-type billing_type_() 	:: prepaid | postpaid.
-type uuid_() 			:: binary(). %% <<"12fd794d-9e32-4cf6...
-type utc_time()	 	:: binary(). %% <<"120827114232">>
-type smpp_type_dto() 	:: receiver | transceiver | transmitter.

-record(error_dto, {
	error_code	 		:: integer(),
	timestamp 			:: utc_time()
}).
-type error_dto() 		:: #error_dto{}.

-record(network_dto, {
	id 					:: uuid_(),
	country_code 		:: binary(), %% <<"375">>
	numbers_len 		:: integer(),
	prefixes 			:: [binary()], %% [<<"44">>, <<"33">>]
	provider_id 		:: uuid_()
}).
-type network_dto() 	:: #network_dto{}.

-record(provider_dto, {
	id 					:: uuid_(),
	gateway 			:: uuid_(),
	bulk_gateway 		:: uuid_(),
	receipts_supported 	:: boolean()
}).
-type provider_dto() 	:: #provider_dto{}.


-endif. % adto_types_hrl
