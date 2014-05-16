-ifndef(adto_types_hrl).
-define(adto_types_hrl, included).

-include("addr.hrl").

-type client_type_dto()  :: funnel | oneapi | soap | mm.
-type pay_type_dto()     :: prepaid | postpaid.
-type uuid_dto()         :: binary(). %% <<"12fd794d-9e32-4cf6...
-type utc_time()         :: binary(). %% <<"120827114232">>
-type smpp_type_dto()    :: receiver | transceiver | transmitter.

-record(error_dto, {
    error_code          :: integer(),
    timestamp           :: utc_time()
}).
-type error_dto()       :: #error_dto{}.

-record(network_dto, {
    id                  :: uuid_dto(),
    country_code        :: binary(), %% <<"375">>
    number_len          :: pos_integer(),
    prefixes            :: [binary()], %% [<<"44">>, <<"33">>]
    provider_id         :: uuid_dto(),

    name                :: binary(),
    country             :: binary(),
    gmt_diff            :: binary(),
    dst                 :: binary(),
    sms_cost            :: float()
}).
-type network_dto()     :: #network_dto{}.

-record(provider_dto, {
    id                  :: uuid_dto(),
    gateway_id          :: uuid_dto(),
    bulk_gateway_id     :: uuid_dto(),
    receipts_supported  :: boolean()
}).
-type provider_dto()    :: #provider_dto{}.

-record(blacklist_entry_dto, {
    id                  :: uuid_dto(),
    dst_addr            :: addr(),
    src_addr            :: undefined | addr()
}).
-type blacklist_entry_dto() :: #blacklist_entry_dto{}.

-endif. % adto_types_hrl
