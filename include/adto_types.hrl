-ifndef(adto_types_hrl).
-define(adto_types_hrl, included).

-include("addr.hrl").
-include("common_types.hrl").

-record(error_dto, {
    error_code          :: integer(),
    timestamp           :: utc_time()
}).
-type error_dto()       :: #error_dto{}.

-record(network_dto, {
    id                  :: uuid(),
    country_code        :: binary(), %% <<"375">>
    number_len          :: pos_integer(),
    prefixes            :: [binary()], %% [<<"44">>, <<"33">>]
    provider_id         :: uuid(),
    is_home             :: boolean(),
    sms_points          :: float(),
    sms_mult_points     :: float(),

    name                :: binary(),
    country             :: binary(),
    gmt_diff            :: binary(),
    dst                 :: binary()
}).
-type network_dto()     :: #network_dto{}.

-record(provider_dto, {
    id                  :: uuid(),
    gateway_id          :: uuid(),
    bulk_gateway_id     :: uuid(),
    receipts_supported  :: boolean(),
    sms_add_points      :: float()
}).
-type provider_dto()    :: #provider_dto{}.

-record(feature_dto, {
    name                :: binary(),
    value               :: binary()
}).
-type feature_dto()     :: #feature_dto{}.

-endif. % adto_types_hrl
