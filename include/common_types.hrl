-ifndef(common_types_hrl).
-define(common_types_hrl, included).

-type interface()  :: transmitter
                    | receiver
                    | tranceiver
                    | funnel
                    | soap
                    | mm
                    | webmm
                    | email
                    | oneapi.
-type pay_type()   :: prepaid | postpaid.
-type uuid()       :: binary(). %% <<"12fd794d-9e32-4cf6...
-type utc_time()   :: binary(). %% <<"120827114232">>
-type smpp_type()  :: receiver | transceiver | transmitter.

-endif. % common_types_hrl
