-module(adto_tests).

-include("adto.hrl").
-include_lib("eunit/include/eunit.hrl").

unsupported_type_test() ->
    BadDTO = unknown_type,
    ?assertError({unknown_adto_type,unknown_type} , adto:encode(BadDTO)).
