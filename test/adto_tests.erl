-module(adto_tests).

-include("adto.hrl").
-include_lib("eunit/include/eunit.hrl").

unsupported_type_test() ->
	BadDTO = unsupported_type,
	?assertError({adto_unsupported_type,unsupported_type} , adto:encode(BadDTO)).
