-ifndef(addr_hrl).
-define(addr_hrl, included).

-record(addr, {
	addr 		:: binary(),
	ton 		:: integer(),
	npi 		:: integer(),
	ref_num		:: integer()
}).
-type addr()	:: #addr{}.

-endif. % addr_hrl
