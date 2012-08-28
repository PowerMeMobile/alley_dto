-ifndef(adto_helpers_hrl).
-define(adto_helpers_hrl, included).

%% converts address field in addr dto (msisdn) from binary to string
-define(msisdn_addr_to_str(Addr),
	apply(fun() ->
		case Addr of
			undefined -> undefined;
			_ ->
				NewAddr = binary_to_list(Addr#addr_dto.addr),
				Addr#addr_dto{addr = NewAddr}
		end
	 end, [])).

%% excutes above script to list of addr dto
-define(msisdn_addrs_to_str(Addrs),
	apply(fun()->
		lists:map(fun(Addr) -> ?msisdn_addr_to_str(Addr) end, Addrs)
	end, [])).

%% converts binary uuid representation to pritty string
-define(bin_uuid_to_str(UUID),
	apply(fun() ->
		adto_uuid:to_string(UUID)
	end, [])).

%% converts binary string representation to list
-define(bin_to_str(Bin),
	apply(fun() ->
		binary_to_list(Bin)
	end, [])).

%% executes above script to list of binaries
-define(bins_to_strs(List),
	apply(fun() ->
		lists:map(fun(Element) -> ?bin_to_str(Element) end, List)
	end, [])).

-endif. % adto_helpers_hrl
