-ifndef(adto_hrl).
-define(adto_hrl, included).

-include("just_dto.hrl").
-include("funnel_dto.hrl").
-include("k1api_dto.hrl").

-type message_type_dto() ::
    k1api_dto() |
    just_dto() |
    funnel_dto().

-endif. % adto_hrl
