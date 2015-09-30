-ifndef(adto_hrl).
-define(adto_hrl, included).

-include("just_dto.hrl").
-include("funnel_dto.hrl").
-include("common_dto.hrl").

-type message_type_dto() ::
    just_dto()   |
    funnel_dto() |
    common_dto().

-endif. % adto_hrl
