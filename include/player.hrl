-record(player, {
    version = player_version:cur_version(),
    id = 0,
    name = <<""/utf8>>,
    items = [],
    float_test = 0.0,
    base_attr = {},   %% from base_attr.hrl
    test = 0.0,
    lv = 1
}).
