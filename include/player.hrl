-record(player, {
    version = player_table:cur_version(),
    id = 0,
    name = <<""/utf8>>,
    items = [],
    float_test = 0.0,
    lv = 1
}).
