# Exporting your multi-file lua project using [amalg](https://github.com/siffiejoe/lua-amalg)

`$ luarocks install --local std.strict`

`$ luarocks install --local amalg`

`$ ~/.luarocks/bin/amalg.lua -o exported_game.lua middleclass std.strict -s game.lua`

`$ tic80 --fs .`

`tic-80:~$ load exported_game.lua`

`tic-80:~$ export html remake.zip`
