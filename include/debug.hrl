%%%=========================================================================
%%%                                 LICENSE
%%%=========================================================================
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU Library General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%=========================================================================
%%% 
%%% This header handles whether to output debug messages or not, and how.
%%%

-ifndef(debug_hrl).

-define(debug_hrl,[]).


-ifdef(newdebug).

-define(DEBL(L,X,Y),
    (fun({__Level,__Module}) when is_integer(__Level)->
            case __Level of 
                1 ->
                    gen_server:cast(newdebug1,{input,__Module,?LINE,self(),X,Y});
                2 ->
                    gen_server:cast(newdebug2,{input,__Module,?LINE,self(),X,Y});
                3 ->
                    gen_server:cast(newdebug3,{input,__Module,?LINE,self(),X,Y});
                4 ->
                    gen_server:cast(newdebug4,{input,__Module,?LINE,self(),X,Y});
                5 ->
                    gen_server:cast(newdebug5,{input,__Module,?LINE,self(),X,Y});
                6 ->
                    gen_server:cast(newdebug6,{input,__Module,?LINE,self(),X,Y});
                7 ->
                    gen_server:cast(newdebug7,{input,__Module,?LINE,self(),X,Y});
                8 ->
                    gen_server:cast(newdebug8,{input,__Module,?LINE,self(),X,Y});
                9 ->
                    gen_server:cast(newdebug9,{input,__Module,?LINE,self(),X,Y});
                10 ->
                    gen_server:cast(newdebug10,{input,__Module,?LINE,self(),X,Y})
            end;
        ({__Module,__Level}) when is_integer(__Level)->
            case __Level of 
                1 ->
                    gen_server:cast(newdebug1,{input,__Module,?LINE,self(),X,Y});
                2 ->
                    gen_server:cast(newdebug2,{input,__Module,?LINE,self(),X,Y});
                3 ->
                    gen_server:cast(newdebug3,{input,__Module,?LINE,self(),X,Y});
                4 ->
                    gen_server:cast(newdebug4,{input,__Module,?LINE,self(),X,Y});
                5 ->
                    gen_server:cast(newdebug5,{input,__Module,?LINE,self(),X,Y});
                6 ->
                    gen_server:cast(newdebug6,{input,__Module,?LINE,self(),X,Y});
                7 ->
                    gen_server:cast(newdebug7,{input,__Module,?LINE,self(),X,Y});
                8 ->
                    gen_server:cast(newdebug8,{input,__Module,?LINE,self(),X,Y});
                9 ->
                    gen_server:cast(newdebug9,{input,__Module,?LINE,self(),X,Y});
                10 ->
                    gen_server:cast(newdebug10,{input,__Module,?LINE,self(),X,Y})
            end;
        (err) ->
                    gen_server:cast(newdebugerr,{input,?MODULE,?LINE,self(),X,Y});
        ({__Module,err}) ->
                    gen_server:cast(newdebugerr,{input,__Module,?LINE,self(),X,Y});
        ({err,__Module}) ->
                    gen_server:cast(newdebugerr,{input,__Module,?LINE,self(),X,Y});
        (__Level) ->
            case __Level of 
                1 ->
                    gen_server:cast(newdebug1,{input,?MODULE,?LINE,self(),X,Y});
                2 ->
                    gen_server:cast(newdebug2,{input,?MODULE,?LINE,self(),X,Y});
                3 ->
                    gen_server:cast(newdebug3,{input,?MODULE,?LINE,self(),X,Y});
                4 ->
                    gen_server:cast(newdebug4,{input,?MODULE,?LINE,self(),X,Y});
                5 ->
                    gen_server:cast(newdebug5,{input,?MODULE,?LINE,self(),X,Y});
                6 ->
                    gen_server:cast(newdebug6,{input,?MODULE,?LINE,self(),X,Y});
                7 ->
                    gen_server:cast(newdebug7,{input,?MODULE,?LINE,self(),X,Y});
                8 ->
                    gen_server:cast(newdebug8,{input,?MODULE,?LINE,self(),X,Y});
                9 ->
                    gen_server:cast(newdebug9,{input,?MODULE,?LINE,self(),X,Y});
                10 ->
                    gen_server:cast(newdebug10,{input,?MODULE,?LINE,self(),X,Y})
            end
    end)(L)).


-else.


-ifdef(debug).

-ifdef(timestamp).
-define(F2,debug:timestamp).
-else.
-define(F2,debug:msg).
-endif.

-define(DEBL(L,X,Y),?F2(L,?MODULE,?LINE,X,Y)).

-else.

-define(DEBL(L,X,Y),ok).

-endif.

-endif.

-define(DEB1(L,X),?DEBL(L,X,[])).
-define(DEB2(L,X,Y),?DEBL(L,X,[Y])).

-endif.
