
-export([hvt/2]).

hvt(queue,Wait) ->
 receive
  {something} -> io:format("Something in queue")
 after 0 when Wait == 1 -> 
   io:format("test~n");
   when Wait == 0 ->
   io:format("test2")
 end.
