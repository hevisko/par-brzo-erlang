%% Author: hendrivi
%% Created: Jun 21, 2010
%% Description: TODO: Add description to hvt
-module(hvt).
-compile(export_all).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

test() ->
	receive
		after 1000 ->true
						  end,
	receive 
		stop -> io:write(firstStop),exit(0)
	after 0 -> true
	end,
	io:format("Continue~n~n"),
	receive 
		stop -> io:write(secondStop),void
	after 10000 -> io:format("Continued timeout~n~n")
	end.


%%
%% Local Functions
%%

