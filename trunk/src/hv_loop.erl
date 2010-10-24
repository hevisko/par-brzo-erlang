%% Author: hendrivi
%% Created: Jun 29, 2010
%% Description: TODO: Add description to hv_loop
-module(hv_loop).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([loop/2]).

%%
%% API Functions
%%

%%
%% TODO: Add description of loop/function_arity
%%
loop(_X,0) -> true;
loop(X,N) when is_integer(N)-> 
	X(N),loop(X,N-1).


%%
%% Local Functions
%%

