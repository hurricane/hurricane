%%% A module for common utility functions.

-module(hurricane_utils).

-export([get_best_pid/1]).

%% Given a group name, gets a random Pid from the group, favoring local
%% Pids over remote ones.
get_best_pid(Name) ->
    pg2:get_closest_pid(Name).
