%% -*- erlang -*-

-type operation() :: atom().
-type arg()       :: any().
-type args()      :: [arg()].
-type segment()   :: {operation(),args()}.

%% A #dactyl_template{} is a list of segments. Each segment is an operation
%% to create the output binary string. Most of the time, this will simply
%% be a {literal,[String]} command. The segments are in reverse order so
%% that building the final output string is done backwards, and quicker.

-record(dactyl_template,
        {segs = [] :: [segment()]
        }).

%% Valid operations:
%%  literal    - The argument should be inserted verbatim
%%  basic      - A simple substitution
%%
