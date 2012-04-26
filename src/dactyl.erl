%% -*- erlang -*-
%%
%% String templating library for Erlang
%%
%% Copyright (c) 2012 by Jeffrey Massung
%% All rights reserved
%%
%% No warranties explicit or implied. Use as you see fit for any projects
%% what-so-ever, but please give credit where due and leave this header
%% intact.
%%
%% dactyl.erl
%%

-module(dactyl).

%% public api
-export([f/2,
         to_s/1,
         render_file/2,
         render_file/3,
         render/2,
         render/3,
         compile_file/1,
         compile/1
        ]).

%% #dactyl_template[} record and types
-include("../include/dactyl.hrl").

%% simple helper function...
f (Fmt,Args) ->
    lists:flatten(io_lib:format(Fmt,Args)).

%% how we convert various types to a string
to_s (X) when is_list(X) -> X;
to_s (X) when is_binary(X) -> binary_to_list(X);
to_s (X) when is_atom(X) -> atom_to_list(X);
to_s (X) -> f("~p",[X]).

%% compile a source file, build a template, then render it
render_file (Filename,Args) ->
    case compile_file(Filename) of
        {ok,Template} -> render(Template,Args);
        Else -> Else
    end.

%% compile a source file, build a template, then render it with callbacks
render_file (Mod,Filename,Args) ->
    case compile_file(Filename) of
        {ok,Template} -> render(Mod,Template,Args);
        Else -> Else
    end.

%% render from a template
render (#dactyl_template{segs=Segs},Args) ->
    lists:flatten(lists:map(fun (Op) -> explode(Op,Args) end,Segs));

%% compile the source, build a template, then render it
render (String,Args) when is_list(String) ->
    render(list_to_binary(String),Args);
render (Binary,Args) when is_binary(Binary) ->
    case compile(Binary) of
        {ok,Template} -> render(Template,Args);
        Else -> Else
    end.

%% render from a template using module callbacks
render (Mod,#dactyl_template{segs=Segs},Args) ->
    lists:flatten(lists:map(fun (Op) -> explode(Op,{Mod,Args}) end,Segs));

%% render with module callbacks
render (Mod,String,Args) when is_list(String) ->
    render(Mod,list_to_binary(String),Args);
render (Mod,Binary,Args) when is_binary(Binary) ->
    case compile(Binary) of
        {ok,Template} -> render(Mod,Template,Args);
        Else -> Else
    end.

%% converts a template operation into a string given a proplist of args
explode ({literal,[String]},_Args) ->
    to_s(String);
explode ({basic,[Param]},Args) ->
    to_s(lookup(Param,Args));
explode ({either,[Param,True,False]},Args) ->
    case lookup(Param,Args) of
        true -> render(True,Args);
        _ -> render(False,Args)
    end;
explode ({list,[Param,Template]},{Mod,_}=ModArgs) ->
    List=lookup(Param,ModArgs),
    [render(Mod,Template,Props) || Props <- List];
explode ({list,[Param,Template]},Args) ->
    List=lookup(Param,Args),
    [render(Template,Props) || Props <- List];
explode ({format,[Param,Fmt]},Args) ->
    List=lookup(Param,Args),
    io_lib:format(Fmt,List).

%% given a parameter, modules, and arglist, call a function or lookup in list
lookup (Param,{Mod,Args}) ->
    case lists:member({Param,1},Mod:module_info(exports)) of
        false -> lookup(Param,Args);
        true -> Mod:Param(Args)
    end;
lookup (Param,Args) ->
    proplists:get_value(Param,Args).

%% build a #dactyl_template{} from a source file
compile_file (Filename) ->
    case file:read_file(Filename) of
        {ok,Binary} -> compile(Binary);
        Else -> Else
    end.

%% build a #dactyl_template{} from a source string or binary
compile (String) when is_list(String) ->
    compile(list_to_binary(String));
compile (Binary) when is_binary(Binary) ->
    make(#dactyl_template{},Binary,[]).

%% construct a new template from a binary string
make (#dactyl_template{segs=Segs}=Template,<<>>,[]) ->
    {ok,Template#dactyl_template{segs=lists:reverse(Segs)}};
make (_,<<>>,_) ->
    {error,unexpected_end_of_template};
make (#dactyl_template{segs=Segs}=Template,<<$~,Binary/binary>>,TS) ->
    <<C,Tail/binary>>=Binary,
    case lists:member(C,TS) of
        true ->
            {ok,C,Template#dactyl_template{segs=lists:reverse(Segs)},Tail};
        false ->
            case format(Binary) of
                {ok,Op,XS,Rest} ->
                    make(Template#dactyl_template{segs=[{Op,XS}|Segs]},Rest,TS);
                Else ->
                    Else
            end
    end;
make (#dactyl_template{segs=Segs}=Template,Binary,TS) ->
    case literal(Binary,[]) of
        {ok,S,Rest} ->
            make(Template#dactyl_template{segs=[{literal,[S]}|Segs]},Rest,TS);
        Else ->
            Else
    end.

%% extract a literal from a template binary
literal (<<$~,$~,Rest/binary>>,S) ->
    case literal(Rest,S) of
        {ok,S,Binary} -> {ok,[$~|S],Binary};
        Else -> Else
    end;
literal (<<$~,_/binary>>=Rest,S) ->
    {ok,S,Rest};
literal (<<C,Rest/binary>>,S) ->
    case literal(Rest,S) of
        {ok,S2,Binary} -> {ok,[C|S2],Binary};
        Else -> Else
    end;
literal (<<>>,S) ->
    {ok,S,<<>>}.

%% extract a formatted operation from a template binary
format (Binary) ->
    case scan(Binary) of
        {ok,Param,Term,Rest} ->
            case term_op(Term) of
                {ok,Op} -> Op(list_to_atom(Param),Rest);
                Else -> Else
            end;
        Else -> Else
    end.

%% scan a binary looking for a terminal
scan (<<>>) ->
    {error,unexpected_end_of_template};
scan (<<$~,$~,Rest/binary>>) ->
    case scan(Rest) of
        {ok,S,Term,Tail} -> {ok,[$~|S],Term,Tail};
        Else -> Else
    end;
scan (<<$~,Term,Rest/binary>>) ->
    {ok,[],Term,Rest};
scan (<<C,Rest/binary>>) ->
    case scan(Rest) of
        {ok,S,Term,Tail} -> {ok,[C|S],Term,Tail};
        Else -> Else
    end.

%% fetch the operation for a given terminal type
term_op ($;) -> {ok,fun basic/2};
term_op ($?) -> {ok,fun either/2};
term_op ($[) -> {ok,fun list/2};
term_op (${) -> {ok,fun format/2};
term_op (Op) -> {error,{invalid_term_op,Op}}.

%% basic substitution
basic (Param,Rest) ->
    {ok,basic,[Param],Rest}.

%% conditional evaluation
either (Param,Binary) ->
    case make(#dactyl_template{},Binary,[$:,$;]) of
        {ok,$;,True,Rest} ->
            {ok,either,[Param,True,#dactyl_template{}],Rest};
        {ok,$:,True,Rest} ->
            case make(#dactyl_template{},Rest,[$;]) of
                {ok,_,False,Tail} ->
                    {ok,either,[Param,True,False],Tail};
                Else ->
                    Else
            end;
        Else ->
            Else
    end.

%% list evaluation
list (Param,Binary) ->
    case make(#dactyl_template{},Binary,[$]]) of
        {ok,_,Template,Rest} -> {ok,list,[Param,Template],Rest};
        Else -> Else
    end.

%% custom formatting
format (Param,Binary) ->
    case scan(Binary) of
        {ok,S,$},Rest} ->
            {ok,format,[Param,S],Rest};
        {ok,S,Fmt,Rest} ->
            case format(Param,Rest) of
                {ok,format,[_,S2],Tail} ->
                    {ok,format,[Param,lists:flatten([S,[$~,Fmt],S2])],Tail};
                Else ->
                    Else
            end;
        Else ->
            Else
    end.
