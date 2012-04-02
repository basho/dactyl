# Dactyl

Dactyl is a simple, string templating library for Erlang. It's not especially fast, but does have quite a bit of power. With Dactyl you can perform basic (named) parameter expansion, as well as conditional expansion, list expansion, and custom formatting.

## The Basics

Dactyl follows a very simple format for identifying named parameters to a template. The `~` character is used to prefix the name of a parameter. At the end of the name, a term operation specifies how the parameter is to be expanded. 

The parameter values are passed into the `render/2` function as a property list. Other templating libraries for Erlang use dictionaries, but property lists can be quite fast for small lists (up to ~20) and are much easier to debug if there is a problem.

Simple example:

	1> dactyl:render("~name~;",[{name,"Jeff"}]).
	"Jeff"

The above is the most basic example of using Dactyl. The `render/2` function compiles the template string into a `#dactyl_template{}` record, which is then expanded using the parameter values: `[{name,"Jeff"}]`. 

You can also choose to pre-compile the template and then render it multiple times using different parameter value proplists:

	2> {ok,Template}=dactyl:compile("~name~;").
	{ok,…}

	3> dactyl:render(Template,[{name,"Bob"}]).
	"Bob"

Dactyl is also capable of compiling files as templates, which can then be used like any other template:

	4> {ok,Home}=dactyl:compile_file("home.html").
   {ok,…}

## Understanding Parameter Formatting

Dactyl uses a syntax for formatting parameters similar to that of Common Lisp's `FORMAT` function. It isn't necessary, however, to be a black-belt in `FORMAT` to use Dactyl, as it only uses a fraction of its features.

Every named parameter begins with the `~` character and is followed by the name of the parameter. The value of this parameter is what will be expanded when the template is rendered. How the value is rendered depends on the operation that is supplied after the parameter name.

### Basic Substitution

The most common operation is the `~;` function, or "basic" substitution. It simply means that whatever the value of the parameter, convert it to a string and render it.

	5> dactyl:render("I am ~age~; years old.",[{age,36}]).
	"I am 36 years old."

### Conditional Branching

Sometimes it becomes necessary to expand a template in one of two directions based on a flag. Using the `~?` function, the value will be treated as a boolean and if `true` will expand one branch, otherwise an alternate branch is expanded.

The alternate branch is optional. Conditional branches are terminated with the `~;` function, but specifying an alternate branch can be done with the `~:` function.

	6> F=fun (N) -> 
	       dactyl:render("You have ~any~?~n~;~:no~; eggs.",[{any,N>0},
	                                                        {n,N}]) 
	     end.
	#Fun<erl_eval.6.80247286>
	7> F(6).
	"You have 6 eggs."
	8> F(0).
	"You have no eggs."

### Lists of Parameters

If you have a list of things you would like to expand multiple times (once per element in the list), then you can use the `~[` function to expand a list of parameter lists.

	9> dactyl:render("Hi,~people~[ ~name~;~].",[{people,[[{name,"Jeff"}],	                                                     [{name,"Tony"}],	                                                     [{name,"Mark"}]]
	                                            }]).
	"Hi, Jeff Tony Mark."

The `people` parameter pointed to a list, where each list was a new proplist that is then used for the inner template.

### Custom Formatting

Dactyl allows you to use the full power of Erlang's `io_lib:format/2` function as well by utilizing the `~{` expansion function. Once inside the `~{` function, you can then treat the template as the `Format` parameter to `io_lib:format/2` and the parameter value is used as the argument list to `io_lib:format/2`.

	10> dactyl:render("Value=~foo~{~p~}",[{foo,[{a,b,c}]}]).
	"Value={a,b,c}"

