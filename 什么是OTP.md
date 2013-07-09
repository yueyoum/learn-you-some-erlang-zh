# What is OTP

## It's The Open Telecom Platform!
OTP代表了 开放电信平台，当然其现在的含义已经超越了 电信 本身。如果说Erlang NB之处 1/3是它的并发和分布式，另外1/3是错误处理，那么最后的1/3就是OTP框架。

在前面的章节中，我们已经见过一些例子，这些例子都是在做着大概相同的事情：如何使用语言内建功能:`links`, `monitors`, `servers`, `timeouts`, `trapping exits` 等写并发程序。在这些不同的例子中，总是有一些事情要做：如何避免race conditions, 或者总要提防一个process可能随时会挂。还有热更新，注册进程名字，添加supervisors，等等。

每个项目都手动的来做这些事情，不仅耗费时间，而且容易出错。OTP框架就是来解决这个问题的，OTP框架把这些在并发程序中必要的职责抽取出来，组织成库。这些库已经经过好多年的检验，每个Erlang程序员都应该使用OTP。

OTP框架同时也是一堆标准化设计的模块，用于帮助你完成你的应用。因为大部分Erlang程序员都在使用OTP，所以你遇到的大部分Erlang应用程序也是遵循OTP标准的。

## The Common Process, Abstracted
我们在前面例子中做了很多次的事情是划分所有的东西到具体的任务。在大部分的进程，我们有函数来spawn new process,有函数来给初始化状态，有主循环，等等。

这部分，后来发现，是所有并发程序都要用到的。无论你的程序是干什么的。

![1][1]

OTP框架的工程师和计算机科学家发现了这些模式，并且把它们集中到了一组公共的库里。这些库相当于我们使用的大部分抽象，这些库经过多年的检验，比我们自己实现的更谨慎，更安全。它们包含了安全的spawn 和 init 初始化进程，以容错的方式给它们发送信息，还有很多其他的特性。但有趣的是，你很少需要直接使用这些库。它们包含的抽象是如此基本的和普遍,更多的有趣的东西都是建立在它们之上的。这些库才是我们使用的。

![2][2]

在下面的内容中，我们会看到一些processes的公共用法，并且它们是如何抽象和通用的。每个例子都会有一个使用OTP的实现。

## The Basic Server
第一个普遍的模式是我们已经见过的。在写 [event server][3] 的时候，我们就有了一个 `client-server model`， `event server`会接收来自`client` 的请求，处理这些请求并回复。

在这章，我们将使用一个很简单server，为了让我们能把注意力放到server的本质上来。 

```erlang
%%%%% Naive version
-module(kitty_server).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, {order, Name, Color, Description}},
    receive
        {Ref, Cat} ->
            erlang:demonitor(Ref, [flush]),
            Cat;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

%% Synchronous call
close_shop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.
    
%%% Server functions
init() -> loop([]).

loop(Cats) ->
    receive
        {Pid, Ref, {order, Name, Color, Description}} ->
            if Cats =:= [] ->
                Pid ! {Ref, make_cat(Name, Color, Description)},
                loop(Cats); 
               Cats =/= [] -> % got to empty the stock
                Pid ! {Ref, hd(Cats)},
                loop(tl(Cats))
            end;
        {return, Cat = #cat{}} ->
            loop([Cat|Cats]);
        {Pid, Ref, terminate} ->
            Pid ! {Ref, ok},
            terminate(Cats);
        Unknown ->
            %% do some logging here too
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(Cats)
    end.

%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    ok.
```

这就是 kitty server/store，行为非常简单： 你描述一个cat，你就能得到这个cat，如果有人返还了一个cat，那么这么cat将加入到列表中，下次如果还有人请求cat，那么就从列表中取出来发送给某人，而不是按照其要求再造一个。

```erlang
1> c(kitty_server).
{ok,kitty_server}
2> rr(kitty_server).
[cat]
3> Pid = kitty_server:start_link().
<0.57.0>
4> Cat1 = kitty_server:order_cat(Pid, carl, brown, "loves to burn bridges").
#cat{name = carl,color = brown,
     description = "loves to burn bridges"}
5> kitty_server:return_cat(Pid, Cat1).
ok
6> kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
#cat{name = carl,color = brown,
     description = "loves to burn bridges"}
7> kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
#cat{name = jimmy,color = orange,description = "cuddly"}
8> kitty_server:return_cat(Pid, Cat1).
ok
9> kitty_server:close_shop(Pid).
carl was set free.
ok
10> kitty_server:close_shop(Pid).
** exception error: no such process or port
     in function  kitty_server:close_shop/1
```

回头看模块的代码，发现有些模式在先前已经做过了。比如monitors的建立和撤销，超时，接收数据，main loop，init函数，等等，这些都很熟悉。这些可以抽象出来，避免每次都做这些重复性的工作。

让我们看看client API, 首先注意到这两个同步调用极度相似。 这种调用就可以放入抽象库中。现在，我们仅仅将其抽象为在一个新模块中的一个函数。这个函数将包含`kitty server`的所有通用部分.

```erlang
-module(my_server).
-compile(export_all).

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.
```
这个函数需要两个参数，message 和 PID，它为你在安全的方式下转发消息。从现在开始，我们就用这个函数来代替消息发送。如果我们要重写`kitty server`，让它使用 `my_server`，那么它现在开起来就是这个样子：

```erlang
-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

%% Synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).
```

下一个通用处并不像`call/2`这样明显，注意到我们到现在写的每个process都有一个`loop`，在这里loop里，对收到的消息做模式匹配。这个地方有点难搞，但我们先把模式匹配从loop中拿出来。最快的方式就是这样：

```erlang
loop(Module, State) ->
    receive
        Message -> Module:handle(Message, State)
    end.
```

在实现模块里，就会有这些东西

```erlang
handle(Message1, State) -> NewState1;
handle(Message2, State) -> NewState2;
...
handle(MessageN, State) -> NewStateN.
```

这样好一些，这也是个让代码保持清晰的一个办法。如果你认真的读了 `kitty_server` 模块的代码，那么你应该注意到了我们用特殊的方法来区分同步调用和异步调用。这个很有用，对于我们的通用server的实现，最好能能清晰的知道到底是同步还是异步调用。

为了做到这点，我盟需要在`my_server:loop/2`中匹配不同的消息。这意味着我们需要修改`call/2`函数，让同步调用显式的使用一个`sync`原子

```erlang
call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.
```

现在我们可以提供一个异步调用函数了， 叫`cast/2`

```erlang
cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.
```
这些做完后，loop现在看起来就是这样：

```erlang
loop(Module, State) ->
    receive
        {async, Msg} ->
             loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} ->
             loop(Module, Module:handle_call(Msg, Pid, Ref, State))
    end.
```

并且对于那些无法匹配同步/异步概念的消息 可以添加特殊的处理。

有一点令人失望的是，上面的loop函数破坏了抽象的封装性。使用`my_server`的程序员当发送同步消息和回复的时候还必须得知道 `reference`。这让抽象变得没用。如果要让抽象工作，你必须得了解这些无聊的细节。下面是一种处理办法：

```erlang
loop(Module, State) ->
    receive
        {async, Msg} ->
             loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} ->
             loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
    end.
```

把`Pid` 和 `Ref` 放入一个tuple，它们可以作为一个单一的参数 `From` 传递给其他函数。这样使用者就完全不用关心其内部是什么东西。我们提供一个发送回复的函数，这个函数明白`From`里包含了什么

```erlang
reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.
```

最后要做的就是制定启动函数，这些函数接收模块名字和初始化参数。添加完毕后，my_server就是这样：

```erlang
-module(my_server).
-export([start/2, start_link/2, call/2, cast/2, reply/2]).

%%% Public API
start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

%%% Private stuff
init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
        {async, Msg} ->
             loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} ->
             loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
    end.
```

下面要做的就是重写kitty server，现在kitty server作为一个基于my_server的回调模块。我们保持了与先前同样的接口，但是所有的请求都转发到了my_server：

```erlang
-module(kitty_server2).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    my_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).
```

注意到上面我添加了第二个 `-export()`，这里面的函数是 `my_server` 所需要的:

```erlang
%%% Server functions
init([]) -> []. %% no treatment of info here!

handle_call({order, Name, Color, Description}, From, Cats) ->
    if Cats =:= [] ->
        my_server:reply(From, make_cat(Name, Color, Description)),
        Cats;
       Cats =/= [] ->
        my_server:reply(From, hd(Cats)),
        tl(Cats)
    end;

handle_call(terminate, From, Cats) ->
    my_server:reply(From, ok),
    terminate(Cats).

handle_cast({return, Cat = #cat{}}, Cats) ->
    [Cat|Cats].
```
最后要做的就是添加上这些私有函数:

```erlang
%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    exit(normal).
```

请确保这里用 `exit(normal)` 来代替先前的 `ok`，否则server会一直运行下去。

[1]: http://learnyousomeerlang.com/static/img/common-pattern.png
[2]: http://learnyousomeerlang.com/static/img/abstraction-layers.png
[3]: http://learnyousomeerlang.com/designing-a-concurrent-application