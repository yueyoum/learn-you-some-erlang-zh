# Clients and Servers

我们将要看到的第一个OTP行为是使用最频繁的一个。它就是`gen_server`。它的接口有点类似于我们在上一章已经写过的`my_server`。它为你提供了一些功能，当然，你的模块也得为`gen_server`准备好回调函数。

#### init

首先是 `init/1`函数。 它和我们使用过的与`my_server`配合的`init/1`很像。它用于初始化server的状态，和执行初始化时的一次性任务。这个函数可以返回 `{ok, State}`, `{ok, State, TimeOUt}`, `{ok, State, hibernate}`, `{stop, Reason}`, 或者是 `ignore`。

正常返回`{ok, State}`的时候，`State`作为server的状态直接传递给main loop。如果返回的tuple里加上了 `TimeOUt`，那么意味着server接收消息会有一个超时时间。如果到了超时时间，server都没有收到消息，那么一个特殊的消息（原子 `timeout`）会发送给server，server应该用 `handle_info/2`（后面会讲解这个） 来处理。

如果你的进程需要很长时间才能得到回复，并且担心内存占用，那么你可以给返回的tuple加上 `hibernate`。 hibernate以损失一定的处理能力，来降低进程state的size，直到其接收到一个消息。如果你不知道该不该使用`hibernate`，那么你八成是不该使用。

如果在初始化的时候发生错误，那么就返回 `{stop, Reason}`.

**注意:** 更多关于hibernation的信息。 如果读者没理解也没关系。如果BIF `erlang:hibernate(M, F, A)` 被调用，那么当前运行进程的调用栈将被丢弃（调用永不会返回）。垃圾回收会介入处理。这会压缩数据，从而让进程占用更少的空间。
一旦进程收到消息，那么会以 A 为参数，再次调用 M:F，从而恢复执行。

**注意：** 当运行`init/1`的时候，这是一个block的调用。

#### handle_call
`handle_call/3` 用来处理同步消息（我们马上将会看到如何使用它）。它需要3个参数： `Request`, `From`, `State`。这个和我们自己写的 my_server 里面的 handle_call/3 很相似。不过最大的不同时如何回复消息。在我们自己的版本中，你必须调用 `my_server:reply/2`来回复消息。 在`gen_server` 中，有8种不同的返回，都是tuple类型。

    {reply, Reply, NewState}
    {reply, Reply, NewState, Timeout}
    {reply, Reply, NewState, hibernate}
    {noreply, NewState}
    {noreply, NewState, Timeout}
    {noreply, NewState, hibernate}
    {stop, Reason, NewState}

`Timeout`, `hibernate` 的作用和`init/1`中的一样。而 `Reply` 将会被发送给调用者。注意还有三种`noreply`的返回形式。如果你使用了`noreply`，那么gen_server会认为你要自己处理返回。这个可以通过调用`gen_server:reply/2`来完成。它和`my_server:reply/2`的使用方式一样。

大多数你只需要使用`reply`。 但也有些情况下是要使用`noreply`的：你想让其他的进程来回复消息，或者你仅仅是先回复一个确认（表示已经收到消息了），然后继续处理收到的消息（并不是收到的时候返还处理结果）等等。如果你要这么做的话，那么显然你得使用`gen_server:reply/2`，否则调用就会因为超时而崩溃。

#### handle_cast
`handle_cast`回调使用起来和我们自己的`my_server`中的很像：它需要`Message`， `State`这两个参数，用来处理异步调用。和`handle_call/3` 相似，你可以在这里干任何事情。但这里不能返回 `reply` ：

    {noreply, NewState}
    {noreply, NewState, Timeout}
    {noreply, NewState, hibernate}
    {stop, Reason, NewState}

#### handle_info
你还记得我提到过我们自己的server并不有完成所有的接口？  `handle_info/2`就是来干这个事的。它和`handle_cast/2`类似，并且事实上连返回都一样。不同的是这个回调处理的是 直接通过 `!` 给server发的消息。比如 `init/1` 的timeout, monitor的通知，以及 `'EXIT'`消息。

#### terminate
`terminate/2` 回调在 上面三个 `handle_Something` 返回 `{stop, Reason, NewState}` 或者 `{stop, Reason, Reply, NewState}`的时候调用。它有两个参数， `Reason`，`State`。 它们的值和 `stop` 元组中的值一样。

当`gen_server` 设置了 trapping exit的时候， `terminate/2`  也会在 父进程挂的时候被调用。

**注意：** 如果调用 `terminate/2`的时候， reason不是 `normal`, `shutdown`, `{shutdown, Term}`的话，那么OTP框架将会认为这个进程执行失败，并且用logging记录日志

这个函数就是 `init/1` 的对立函数一样。所以你要在这里做 与`init/1` 中向对立的事情。它就是你server的看门人，当所有的人都离开后，它来关门。这和函数应该删除 ETS table, 关闭 ports，等等释放资源。但注意的是这个函数返回什么已经不重要了，因为当这个函数被调用，此进程就结束了执行。

#### code_change
`change_change/3` 用来更新代码。它的形式是 `code_change(PreviousVersion, State, Extra)`。 `PreviousVersion` 要么是version term， 要么是 `{down, Version}` 用于降级。 `State`是当前的server state，你在升级的时候可以改变它。

假设现在我们用orddict作为server state来存储数据。但现在orddict太慢了，所以我们决定将其变成普通的dict。为了避免在下次函数调用的是崩溃，数据结构的改变就应该在这里完成。然后你就返回新的状态 {ok, NewState}。

`Extra` 我们现在用不到，它是用于大型OTP应用的。

现在我们看完了所有的回调定义。如果你有点迷糊了，别担心。OTP框架就是这样的，你要理解A，你就必须先立即B，但B又依赖于A。要想搞明白的最好办法就是自己实现一个gen_server.

## .BEAM me up, Scotty!
让我们来写一个 [kitty_gen_server][1].它和`kitty_server2`很像，只有很少的API变动。 首先在 `kitty_gen_server.erl` 中写入下面两行：

```erlang
-module(kitty_gen_server).
-behaviour(gen_server).
```

然后尝试编译，你将得到下面的输出:

```erlang
1> c(kitty_gen_server).
./kitty_gen_server.erl:2: Warning: undefined callback function code_change/3 (behaviour 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function handle_call/3 (behaviour 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function handle_cast/2 (behaviour 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function handle_info/2 (behaviour 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function init/1 (behaviour 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function terminate/2 (behaviour 'gen_server')
{ok,kitty_gen_server}
```

编译工作了，但是警告显示缺少callbacks。 这是因为 `gen_server`行为模式。 一个行为模式就是它期望一个模块要到处什么函数。行为模式就是来规范和嫁接通用部分和你自己的特定模块的。

**注意：** `behavior`, `behaviour` 都可以。

定义你自己的行为模式也很简单。你只需要到处一个名叫 `behaviour_info/1` 的函数：

```erlang
-module(my_behaviour).
-export([behaviour_info/1]).

%% init/1, some_fun/0 and other/3 are now expected callbacks
behaviour_info(callbacks) -> [{init,1}, {some_fun, 0}, {other, 3}];
behaviour_info(_) -> undefined.
```

然后你在你的模块中 添加 `-behaviour(my_behaviour).`，并且实现定义的回调函数即可。显然让我们回到 kitty server。

首先是 `start_link/0`， 这个函数可以修改成下面的样子：

```erlang
start_link() -> gen_server:start_link(?MODULE, [], []).
```

第一个参数是回调模块，第二个是参数列表，这些参数会传递给`init/1`，第三个是一个写debugging选项，我们在这里暂时不涉及。你可以在参数的第一个位置添加 [第四个][2] 参数，这个参数是server要注册的名字。注意现在的函数是返回Pid，现在得返回　`{ok, Pid}`.

后续函数是:

```erlang
%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
   gen_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
    gen_server:call(Pid, terminate).
```

所有调用都是简单的一一转换。注意你可以添加第三个timeou参数给 `gen_server:call/2-3`。如果你没有给timeout（或者没给原子 `infinity`），那么默认的timeout是5秒。5秒内，如果没有收到回复，这个调用就会崩溃。

现在我们来添加gen_server的回调。下面是调用和回调之间的关系：

<table>
    <tr><td>gen_server</td><td>YourModule</td></tr>
    <tr><td>start/3-4</td><td>init/1</td></tr>
    <tr><td>start_link/3-4</td><td>init/1</td></tr>
    <tr><td>call/2-3</td><td>handle_call/3</td></tr>
    <tr><td>cast/2</td><td>handle_cast/2</td></tr>
</table>

你还有些回调，它们用于处理特殊情况

*   handle_info/2
*   terminate/2
*   code_change/3

让我们一一修改这些函数:

```erlang
%%% Server functions
init([]) -> {ok, []}. %% no treatment of info here!

handle_call({order, Name, Color, Description}, _From, Cats) ->
    if Cats =:= [] ->
        {reply, make_cat(Name, Color, Description), Cats};
       Cats =/= [] ->
        {reply, hd(Cats), tl(Cats)}
    end;
handle_call(terminate, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
    {noreply, [Cat|Cats]}.
```

得益于更好的抽象，我们的代码更简短了。现在来看看新的回调，首先是 `handle_info/2`。因为这是个玩具模块，而且我们并没有logging system，所以这里就简单的输出 意外的消息即可：

```erlang
handle_info(Msg, Cats) ->
    io:format("Unexpected messages: ~p~n", [Msg]),
    {noreply, Cats}.
```

接下来是 `terminate/2` 回调

```erlang
terminate(normal, Cats) ->
    [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
    ok.
```

最后一个回调 `code_change/3` :

```erlang
code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}. 
```

同时还要记得 `make_cat/3` 这个私有函数：

```erlang
%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, Description=Desc}.
```

现在让我们来试试这个新模块

```erlang
1> c(kitty_gen_server).
{ok,kitty_gen_server}
2> rr(kitty_gen_server).
[cat]
3> {ok, Pid} = kitty_gen_server:start_link().
{ok,<0.253.0>}
4> Pid ! <<"Test handle_info">>.
Unexpected message: <<"Test handle_info">>
<<"Test handle_info">>
5> Cat = kitty_gen_server:order_cat(Pid, "Cat Stevens", white, "not actually a cat").
#cat{name = "Cat Stevens",color = white,
     description = "not actually a cat"}
6> kitty_gen_server:return_cat(Pid, Cat).
ok
7> kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
#cat{name = "Cat Stevens",color = white,
     description = "not actually a cat"}
8> kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
#cat{name = "Kitten Mittens",color = black,
     description = "look at them little paws!"}
9> kitty_gen_server:return_cat(Pid, Cat).
ok       
10> kitty_gen_server:close_shop(Pid).
"Cat Stevens" was set free.
ok
```

通用模式有什么好处？把通用的部分抽离出来，这样你的代码更容易维护，复杂度降低，代码更加安全，更容易测试，更少的bugs。如果有bug，也很容易fix。generic servers仅仅是众多的抽象之一，但却是使用最频繁的一个。在后续的章节里我们会看到更多的抽象行为。


[1]: http://learnyousomeerlang.com/static/erlang/kitty_gen_server.erl
[2]: http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4