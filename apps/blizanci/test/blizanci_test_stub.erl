%% A servlet module for tests, whose behaviour is chosen by the test.
%%
%% The route options carry a fun, under the key 'script', which is called
%% with 'request' or 'serve' and must return what that callback should
%% return. A servlet worker that is to be handed client data is started
%% with worker/2.

-module(blizanci_test_stub).

-export([request/4, serve/4, handle_client_data/2, cancel/1,
         default_options/0, validate_options/1]).
-export([worker/2]).

default_options() -> #{}.

validate_options(_Opts) -> ok.

request(_Matches, _Req, _Config, #{ script := F }) -> F(request).

serve(_Matches, _Req, _Config, #{ script := F }) -> F(serve).

%% Runs in the caller (the servlet container); the worker's answer is
%% either a reply, or a request that this call fail.
handle_client_data(Pid, Payload) ->
    Pid ! {client_data, self(), Payload},
    receive
        {worker_reply, {raise, Reason}} -> error(Reason);
        {worker_reply, Reply} -> Reply
    after 5000 -> error(worker_timeout)
    end.

cancel(Pid) ->
    Pid ! cancel,
    ok.

%% Start a worker that answers each client data message with
%% Handler(Payload), and tells Notify {worker_cancelled, WorkerPid} if
%% it is cancelled.
-spec worker(Notify, Handler) -> pid()
              when Notify  :: pid(),
                   Handler :: fun((binary()) -> term()).
worker(Notify, Handler) ->
    spawn(fun() -> worker_loop(Notify, Handler) end).

worker_loop(Notify, Handler) ->
    receive
        {client_data, From, Payload} ->
            From ! {worker_reply, Handler(Payload)},
            worker_loop(Notify, Handler);
        cancel ->
            Notify ! {worker_cancelled, self()}
    end.
