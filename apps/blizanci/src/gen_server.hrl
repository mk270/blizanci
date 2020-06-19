%% blizanci, a Gemini protocol server, by Martin Keegan

%% Whereas blizanci is free software protected by copyright, the definitions
%% in this file are determined mechanically by the specification of Erlang/OTP
%% and thus are unlikely to be protected by copyright. In any case, they
%% are certainly not the creation of the author of blizanci.

-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
-spec handle_call(Request :: term(), From :: {pid(), term()},
                  State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(),
                          Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(),
                          hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(),
                          NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.

-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().

-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.

-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().

