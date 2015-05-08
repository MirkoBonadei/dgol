-ifdef(TEST).

-define(fail(MessageString, Replaceable),
        erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {msg, erlang:iolist_to_binary(io_lib:format(MessageString, Replaceable))}]})
       ).
-define(assertReceive(ExpectedMessage, Timeout),
        begin
            ((fun() ->
                  receive
                      ExpectedMessage ->
                          ok
                  after Timeout ->
                          ?fail(
                             "Failed to receive ~p within a timeout of ~p ms", 
                             [ExpectedMessage, Timeout])
                  end
              end)())
        end).

-endif.
