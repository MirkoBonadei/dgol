dgol - Distributed Game Of Life
=====

This my try to implement a distributed game of life, on the path showed by Torben Hoffman [here](https://github.com/lehoff/egol).

Checklist
---------
- [ ] The `dgol` process is not under the root supervisor. This could be a problem because a crash of the given process should cause the termination of the game;
- [ ] The `dgol` process should be a `gen_fsm` because it is based on the state (for example the async initialization which uses the state to leave the server in a pending state `#state{mode=not_started}`);
- [ ] Write some good tests to avoid regression on the startup of a `dgol` session (is it true that the `cell_sup` is supervising NxN processes?, ecc...);
- [ ] Introduce the `cell_mgr` process to decouple the coordinates of a cell from the pid that reppresent that cell;
- [ ] write the next steps :-)

Open a shell the the last source compiled and in the path
-----

    $ ./rebar3 shell

Starting the application
------------------------
```erl
Erlang/OTP 17 [erts-6.0] [source-07b8f44] [64-bit] [smp:8:8] [async-threads:0] [kernel-poll:false]

Eshell V6.0  (abort with ^G)
1> application:start(dgol).
ok
2> dgol:start(3, 3, [{1, 1}, {1, 2}]).
{ok,<0.73.0>}
```

Application overview
--------------------
![dgol application](https://github.com/MirkoBonadei/dgol/blob/master/doc/application.png)
