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
