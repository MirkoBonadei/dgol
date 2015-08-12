dgol - Distributed Game Of Life
=====

dgol - Distributed Game Of Life
=====

This project is an implementation of the [Game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) done by [Gabriele Lana](https://github.com/gabrielelana) and me during the last months. 

We took it as a "toy project" to explore all the nontrivial decisions that need to be made when you have to program a distributed system (eg: choose the right supervision strategy, how to make sub-systems communicate each other, how to store data to make it fault tolerant, ecc...).

It is inspired by the [Torben Hoffman's version](https://github.com/lehoff/egol) and on the talk [Thinking like an Erlanger](https://www.youtube.com/watch?v=6sBL1kHoMoo).

The project is still under development, at the moment we are doing a huge refacroting of the codebase because we are reorganizing the supervision strategy.

There is nice interface (developed with [wxWidgets](http://www.erlang.org/doc/apps/wx/chapter.html)) to visually test the project:

It is possible to start a demonstration with:
Note that the first parameter of `demo:start/2` is the dimension of the universe and the second parameter is a list of cells alive at the time 0.
```erlang
demo:start(30, [{15, 15},{14,15},{16,15}]).
```
<img src="https://github.com/MirkoBonadei/dgol/blob/master/doc/dgol_1.png" width="400">

Clicking on "tick" makes the univese time advance by one tick while clicking on "start" makes the univese time ticking at a constant rate (until "stop" is clicked).

<img src="https://github.com/MirkoBonadei/dgol/blob/master/doc/dgol_2.png" width="400">

Killing cells is really easy, it only requires a double click on the condemned cell. The cell will chage its color to red until it will have recovered and will not be at par with the other cells.

<img src="https://github.com/MirkoBonadei/dgol/blob/master/doc/dgol_3.png" width="400">

When it happens everything will go on normally.

<img src="https://github.com/MirkoBonadei/dgol/blob/master/doc/dgol_4.png" width="400">

### How to use it
To compile code:
```
./rebar3 compile
```

To run tests:
```
./rebar3 eunit
```

To start a shell and try to play:
```
./rebar3 shell
```
