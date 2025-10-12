# 🤖 CROBOTS2000 🤖

<p align="center">
  <img src="art/preview.png" width="500" alt="demo"/>
</p>

This is a remake of the vintage videogame [CROBOTS](http://tpoindex.github.io/crobots/) with 2D graphics and rigid body physics provided by [raylib](https://github.com/tjammer/raylib-ocaml) (more precisely, with the [OCaml bindings](https://github.com/tjammer/raylib-ocaml) of the library).

## Play

To run this game on your computer, you'll need a working installation of OCaml with the `opam` package manager and the `dune` build system installed. Once you have that sorted, you can start the game with the command:

```bash
dune exec crobots <robot-files>
```

There are examples working robots are in the [test directory](test/). In the command line below, we make two rabbits and a sniper fight:

```bash
dune exec crobots test/rabbit test/rabbit test/sniper
```

Now you can watch the sample robots compete or enjoy spinning your own robots!

## Robot API

The physics engine introduces a few changes to the robot API. The robot programming style is a bit different than the original game, in that:

+ The new `heading` primitive can be used to check a robot's current heading

+ A change in a robot's heading is not instantaneous but requires a number of CPU cycles to reach a desired angle.

  In order to make the robot travel on a precise `course` (e.g. towards the top left corner of the field, `135`°), you call the `drive` primitive with a desired speed of 0 to begin turning the robot in place:

  ```c
  drive(course, 0);
  ```

  Cycle until the desired heading is met:

  ```c
  while (heading() != course) ; // do nothing
  ```

  Then set off with your desired speed:

  ```c
  drive(course, speed);
  ```

  The effect of this procedure is shown in the right animation: the robot first rotates then moves in a clean straight line, whereas in the left simulation the robot turns and accelerates at the same time, missing the top-left corner.

  <p align="center">
    <img src="art/turn-default.gif" width="150" alt="free"/>
    <img src="art/turn-precise.gif" width="150" alt="in-place"/>
  </p>

+ Acceleration takes a few more CPU cycles than the original game. Before testing a robot's speed against 0 (i.e. the robot stopping due to damage or collision), make sure it is actually accelerating (i.e. the robot approaching a non-null speed), otherwise you'd risk stopping the robot unintentionally.

---

Enjoy!

<p align="center">
  <img src="art/demo.gif" width="300" alt="demo"/>
</p>