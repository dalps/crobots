# 🤖 CROBOTS2000 🤖

<p align="center">
  <img src="art/demo.gif" width="300" alt="demo"/>
</p>

A silly spin on the vintage game [CROBOTS](http://tpoindex.github.io/crobots/) with 2D graphics and rigid body physics!

Made for fun with the [raylib](https://github.com/tjammer/raylib-ocaml) library.

## Play

```bash
dune exec crobots <robot-files>
```

Watch the sample robots in the `test` directory compete, or write and test your own robots!

## Robot API

The physics engine introduces a few changes to the robot API and the robot programming style of the original game:

+ The new `heading` primitive can be used to check a robot's current heading

+ Unlike the original game, a change in a robot's heading is not instantaneous but requires a number of CPU cycles to reach a desired angle. In order to make the robot travel on a precise trajectory (e.g. the top left corner of the field), call the `drive` primitive with a desired speed of 0 to start the turning the robot, loop until the desired heading is met, then set a desired speed. The effect of this procedure is shown in the right animation. 

<p align="center">
  <img src="art/turn-default.gif" width="150" alt="free"/>
  <img src="art/turn-precise.gif" width="150" alt="in-place"/>
</p>

+ Acceleration takes a few more CPU cycles than the original game. Before testing a robot's speed against 0 (i.e. the robot stopping due to damage or collision), make sure it is actually accelerating (i.e. the robot approaching a non-null speed)
