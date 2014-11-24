# Frog-utils

Scheduling and running jobs on a shared computer, then analyse their output.

License: BSD.

## Usage

Provide several tools.

**WIP**

- `froglock`: execute a command when the scheduler decides so
- `frogmap`: run a command on many inputs (files?), with parallelization
- `frogiter`: run a command on every result obtained with `frogmap`

For instance:

```sh
$ froglock <cmd> <arg1> ... <argn>
```

## Install

With opam:

```sh
$ opam pin add frogutils https://github.com/c-cube/frog-utils.git
```


