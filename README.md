# Frog-utils

Scheduling and running jobs on a shared computer, then analyse their output.

License: BSD.

## Usage

Provide several tools.

**WIP**

- `froglock`: execute a command when the scheduler decides so
- `frogmap`: run a command on many inputs (files?), with parallelization
- `frogreduce`: a library to analyse the output of `frogmap`



```sh
$ froglock <cmd> <arg1> ... <argn>
```

## Install

With opam:

```sh
$ opam pin add frogutils https://github.com/c-cube/frog-utils.git
```


