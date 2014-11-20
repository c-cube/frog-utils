# join-locke

A queue system for scheduling jobs on a shared computer. BSD license.

## Usage

```sh
$ join_locke <cmd> <arg1> ... <argn>
```

## Install

With opam:

```sh
$ opam pin add join-locke https://github.com/c-cube/join-locke.git
```


## Misc

The name obviously refers to a (dining?) philosopher. Basically this
program is used to share one fork between many hungry philosophers,
in a dictatorial way.
