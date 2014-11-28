# Frog-utils

Scheduling and running jobs on a shared computer, then analyse their output.

License: BSD.

## Usage

Provide several tools and one library, `frogutils`. The tools are the
following ones:

- `froglock`: execute commands sequentially, no overlap.
- `frogmap`: run a command on many inputs (files?), with parallelization.
- `frogiter`: run a command on every result obtained with `frogmap`.
- `frogtptp`: run a TPTP prover (more specific!) and analyse its output.

### Froglock

The basic usage is `froglock <cmd>` or `froglock -- <cmd>`. This
has the same behavior as `<cmd>`, except for one point: at any given
time, on a given computer, at most one command launched from `froglock <cmd>`
runs. Until it terminates, the other commands aren't started yet; once
it terminates one waiting command is started and takes ownership of the "lock".
This works by connecting to a daemon on a given port, starting it if required.
The daemon will stop if its queue is empty.

`froglock -status` can be used to display the current queue of tasks (including
the one that owns the lock). It prints nothing if no task is launched.

There is one "lock" per TCP port on the machine (although only ports > 1024
should be used, otherwise only root will be able to use `froglock`). The
port can be changed with `-port <n>`.

### Frogmap

`frogmap` applies a command `<cmd>` (shell command) to a list of elements,
and stores the result of `<cmd> <arg>` for every such element `<arg>`.

Parallelism (on a single computer) can be achieved with `-j <n>` where
`<n>` is the number of parallel invocations of `<cmd>`

If `frogmap` is killed or stopped for any other reason before it could
process every argument, it is possible to resume the computation
from where it left: `frogmap -resume <state_file.json>`.

Results stored in a file `<file.json>` can be analysed either using the module
`FrogMapState` (in the library `frogutils`, see `src/frogMapState.mli`) or with
`frogiter` (see below).

### Frogiter

`frogiter` goes hand in hand with `frogmap`. It is a simple way to
apply a command to all the results stored in some `<file.json>` produced by
`frogmap`. Example:

```sh
    frogmap -o foo.json -j 20 'sleep 3; echo ' `seq 1 1000`

    # later...
    frogiter foo.json -c 'grep 11'
```

will print all number from 1 to 1000 that contains the string 11 in
their decimal representation. In a slightly inefficient way.

### Frogtptp

Frogtptp provides several commands to deal with TPTP provers (although it
would probably work almost as-is with SMT solvers too).

The commands are:

- `frogtptp -run <prover_name> file.p` runs a theorem prover on the TPTP file.
  `<prover_name>` is the name of a prover as listed in the config file
  `$HOME/.frogtptp.toml` (or with the `-config` flag).
  Other options specify the memory limit and timeout for the prover.
- `frogtptp -list` lists the known provers (those detailed
  in the config file).
- `frogtptp -analyse <prover>,<file.json>` analyses a single output file
  as obtained from `frogmap 'frogtptp -run <prover>' file1 file2 ...`.
  `frogtptp -analyse "<prover1>,<file1.json>" "<prover2>,<file2.json>" ...`
  will do the same but also compare the performance of the different provers.
  This is still work in progress.

Example:

```sh
    # I have 10 cores, let's prove stuff with E
    frogmap -j 10 -o bench.json \
      'frogtptp -run eprover -timeout 5' \
      $TPTP/Problems/*/*.p

    # gosh, I have to reboot!
    sleep 500; reboot

    # resume where I left. But now I have 30 cores!
    frogmap -j 30 -resume bench.json

    # then: basic statistics on the results
    frogtptp -analyse eprover,bench.json
```

runs the [E prover](http://eprover.org) (named `eprover`) on all files in
a [TPTP](http://www.cs.miami.edu/~tptp/) archive, resumes the computation
after a reboot, and finally prints some basic statistics about the results.

A sample config file for `frogtptp` can be found in `data/frogtptp.toml`. Its
format is [toml](https://github.com/toml-lang/toml), a simple textual
configuration format, close to `ini`.


## Install

With opam:

```sh
$ opam pin add frogutils https://github.com/c-cube/frog-utils.git
```


