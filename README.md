# Tortoise [![Build Status](https://travis-ci.org/plasma-umass/Tortoise.svg?branch=master)](https://travis-ci.org/plasma-umass/Tortoise)

Tortoise is an automated repair tool for Puppet that allows system administrators to automatically synthesize program repairs based on changes made via the command-line. It's built on the idea of a synergistic programming model where the system administrator can fluidly move between declarative configuration in Puppet and imperative configuration via the command-line. Tortoise uses strace to attach to the shell of your choosing, and monitors filesystem effects while you edit. When you're ready, you can run `synth` to propagate these changes back to the Puppet manifest. You can find out more about Tortoise in [our paper](https://aaronweiss.us/pubs/ase17.pdf).

## Prerequisites

1. Oracle JDK 8.

2. [Microsoft Z3 Theorem Prover 4.5.0](https://github.com/Z3Prover/z3/releases/tag/z3-4.5.0).
   After installation, place the `z3` executable in your `PATH`.

3. [sbt](http://www.scala-sbt.org) version 0.13.9 or higher.

4. strace (should be installed or installable on Linux systems).

## Experiments

You can reproduce the results of our experiments by running the following commands. You may wish to vary the number of trials, as some of the tests took a while.
```
# Benchmarks of manifests from GitHub
./tortoise github-bench --trials 50
# Benchmarks varying manifest size (tortoise script doesn't play nice with quotes currently)
sbt 'run size-bench --infile synth/basic.pp --outfile size.csv --trials 10 --max 250 --constraints "</foo> -> nil, </bar> -> dir"'
# Benchmarks varying update size
./tortoise update-bench --outfile update.csv --trials 10 --max 15
# Benchmarks evaluating repair rankings
./tortoise human-bench
```

Note: the last benchmark is interactive, and requires the selection of the "correct" edit. For this reason, results may vary.

## Testimonials

"Hospitals use computers. Computers need to be configured. When computers are configured badly, they fail. When the computers hospitals use fail, people die. Some of those people are babies. This tool saves babies." - [Jared Holzman](https://github.com/JaredHolzman)
