#!/usr/bin/env bash

hyperfine --warmup 1 --runs 3 'cargo run --release -- benchmarking/bench_fib.las'
