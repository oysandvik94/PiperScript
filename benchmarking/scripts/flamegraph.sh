#!/usr/bin/env bash

CARGO_PROFILE_RELEASE_DEBUG=true cargo flamegraph -- benchmarking/bench_fib.las
