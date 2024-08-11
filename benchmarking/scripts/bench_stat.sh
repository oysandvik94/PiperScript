#!/usr/bin/env bash
CARGO_PROFILE_RELEASE_DEBUG=true cargo build --release && perf record --call-graph dwarf target/release/repl benchmarking/bench_fib.las && perf report
