#!/usr/bin/env bash

cargo build --release
cp target/release/piper target/piper-$1
