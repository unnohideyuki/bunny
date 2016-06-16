#!/bin/bash -v
# usage (eg.) : compile0 sample0

f=$1

mkdir -p jout/$f

sample/compiler0 < testcases/$f.hs > jout/$f/Sample.java
