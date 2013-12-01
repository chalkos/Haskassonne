#!/bin/bash

IN=$(cat /dev/stdin)

LINES=echo "$IN" | wc -l

echo "$LINES"