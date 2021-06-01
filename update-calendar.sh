#!/usr/bin/env bash

dir=$(dirname "$0")

racket ${dir}/update-calendar.rkt -s "$1" -r
racket ${dir}/update-calendar.rkt -s "$1" -i "$2" -p "$3"
