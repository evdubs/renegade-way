#!/usr/bin/env bash

dir=$(dirname "$0")

racket -y ${dir}/update-calendar.rkt -i "$1" -k "$2" -p "$3" -s "$4"
