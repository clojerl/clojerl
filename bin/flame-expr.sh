#!/bin/bash

MODULE=${1:-"erlang"}
FUNCTION=${2:-"self"}
ARGS=${3:-"[]"}

TRACE_FILE="$MODULE-$FUNCTION.out"
TRACE_FILE_SORTED="$TRACE_FILE.sorted"
SVG_FILE="$TRACE_FILE_SORTED.svg"

echo "Running eflame..."

PID=`erl -sname eflame-expr -pa ebin -pa deps/*/ebin -s clojerl -noshell -eval "eflame:apply(normal_with_children, \"$TRACE_FILE\", '$MODULE', '$FUNCTION', $ARGS), io:format(\"~p~n\", [self()]), erlang:halt(0)."`

echo "PID: $PID"

cat $TRACE_FILE | sort | grep -v "$PID" > "$TRACE_FILE_SORTED"

deps/eflame/stack_to_flame.sh < $TRACE_FILE_SORTED > $SVG_FILE

open -a "Google Chrome" $SVG_FILE
