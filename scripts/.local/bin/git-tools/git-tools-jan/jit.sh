#!/usr/bin/env bash

jit() {
    git_command=$1;
    before=$2;
    after=$3;

    shift;shift;shift;

    for arg do
        args="$args $before$arg$after"
    done

        if [ -n "$args" ]; then
            git ${git_command} ${args}
        else
            echo "No arguments specified after git command $git_command"
        fi
}
