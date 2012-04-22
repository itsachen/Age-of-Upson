#!/bin/bash
if [ $# -lt 1 ]
then
echo "usage: $0 <team-name>"
exit -1
fi
ocamlc -o $1.exe -I +threads -I ../game -I ../shared unix.cma threads.cma str.cma ../shared/thread_pool.mli ../shared/thread_pool.ml ../shared/connection.mli ../shared/connection.ml ../shared/constants.ml ../shared/definitions.ml ../shared/util.ml team.ml $1.ml
