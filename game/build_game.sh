#!/bin/bash
ocamlc -o game.exe -I +threads -I ../shared unix.cma threads.cma str.cma ../shared/thread_pool.mli ../shared/thread_pool.ml ../shared/connection.mli ../shared/connection.ml ../shared/constants.ml ../shared/definitions.ml ../shared/util.ml netgraphics.mli netgraphics.ml game.mli game.ml server.ml
