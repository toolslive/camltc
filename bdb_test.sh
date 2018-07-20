#!/bin/bash

\rm -f _build/default/example/bdb_test.exe

jbuilder build example/bdb_test.exe

_build/default/example/bdb_test.exe
