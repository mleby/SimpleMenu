#!/bin/bash

echo
echo ------------------------------------------------------------------
lazbuild --build-all --build-mode=Release simpleMenu.lpr

echo
echo ------------------------------------------------------------------
#lazbuild --build-all -q unittests.lpr
#./unittests --format=plain -a -p

