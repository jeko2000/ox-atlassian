#!/bin/bash

EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
ERT="ert"
SRC_FILE="./lisp/ox-atlassian.el"
TEST_FILE="./test/ox-atlassian-tests.el"

$EMACS -batch -l $ERT -l $SRC_FILE -l $TEST_FILE -f ert-run-tests-batch-and-exit
