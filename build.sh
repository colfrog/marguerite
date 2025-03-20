#!/bin/sh
sbcl --non-interactive --load marguerite.asd --eval '(ql:quickload :marguerite)' --eval '(asdf:make :marguerite)' --eval '(quit)'
