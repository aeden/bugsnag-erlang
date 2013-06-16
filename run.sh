#!/bin/sh

erl -config sys.config -pa ebin deps/**/ebin -s bugsnag
