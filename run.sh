#!/bin/sh

erl -config sys.config -pa ebin _build/default/lib/**/ebin -s bugsnag
