#!/bin/sh

erl -pa ebin deps/**/ebin -s bugsnag_app
