#!/bin/sh
erl -sname reloader -setcookie nocookie -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s lager -s reloader 
