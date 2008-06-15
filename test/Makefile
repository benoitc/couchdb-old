# depends on ../src/*/*.beam and ../src/couchdb/test/*.beam
test: test_runner.beam
	@erl -noshell -pa ../src/couchdb  -pa ../src/mochiweb -eval "test_runner:run()"

%.beam: %.erl
	erlc $<