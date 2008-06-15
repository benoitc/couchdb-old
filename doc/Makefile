SOURCEFILES=\
"\"../src/couchdb/couch_config.erl\", \
\"../src/couchdb/couch_config_writer.erl\""

all:
	@echo "making docdir"
	erl -noshell -run edoc_run files [$(SOURCEFILES)]
