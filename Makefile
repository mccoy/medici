APP_NAME="medici"
VSN="0.5"

all: compile

docs: 
	erl -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '$(VSN)'

compile: clean
	erlc -o ebin/ src/*.erl
	cp src/*.app ebin/

clean:
	rm -rfv ebin/
	mkdir ebin
	rm -fv test/*.beam

# Testing with a Tokyo Tyrant server instance
test: clean ttclean tt_normal testbuild run_basic_test tt_table run_table_test ttstopd #ttclean
testbuild:
	erlc -DTEST -DDEBUG +debug_info -o ebin/ src/*.erl
	erlc -o test/ test/*.erl
run_basic_test:
	erl -pa ebin/ -pa test/ -noshell -s principe_test test -s init stop
run_table_test:
	erl -pa ebin/ -pa test/ -noshell -s principe_table_test test -s init stop
ttclean:
	rm -f /tmp/ttserver.pid /tmp/ttserver.*
tt_normal:
	ttserver -dmn -kl -pid /tmp/ttserver.pid /tmp/ttserver.tch
tt_table:
	ttserver -dmn -kl -pid /tmp/ttserver.pid /tmp/ttserver.tct
ttstopd:
	kill -TERM `head -1 /tmp/ttserver.pid`
