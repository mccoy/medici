APP_NAME="principe"
VSN="0.4"

all: compile

docs: 
	erl -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '$(VSN)'

compile: clean
	erlc -o ebin/ src/*.erl
	cp src/*.app ebin/

clean:
	rm -rfv ebin/
	mkdir ebin

# Testing with a Tokyo Tyrant server instance
test: clean ttclean ttstartd runtest ttstopd
runtest:
	erlc -DTEST -I test/ -o ebin/ src/*.erl
	erl -pa ebin/ -noshell -s principe test -s init stop
ttclean:
	rm -f /tmp/ttserver.pid /tmp/ttserver.tcb
ttstartd:
	ttserver -dmn -pid /tmp/ttserver.pid /tmp/ttserver.tcb
ttstopd:
	kill -TERM `cat /tmp/ttserver.pid`
ttstart:
	ttserver /tmp/ttserver.tcb
