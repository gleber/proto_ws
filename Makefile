all:
	./rebar compile

test:
	./rebar eunit

sh:
	erl -pa ebin/

clean:
	./rebar clean
