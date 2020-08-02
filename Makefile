## set the paths for a default setup
all:
	rm -rf *.info configs  logfiles *_service include *~ */*~ */*/*~;
	rm -rf */*.beam;
	rm -rf *.beam erl_crash.dump */erl_crash.dump */*/erl_crash.dump;
#	include
	git clone https://github.com/joq62/include.git;
	cp src/*.app ebin;
	erlc -I include -o ebin src/*.erl;
clean:
	rm -rf */*~ *.beam ebin/*.beam *~
test:
	rm -rf */*~ *.beam ebin/*.beam *~;
	rm -rf */*~ test_ebin/* test_src/*.beam test_src/*~;
	cp src/*.app ebin;
	erlc -o ./ebin src/*.erl;
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin -s test2 start
