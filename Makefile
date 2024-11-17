all:	 
	rm -rf erl_cra* rebar3_crashreport;
	rm -rf *~ */*~ */*/*~ */*/*/*~;
	rm -rf test_ebin;
	rm -rf *.beam */*.beam;
	rm -rf test.rebar;
	rm -rf logs;
	rm -rf Mnesia.*;
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock;
	rm -rf *_container;
	rm -rf tar_dir;
	#INFO: Compile application
	rm -rf common_include;
	cp -r ~/erlang/common_include .
	rebar3 compile;
	rebar3 release;
	rebar3 as prod tar;
	rm -rf tar_dir;
	mkdir tar_dir;
	cp _build/prod/rel/$(appl)/*.tar.gz tar_dir/$(appl).tar.gz;
	rm -rf _build;
	git status
	echo Ok there you go!
	#INFO: no_ebin_commit ENDED SUCCESSFUL
clean:
	rm -rf erl_cra* rebar3_crashreport;
	rm -rf *~ */*~ */*/*~ */*/*/*~;
	rm -rf test_ebin;
	rm -rf *.beam */*.beam;
	rm -rf test.rebar;
	rm -rf logs;
	rm -rf Mnesia.*;
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock;
	rm -rf *_container;
	rm -rf tar_dir;
	#INFO: Compile application
	rm -rf common_include;
	cp -r ~/erlang/common_include .
	rebar3 compile;
	rm -rf _build;
	rm -rf rebar.lock
#INFO: clean ENDED SUCCESSFUL
eunit: 
	rm -rf erl_cra* rebar3_crashreport;
	rm -rf *~ */*~ */*/*~ */*/*/*~;
	rm -rf test_ebin;
	rm -rf *.beam */*.beam;
	rm -rf test.rebar;
	rm -rf logs;
	rm -rf Mnesia.*;
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock;
	rm -rf *_container;
#INFO: Creating eunit test code using test_ebin dir;
	mkdir test_ebin;
	erlc -o test_ebin test/*.erl;
	rm -rf common_include;
	cp -r ~/erlang/common_include .
	rebar3 release;
	rebar3 as prod tar;
	rm -rf tar_dir;
	mkdir tar_dir;
	cp _build/prod/rel/$(appl)/*.tar.gz tar_dir/$(appl).tar.gz;
	#INFO: Starts the eunit testing .................
	erl -pa test_ebin\
	 -sname test_$(appl)\
	 -run $(m) start\
	 -setcookie a
