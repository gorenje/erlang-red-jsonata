##
## Development docker image - used for local development.
## assumes codebase happens to be located at /mnt/github/erlang-red ...
build-docker-container:
	docker build -f Dockerfile.dev -t erlang-jsonata-shell .

start-docker-shell: build-docker-container
	docker run -it -v /mnt/github/erlang-red-jsonata:/code -w /code --rm erlang-jsonata-shell bash

enter-docker-shell:
	docker exec -it $$(docker ps -f ancestor=erlang-jsonata-shell -q) bash

##
## The following are done inside the docker container - after running
## make enter-docker-shell
##
compile:
	rebar3 compile

compile-loop:
	while [ 1 ] ; do make -s compile ; sleep 2 ; reset ; done

shell:
	rebar3 shell

app-start:
	rebar3 shell --apps erlang_red_jsonata

app-start-loop:
	while [ 1 ] ; do make -s app-start ; done

eunit-test:
	rebar3 eunit

# This helps understanding dialyzser
#  --> https://grantwinney.com/common-dialyzer-errors-and-solutions-in-erlang/
#  --> https://erlangforums.com/t/unknown-type-ranch-ref-0-dialyzer-warning-in-cowboy-req-on-erlang-otp-26/2760 - discussion "unknown functions"
dialyzer:
	rebar3 dialyzer

format-code:
	rebar3 fmt -w

release:
	rebar3 release
