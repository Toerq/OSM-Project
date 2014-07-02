######################
GROUP_NUMBER := 10
######################

#####################
# COMPILER COMMANDS #
ERLC := erlc
ERLC_FLAGS := -W -I include

JAVAC := javac
JAVAC_FLAGS := -classpath ".:./src/GeeseClient/resources/jinterface-1.5.6.jar"
#####################

.PHONY: doc

## Erlang files ##
_GEESE_FILES := $(wildcard geese/src/*.erl)
_BEAM_GEESE := $(patsubst geese/src/%.erl,geese_ebin/%.beam,${GEESE_FILES})
_GEESE_TEST_FILES := $(wildcard geese/src/*_test.erl)
## end of Erlang files

APPNAME=GEESE Project Group 10 - OSM 2014
ROOT=./
EBIN=$(ROOT)ebin/

GEESE=$(ROOT)geese/src/

## Filter out test files
GEESE_FILES=$(filter-out $(EUNIT_FILES), $(_GEESE_FILES))

## Filter out non-EUnit files
GEESE_TEST_FILES=$(filter-out geese/src/stress_test.erl, $(_GEESE_TEST_FILES))

comma:= ,
empty:=
space:= $(empty) $(empty)

EDOC_SRC := $(filter-out %_test.erl, $(_GEESE_FILES))
EDOC_SRC_LIST := [$(subst $(space),$(comma),$(patsubst src/%.erl,'src/%.erl', $(EDOC_SRC)))]

REQUIRED_DIR_NAME := pop_2012_project_group_$(GROUP_NUMBER)
REQUIRED_DIR_NAME := OSM-Project

PROJECT_DIR := $(notdir $(shell pwd))

USER=$(shell whoami)
## ARCHIVE_NAME := $(REQUIRED_DIR_NAME)_archive_$(USER)_$(shell date "+%Y-%m-%d__%H%M%S")__.tar.gz
ARCHIVE_NAME := OSM_2014_group_$(GROUP_NUMBER)_final_deliverable__$(shell date "+%Y-%m-%d__%H%M%S")__.tar.gz
ARCHIVE_DIR := ..

#=======================================================================#

all: erlc

## ERLANG CUSTOM COMPILATION (.beam files in ebin folder)
erlc: geese_compile geese_tests

geese_tests:
	$(ERLC) $(ERLC_FLAGS) -o ebin $(GEESE_TEST_FILES)

geese_compile:
	$(ERLC) $(ERLC_FLAGS) -o ebin $(GEESE_FILES)

## end of Erlang compilation

client: 
	java -jar jar/geese_client.jar &

server: erlc
	(cd ebin && erl -eval 'io:format("~n~p~n", ["START THE SERVER BY TYPING: geese_sup:start_link(3010)."])')

test: all
	(cd ebin && erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()')

doc: edoc javadoc

edoc: 
	erl -noshell -run edoc_run application "'$(APPNAME)'"  '"./geese/src/"' '[{dir, ["doc/html/erlang_server"]}, {def,{vsn,"$(VSN)"}}, {stylesheet, "my_style.css"}]'

javadoc:
	javadoc -d ./doc/html/java_client -sourcepath ./src/GeeseClient/src -subpackages geese -classpath ".:./src/GeeseClient/resources/jinterface-1.5.6.jar"

clean:
	rm -fr .#* *.dump
	rm -fr ebin/*
	rm -fr jbin/*	
	rm -fr doc/html/erlang_server/*.html
	rm -fr doc/html/java_client/*
## (cd doc/html && find . -name "*" -a ! -name overview.edoc -exec rm -rf {} \;)

remove_finderinfo:
	-xattr -d "com.apple.FinderInfo" src/*.erl include/*.hrl doc/* doc/html/*

archive: clean
ifeq ($(REQUIRED_DIR_NAME), $(PROJECT_DIR))
	(cd $(ARCHIVE_DIR) && tar cvfz $(ARCHIVE_NAME) $(PROJECT_DIR) --exclude .git)
	@echo 
	@echo NOTE: Archive created in $(ARCHIVE_DIR)/$(ARCHIVE_NAME)
	@echo 
else
	@echo Error: Wrong directory name >$(PROJECT_DIR)<, change to >$(REQUIRED_DIR_NAME)<
endif

