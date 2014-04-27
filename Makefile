######################
# GROUP_NUMBER := 10 #
######################

#####################
# COMPILER COMMANDS #
ERLC := erlc
ERLC_FLAGS := -W -I include

JAVAC := javac
JAVAC_FLAGS := -classpath ".:./src/lib/jinterface-1.5.6.jar"
# local jinterface .jar file
#####################


ERL_FILES := $(wildcard src/*.erl) 
# the wildcard function will get a list of all the .erl files in the src folder

BEAM_FILES := $(patsubst src/%.erl,ebin/%.beam,${ERL_FILES}) 
# $(patsubst pattern,replacement,text) - the list of ERL_FILES are modified so
# that the .erl extension is replaced by .beam. 
# $(patsubst %.erl,%.beam,foo.erl bar.erl) -> 'foo.beam bar.erl'


## Erlang files - custom (ugly) version 1.0 ##
ERL_UDP_FILES := $(wildcard src/simple_UDP_testing/*.erl)
BEAM_SIMPLE_UDP_TESTING := $(patsubst src/simple_UDP_testing/%.erl,simpleUDP_ebin/%.beam,${ERL_UDP_FILES})

ERL_SB_FILES := $(wildcard src/server_browser_draft/*.erl)
BEAM_SERVER_BROWSER_DRAFT := $(patsubst src/server_browser_draft/%.erl,serverBrowser_ebin/%.beam,${ERL_SB_FILES})
## end of Erlang files


## Java files - custom (ugly) version 1.0 ##
#JAVA_GAME_FILES := $(wildcard src/Java_game/src/*.java)
#CLASS_JAVA_GAME := $(patsubst src/Java_game/src/%.java,javaGame_jbin/%.class,${JAVA_GAME_FILES})

#JAVA_JINTERFACE_SB_FILES := $(wildcard src/Jinterface_server_browser/src/server_browser/*.java)
#CLASS_JINTERFACE_SB := $(patsubst src/Jinterface_server_browser/src/server_browser/%.java,jinterfaceSB_jbin/%.class,${JAVA_JINTERFACE_SB_FILES})

#JAVA_JINTERFACE_GAME_FILES := $(wildcard src/Jinterface_game/src/*.java)
#CLASS_JINTERFACE_GAME := $(patsubst src/Jinterface_game/src/%.java,jinterfaceGame_jbin/%.class,${JAVA_JINTERFACE_GAME_FILES})
## end of Java files


comma:= ,
empty:=
space:= $(empty) $(empty)

EDOC_SRC := $(filter-out %_test.erl, $(ERL_FILES))
EDOC_SRC_LIST := [$(subst $(space),$(comma),$(patsubst src/%.erl,'src/%.erl', $(EDOC_SRC)))]

#REQUIRED_DIR_NAME := pop_2012_project_group_$(GROUP_NUMBER)
REQUIRED_DIR_NAME := OSM-Project

PROJECT_DIR := $(notdir $(shell pwd))

USER=$(shell whoami)
ARCHIVE_NAME :=  $(REQUIRED_DIR_NAME)_archive_$(USER)_$(shell date "+%Y-%m-%d__%H%M%S")__.tar.gz
ARCHIVE_DIR := ..




all: game_server Java_game Jinterface_server_browser Jinterface_game $(BEAM_SIMPLE_UDP_TESTING) $(BEAM_SERVER_BROWSER_DRAFT) $(BEAM_FILES)

ebin/%.beam: src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<
# $<     -is the name of the first dependency.


## ERLANG CUSTOM COMPILATION (.beam files in ebin folder)
simpleUDP_ebin/%.beam: src/simple_UDP_testing/%.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

serverBrowser_ebin/%.beam: src/server_browser_draft/%.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<
## end of Erlang compilation


## JAVA CUSTOM COMPILATION (.class files in jbin folder)
Java_game:
	$(JAVAC) $(JAVAC_FLAGS) -d jbin src/Java_game/src/*.java

Jinterface_server_browser: 
	$(JAVAC) $(JAVAC_FLAGS) -d jbin src/Jinterface_server_browser/src/server_browser/*.java

Jinterface_game:
	$(JAVAC) $(JAVAC_FLAGS) -d jbin src/Jinterface_game/src/*.java

game_server:
	$(JAVAC) $(JAVAC_FLAGS) -d jbin src/game_server/src/*.java
## end of Java compilation


start: all
	(cd ebin && erl -eval 'foo:start(), init:stop()')

test: all
	(cd ebin && erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()')

doc: $(BEAM_FILES)
	erl -noshell -eval "edoc:files($(EDOC_SRC_LIST), [{dir, 'doc/html'}])" -s init stop

clean:
	rm -fr .#* *.dump
	rm -fr ebin/*.beam
	rm -fr jbin/*.class
	(cd doc/html && find . -name "*" -a ! -name overview.edoc -exec rm -rf {} \;)

remove_finderinfo:
	-xattr -d "com.apple.FinderInfo" src/*.erl include/*.hrl doc/* doc/html/*

archive: clean
ifeq ($(REQUIRED_DIR_NAME), $(PROJECT_DIR))
	(cd $(ARCHIVE_DIR) && tar cvfz $(ARCHIVE_NAME) $(PROJECT_DIR) )
	@echo 
	@echo NOTE: Archive created in $(ARCHIVE_DIR)/$(ARCHIVE_NAME)
	@echo 
else
	@echo Error: Wrong directory name >$(PROJECT_DIR)<, change to >$(REQUIRED_DIR_NAME)<
endif

