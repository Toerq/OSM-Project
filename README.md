==== OSM-Project ====

==== COURSE ==== 

Process Oriented Programming (1DT049) Spring 2014

Department of Information Technology 
Uppsala university


==== GROUP ==== 

10


==== PROJECT NAME ==== 

GEESE (Generic Erlang Server)


==== GROUP MEMBERS ==== 
930922-3452 Johan.Gille.8100@student.uu.se
880615-6215 Niklas.Hokenstrom.6511@student.uu.se
900215-5217 Valle.Magnusson.5911@student.uu.se
920927-2179 Jonas.Nilson.8705@student.uu.se
920104-0616 Christian.Tornqvist.2175@student.uu.se

==== MAY THE SOURCE BE WITH YOU ==== 

Use 'git clone https://github.com/Toerq/OSM-Project.git' to get the latest version of the project.

==== ERLANG VERSION ====

The server was developed and tested using Erlang R15B01 and Erlagn R16B

==== JAVA VERSION ====

The client was developed and tested using Java 6
     	      	  	    	       
==== MAKE IT HAPPEN ==== 

Using the make utility you can perform the following actions:

make client             ==> Runs the client jar file
make server             ==> Compiles and runs the server
make geese_compile      ==> Compiles the Erlang source files if necessary. 
make archive            ==> Creates a gziped tar archive of this directory. 
make clean              ==> Removes all beam files and html files generated by Edoc and Javadoc.
make doc                ==> Generates Edoc and Javadoc documentation in the doc/html directory.
make test               ==> Runs all tests.


==== TO COMPILE ==== 

To compile the project, simply type make and press enter.


==== TO RUN AND TEST THE SYSTEM ==== 
To run the server type 'make server' when standing in the root directory of the project. 
When prompted type 'geese_sup:start_link(<port number>)' to set up the server.

To run the client type 'make client' when standing in the root directory of the project.
To connect to a local server press connect to ip and choose the ip "127.0.0.1" and the port numer you choose when setting up the server.

