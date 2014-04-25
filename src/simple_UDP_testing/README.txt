~Instructions~

(You need two terminals)

##########################################################
Terminal 1 (standing in the ../simple_UDP_testing folder):
erl
c(udp_test).
udp_test:server(53530).

* the server is now running * 

Terminal 2 (standing in the ../simple_UDP_testing folder):
erl
udp_test:startclient().
##########################################################

You should now see some output in Terminal 1, 
sent from the client in Terminal 2.


