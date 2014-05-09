

/*
  Send from erlang:
  
  1> {ok, Sock} = gen_tcp:connect({IP}, 4020, [{nodelay, true}]).
  {ok,#Port<0.516>}
  2> gen_tcp:send(Sock, <<97,98,99,100>>).
  ok
  3> gen_tcp:close(Sock).
  
 */



// Server Side
import java.net.*;
import java.io.*;

public class ServerSideSocket { 
    public void run() {
	try {
	    int serverPort = 4020;
	    ServerSocket serverSocket = new ServerSocket(serverPort);
	    //serverSocket.setSoTimeout(10000); 
	    
	    while(true) {
		System.out.println("Waiting for client on port " + 
				   serverSocket.getLocalPort() + "..."); 

		Socket server = serverSocket.accept();
		System.out.println("Just connected to " + 
				   server.getRemoteSocketAddress()); 

		PrintWriter toClient = 
		    new PrintWriter(server.getOutputStream(),true);
		
		BufferedReader fromClient =
		    new BufferedReader(new InputStreamReader
				       (server.getInputStream()));
		System.out.println("Waiting for a message...");
		String line = fromClient.readLine();
		
		System.out.println("Server received: " + line); 
		toClient.println("Thank you for connecting to " + 
				 server.getLocalSocketAddress() + 
				 "\nGoodbye!"); 
	    }
	}
	catch(UnknownHostException ex) {
	    ex.printStackTrace();
	}
	catch(IOException e){
	    e.printStackTrace();
	}
    }
	
    public static void main(String[] args) {
	ServerSideSocket srv = new ServerSideSocket();
	srv.run();
    }
}
