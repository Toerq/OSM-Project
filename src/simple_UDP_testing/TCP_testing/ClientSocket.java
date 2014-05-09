// Client Side
import java.io.*;
import java.net.*;

public class ClientSocket {
    public void run() {
	try {
	    int serverPort = 4020;
	    InetAddress host = InetAddress.getByName("127.0.0.1"); 
	    System.out.println("Connecting to server on port " + serverPort); 

	    Socket socket = new Socket(host,serverPort); 
	    //Socket socket = new Socket("127.0.0.1", serverPort);
	    System.out.println("Just connected to " + socket.getRemoteSocketAddress()); 
	    PrintWriter toServer = 
		new PrintWriter(socket.getOutputStream(),true);
	    // BufferedReader fromServer = 
	    // new BufferedReader(new InputStreamReader
	    // (socket.getInputStream()));
	    
	    toServer.println("Hello from " + socket.getLocalSocketAddress()); 
	    
	    /* DataInputStream to read from TCP */
	    DataInputStream fromServer = 
		new DataInputStream(socket.getInputStream());
	    int receivedSize = fromServer.readInt();
	    byte[] message = new byte[receivedSize];
	    System.out.println("Size: " + receivedSize);
	    fromServer.readFully(message);
	    
	    int pos0 = message[0];
	    int pos1 = message[1];
	    int pos2 = message[2];
	    int pos3 = message[3];

	    // readLine waits for <<10>>, or "\n"
	    // String line = fromServer.readLine();
	    System.out.println("Client received: " + "<<" + pos0 + "," + 
			       pos1 + "," + pos2 + "," +  pos3 + 
			       ">> from Server");
	    
	    System.out.println("Press ENTER to close the connection.");
	    System.in.read();
	    
	    toServer.close();
	    fromServer.close();
	    socket.close();
	}
	catch(UnknownHostException ex) {
	    ex.printStackTrace();
	}
	catch(IOException e){
	    e.printStackTrace();
	}
    }
	
    public static void main(String[] args) {
	ClientSocket client = new ClientSocket();
	client.run();
    }
}
