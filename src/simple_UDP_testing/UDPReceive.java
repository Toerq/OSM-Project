/*
  SOURCE: 
  http://www.java2s.com/Code/Java/Network-Protocol/ReceiveUDPpockets.htm
 */

/*
  1>{ok, Socket} = gen_udp:open(7777, [{ip,{127,0,0,1}}, binary]). 
  {ok,#Port<0.511>}
  2> gen_udp:send(Socket, {127,0,0,1}, 8080, "Hello").   

 */


import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.util.Scanner;

public class UDPReceive {
    public static void main(String args[]) {
	try {
	    // Hard coded port
	    int port = 8080;

	    // Create a socket to listen on the port.
	    DatagramSocket dsocket = new DatagramSocket(port);

	    // Create a buffer to read datagrams into. If a
	    // packet is larger than this buffer, the
	    // excess will simply be discarded!
	    byte[] buffer = new byte[2048];

	    // Create a packet to receive data into the buffer
	    DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
	    
	    // Now loop forever, waiting to receive packets and printing them.
	    while (true) {
		// Temporary wait solution 
		Scanner reader = new Scanner(System.in);
		reader.nextInt();
		
		// Wait to receive a datagram
		dsocket.receive(packet);

		// Convert the contents to a string, and display them
		String msg = new String(buffer, 0, packet.getLength());
		System.out.println(packet.getAddress() + ": "
				   + msg);

		// Reset the length of the packet before reusing it.
		packet.setLength(buffer.length);
	    }
	} catch (Exception e) {
	    System.err.println(e);
	}
    }
}
