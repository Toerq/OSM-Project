
import java.io.IOException;

import com.ericsson.otp.erlang.*;
 
 
public class ErlConnection {
 
    private static OtpConnection conn;
     public OtpErlangObject received;
     private final String peer;
     private final String cookie;
      
     public static void main(String []args){
         new ErlConnection("enode","erlang");
     }
 
      public ErlConnection(String _peer, String _cookie) {
          peer = _peer;
          cookie = _cookie;
          connect();
 
           /*Do Calls to Rpc methods and then close the connection*/
          //disconnect();
 
      }
 
      private void connect() {
       System.out.print("Please wait, connecting to "+peer+"....\n");
 
       String javaClient ="java";
       try {
         OtpSelf self = new OtpSelf(javaClient, cookie.trim());
         OtpPeer other = new OtpPeer(peer.trim());
         System.out.println(other);
         conn = self.connect(other);
         System.out.println("Connection Established with "+peer+"\n");
       }
       catch (Exception exp) {
         System.out.println("connection error is :" + exp.toString());
         exp.printStackTrace();
       }
 
     }
      
      
      public void sendRPC(String mod, String fun, OtpErlangList arg) {
    	  try {
			conn.sendRPC(mod, fun, arg);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
      }
      
      public OtpErlangObject receiveRPC() {
			try {
				return conn.receiveRPC();
			} catch (OtpErlangExit e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (OtpAuthException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
    	  return null;
      }
      
 
     public void disconnect() {
       System.out.println("Disconnecting....");
       if(conn != null){
         conn.close();
       }
       System.out.println("Successfuly Disconnected");
     }
 
}