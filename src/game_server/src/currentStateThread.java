import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpInputStream;
import com.ericsson.otp.erlang.OtpOutputStream;

public class currentStateThread implements Runnable {
	private Socket socket;
	private DataInputStream fromServer;
	private DataOutputStream toServer;
	private OtpErlangTuple argument;
	private OtpErlangObject currentState;
	
	public currentStateThread(String host, int port, OtpErlangTuple arg) 
	{
		try {
			this.argument = arg;
			this.socket = new Socket (InetAddress.getByName(host), port);
			this.toServer = new DataOutputStream(socket.getOutputStream());
			this.fromServer = new DataInputStream(socket.getInputStream());
			
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void run() 
	{
		//System.out.println("Working in currentStateThread......");
		OtpOutputStream stateStream = new OtpOutputStream(argument);
		byte[] data = arrayPrepend(stateStream);
		byte[] message = new byte[74];
		try {
			while(true) {
				
				toServer.write(data);
				
				fromServer.read(message);
				
				OtpInputStream otpInputStream = new OtpInputStream(message);
				currentState = otpInputStream.read_any();
				otpInputStream.close();
				
			}
			//return answer;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		//return null;
	}
	
	public OtpErlangObject getCurrentState() {
		return currentState;
	}
	
	private byte[] arrayPrepend(OtpOutputStream availableStream) {
		byte[] tmp = availableStream.toByteArray();
		byte[] prepend = {(byte)131};
		byte[] data = new byte[prepend.length + tmp.length];
		System.arraycopy(prepend, 0, data, 0, prepend.length);
		System.arraycopy(tmp, 0, data, prepend.length, tmp.length);
		return data;
	}
}
