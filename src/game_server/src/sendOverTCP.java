import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Arrays;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpInputStream;
import com.ericsson.otp.erlang.OtpOutputStream;


public class sendOverTCP {
	
	public static void main(String[] args) {
		try {
			// 130.243.179.21 - C
			// 130.243.202.14 - J
			Socket socket = new Socket(InetAddress.getByName("130.243.179.21"), 4444);
			System.out.println("Connected to socket: " + socket);
			
			DataOutputStream toServer = new DataOutputStream(socket.getOutputStream());
			OtpErlangAtom arg = new OtpErlangAtom("ping");
			
			OtpOutputStream stateStream = new OtpOutputStream(arg);
			byte[] data = arrayPrepend(stateStream);
			//System.out.println("DataOutputStream write: " + data);
			System.out.println("Sending: " + Arrays.toString(data));
			toServer.write(data);
			
			DataInputStream fromServer = new DataInputStream(socket.getInputStream());
			byte[] message = new byte[1024];
			fromServer.read(message);
			//System.out.println("DataInputStream read: " + message);
			System.out.println("Received: " + Arrays.toString(message));
			
			OtpInputStream otpInputStream = new OtpInputStream(message);
			OtpErlangObject answerFromServer = otpInputStream.read_any();
			System.out.println("OTPERLANGOBJECT: " + answerFromServer);
			
			while(true) {
				;
			}
			
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	static private byte[] arrayPrepend(OtpOutputStream availableStream) {
		byte[] tmp = availableStream.toByteArray();
		System.out.println("tmp = " + Arrays.toString(tmp));
		byte[] prepend = {(byte)131};
		System.out.println("prepend = " + Arrays.toString(prepend));
		byte[] data = new byte[prepend.length + tmp.length];
		System.out.println("data = " + Arrays.toString(data));
		System.arraycopy(prepend, 0, data, 0, prepend.length);
		System.arraycopy(tmp, 0, data, prepend.length, tmp.length);
		System.out.println("data after: " + Arrays.toString(data));
		return data;
	}
}
