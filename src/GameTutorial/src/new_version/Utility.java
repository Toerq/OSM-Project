package new_version;

import com.ericsson.otp.erlang.OtpOutputStream;


public class Utility {
	
	public static byte[] stringToIp(String s) {
		System.out.println(s);
		String[] tokens = s.replaceFirst("^\\{", "").split("\\.|\\,|\\{|\\}");
		
		byte[] ip = new byte[4];
		for (int i = 0; i < 4; i++) {
			System.out.println(tokens[i]);
			ip[i] = (byte) Integer.parseInt(tokens[i]); 
		}
		return ip;
	}
	public static byte[] arrayPrepend(byte[] tmp) {
		 byte[] prepend = {(byte)131};
		 byte[] data = new byte[prepend.length + tmp.length];
		 System.arraycopy(prepend, 0, data, 0, prepend.length);
		 System.arraycopy(tmp, 0, data, prepend.length, tmp.length);
		return data;
	}
	
	public static byte[] arrayPrepend(OtpOutputStream availableStream) {
		byte[] data = availableStream.toByteArray();
		return arrayPrepend(data);
	}
}