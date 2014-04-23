package jinterface_server_brower;

import com.ericsson.otp.erlang.*; 

public class Jinterface_bank_client {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		//ErlConnection bank_client = 
		ErlConnection java_client = new ErlConnection("enode", "erlang");
		java_client.disconnect();
	}

}
