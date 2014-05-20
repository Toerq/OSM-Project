package new_version;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class Test {
	
	
	public static void main(String []args) {
		
		byte[] ip = {127,0,0,1};
		//byte[] ip = {(byte) 212, 25,(byte) 151, (byte) 178};
		Jinterface_bank_client client = new Jinterface_bank_client(ip, 3011);
		
		OtpErlangAtom addTable = new OtpErlangAtom("add_table");
		client.sendTCP(addTable);
		try {
			Thread.sleep(500);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		//OtpErlangAtom availableTuple = new OtpErlangAtom("browse_tables");
		//OtpErlangObject answer = client.sendTCP(availableTuple);
		//System.out.println("Answer :" + answer);
		System.out.println();
		System.out.println("Available: ");
		Object[][] available = client.available();
		for (int i = 0; i < available.length; i ++) {
			for (int j = 0; j < 5;  j++) {
				System.out.print(available[i][j] + ", ");
			}
			System.out.println();
		}
		while(true) {
		}
	}
}
