package server_browser;

import com.ericsson.otp.erlang.*; 

public class Jinterface_bank_client {
	private ErlConnection conn;
	
	public Jinterface_bank_client(String enode, String cookie) {
		this.conn = new ErlConnection("enode", "erlang");
	}
	
	/**
	 * @param args
	 */
	public void available(int[] destIp) {
		int size = destIp.length;
		OtpErlangObject tmp[] = new OtpErlangObject[size];
		for (int i = 0; i < size; i++) {
			tmp[i] = new OtpErlangInt(destIp[i]);
		}
		OtpErlangTuple tuple = new OtpErlangTuple(tmp);
		OtpErlangList arg = new OtpErlangList(tuple);
		conn.sendRPC("bank_client", "available", arg);
		OtpErlangObject received = conn.receiveRPC();
		System.out.println("Server list: \n" + received + "\n");
	}
	
	public static void main(String[] args) {
		Jinterface_bank_client client = new Jinterface_bank_client("enode", "erlang");
		int[] IP = {192,168,1,2};
		client.available(IP);
		client.conn.disconnect();
	}
}