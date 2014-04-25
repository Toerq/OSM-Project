
import com.ericsson.otp.erlang.*; 

public class Jinterface_bank_client {
	private ErlConnection conn;
	
	public Jinterface_bank_client(String enode, String cookie) {
		this.conn = new ErlConnection(enode, cookie);
	}
	
	
	public void add(String servername, int[] ip, int[] destIp) {
		OtpErlangObject[] argArray = new OtpErlangObject[3];
		OtpErlangObject sName = new OtpErlangAtom(servername);
		argArray[0] = sName;
		
		int size = ip.length;
		OtpErlangObject tmp[] = new OtpErlangObject[size];
		for (int i = 0; i < size; i++) {
			tmp[i] = new OtpErlangInt(ip[i]);
		}
		OtpErlangTuple IpTuple = new OtpErlangTuple(tmp);
		argArray[1] = IpTuple;
		
		size = destIp.length;
		OtpErlangObject tmp2[] = new OtpErlangObject[size];
		for (int i = 0; i < size; i++) {
			tmp2[i] = new OtpErlangInt(destIp[i]);
		}
		OtpErlangTuple destIpTuple = new OtpErlangTuple(tmp2);
		argArray[2] = destIpTuple;
		
		OtpErlangList argList = new OtpErlangList(argArray);
		conn.sendRPC("bank_client", "add", argList);
		OtpErlangObject received = conn.receiveRPC();
		System.out.println(received + "\n");
	}
	
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
	
	public void addPlayer(int[] destIp, String playerName) {
		int size = destIp.length;
		OtpErlangObject tmp[] = new OtpErlangObject[size];
		for (int i = 0; i < size; i++) {
			tmp[i] = new OtpErlangInt(destIp[i]);
		}
		OtpErlangAtom erlName = new OtpErlangAtom(playerName);
		OtpErlangList arg = new OtpErlangList(erlName);
		conn.sendRPC("game_logic", "addPlayer", arg);
		OtpErlangObject received = conn.receiveRPC();
		System.out.println("Outcome: " + received);
	}
	
	public void movePlayer(String name, String dir, int amount) {
		OtpErlangObject argArray[] = new OtpErlangObject[3];
		
		argArray[0] = new OtpErlangAtom(name);
		argArray[1] = new OtpErlangAtom(dir);
		argArray[2] = new OtpErlangInt(amount);
		
		OtpErlangList argList = new OtpErlangList(argArray);

		conn.sendRPC("game_logic", "movePlayer", argList);
		OtpErlangObject received = conn.receiveRPC();
		System.out.println("Outcome: " + received);
	}
	
	public int getPos(String name) {
		OtpErlangObject argArray[] = new OtpErlangObject[1];
		argArray[0] = new OtpErlangAtom(name);
		OtpErlangList argList = new OtpErlangList(argArray);
		
		conn.sendRPC("game_logic", "getPos", argList);
		OtpErlangObject received = conn.receiveRPC();
		int x;
		
		System.out.println("asd: " + received.toString());
		try {
			x = ((OtpErlangLong) received).intValue();
			return x;
		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
//		int[] arr = new int[2];
//		try {
//			arr[0] = ((OtpErlangInt)(tuple.elementAt(0))).intValue();
//			arr[1] = ((OtpErlangInt)(tuple.elementAt(0))).intValue();
//			return arr;
//		} catch (OtpErlangRangeException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
		return 0;
	}
	
//	public static void main(String[] args) {
//		Jinterface_bank_client client = new Jinterface_bank_client("enode", "erlang");
//		int[] ip = {212,25,148,32};
//		int[] newServerIp = {1,2,3,4};
//		client.add("hejsan", newServerIp, ip);
//		client.available(ip);
//		client.conn.disconnect();
//	}
}