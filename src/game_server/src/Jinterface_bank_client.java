
import java.util.ArrayList;

import com.ericsson.otp.erlang.*; 

public class Jinterface_bank_client {
	private ErlConnection conn;
	
	public Jinterface_bank_client(String enode, String cookie) {
		this.conn = new ErlConnection(enode, cookie);
	}
	
	public void initMailbox(Jinterface_bank_client client) {
		
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
		OtpErlangObject[] argArray = new OtpErlangObject[2];

		int size = destIp.length;
		OtpErlangObject tmp[] = new OtpErlangObject[size];
		for (int i = 0; i < size; i++) {
			tmp[i] = new OtpErlangInt(destIp[i]);
		}
		OtpErlangAtom erlName = new OtpErlangAtom(playerName);
		OtpErlangList arg = new OtpErlangList(erlName);
		conn.sendRPC("game_client", "addPlayer", arg);
		OtpErlangObject received = conn.receiveRPC();
	}
	
	public void move(String name, String dir, int amount) {
		OtpErlangObject argArray[] = new OtpErlangObject[3];
		
		argArray[0] = new OtpErlangAtom(name);
		argArray[1] = new OtpErlangAtom(dir);
		argArray[2] = new OtpErlangInt(amount);
		
		OtpErlangList argList = new OtpErlangList(argArray);

		conn.sendRPC("game_client", "move", argList);
		OtpErlangObject received = conn.receiveRPC();
		
	}

	public ArrayList<Player> getAllPos() {
		OtpErlangObject argArray[] = new OtpErlangObject[0];
		OtpErlangList argList = new OtpErlangList(argArray);
		conn.sendRPC("game_client", "getAllPos", argList);
		
		OtpErlangObject received = conn.receiveRPC();
		OtpErlangList erlangPlayerList = (OtpErlangList) ((OtpErlangTuple) received).elementAt(1);
        ArrayList<Player> playerList = new ArrayList<Player>();
        for(OtpErlangObject erlangPlayer : erlangPlayerList) {
        	int x = 0;
        	int y = 0;
        	System.out.println("print av erlangPlayer: " + erlangPlayer);
        	String playerName =  ((OtpErlangTuple) erlangPlayer).elementAt(1).toString();
        	try {
				x = ((OtpErlangLong) ((OtpErlangTuple) erlangPlayer).elementAt(2)).intValue();
				y = ((OtpErlangLong) ((OtpErlangTuple) erlangPlayer).elementAt(3)).intValue();
			} catch (OtpErlangRangeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        	System.out.println("X: " + x);
        	playerList.add(new Player(x, y, playerName));
        }

        return playerList;
//		OtpErlangList erlangPlayerList = (OtpErlangList) received;
//		OtpErlangObject[] list = erlangPlayerList.elements();
//		int length = list.length;
//		
//		System.out.println(erlangPlayerList.elementAt(0));
	}

	public void updatePos(String playerName, Player player) {

				OtpErlangAtom erlName = new OtpErlangAtom(playerName);
				OtpErlangList arg = new OtpErlangList(erlName);
				
				conn.sendRPC("game_client", "getPos", arg);
				OtpErlangTuple received = (OtpErlangTuple) conn.receiveRPC();
				OtpErlangTuple coordinates = (OtpErlangTuple) received.elementAt(1);
				int x;
				int y;
				//System.out.println("REC: " + received);
				//OtpErlangTuple tuple = (OtpErlangTuple) received;
				//System.out.println("tuple: " + tuple.elementAt(1));
				//System.out.println("asd: " + received.toString());
				try {
					x =  ((OtpErlangLong) coordinates.elementAt(0)).intValue();
					y =  ((OtpErlangLong) coordinates.elementAt(1)).intValue();
					//x = (OtpErlangTuple) received.elementAt(1);
					//x = ((OtpErlangLong ((OtpErlangTuple) received.elementAt(1))).intValue();
					//x = ((OtpErlangLong)received).intValue();
					player.setCoordinates(x, y);
					System.out.println(x);
					System.out.println(y);
				} catch (OtpErlangRangeException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		//		int[] arr = new int[2];
		//		try {
		//			arr[0] = ((OtpErlangInt)(tuple.elementAt(0))).intValue();
					
		//			return arr;
		//		} catch (OtpErlangRangeException e) {
		//			// TODO Auto-generated catch block
		//			e.printStackTrace();
		//		}
			}
	
	
	
//	public static void main(String[] args) {
//		Jinterface_bank_client client = new Jinterface_bank_client("enode", "erlang");
//		int[] ip = {127,0,0,1};
//		int[] newServerIp = {1,2,3,4};
//		client.add("hejsan123", newServerIp, ip);
//		client.available(ip);
//		client.conn.disconnect();
//	}
}