package moon_lander;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;

import com.ericsson.otp.erlang.*; 

public class Jinterface_bank_client {
	private Socket socket; 
	private OutputStream out;
	private DataOutputStream dos;
	private DataInputStream fromServer;
	
	public Jinterface_bank_client(byte[] host, int port) {
		try {
			this.socket = new Socket (InetAddress.getByAddress(host), port);
			this.out = socket.getOutputStream();
			this.dos = new DataOutputStream(out);
			this.fromServer = new DataInputStream(socket.getInputStream());
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void initMailbox(Jinterface_bank_client client) {
		
	}
	public OtpErlangObject sendTCP(OtpErlangObject arg) {
		OtpOutputStream availableStream = new OtpOutputStream(arg);
		byte[] data = arrayPrepend(availableStream);
		try {
			dos.write(data);
			byte[] message = new byte[248];
			fromServer.read(message);

			OtpErlangObject answer = (new OtpInputStream(message)).read_any();
			return answer;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	public void add(String servername, int[] ip) {
		OtpErlangObject[] argArray = new OtpErlangObject[3];
		OtpErlangObject add = new OtpErlangAtom("add");
		argArray[0] = add;
		OtpErlangObject sName = new OtpErlangAtom(servername);
		argArray[1] = sName;
		
		OtpErlangTuple ipTuple = destIpToErlang(ip);
		argArray[2] = ipTuple;
		
		OtpErlangTuple dataTuple = new OtpErlangTuple(argArray);
		 OtpErlangObject answer = sendTCP(dataTuple);
		 System.out.println(answer);
	}


	
	public String[][] available() {

	    OtpErlangTuple availableTuple = new OtpErlangTuple(new OtpErlangAtom("available"));
		OtpErlangObject answer = sendTCP(availableTuple);

	    OtpErlangList servers =  (OtpErlangList) ((OtpErlangTuple) answer).elementAt(1);
	    String [][] serverList = new String[servers.arity()][2];

		for (int i = 0; i < servers.arity(); i++) {
			for (int j = 0; j < 2; j++) {
				serverList[i][j] =  ((OtpErlangTuple)servers.elementAt(i)).elementAt(j+1).toString();
			}
		}
		return serverList;
	}
	
	public void addPlayer(String playerName, int x, int y) {
		OtpErlangObject[] argArray = new OtpErlangObject[4];
		OtpErlangAtom addPlayer = new OtpErlangAtom("addPlayer");
		OtpErlangAtom erlName = new OtpErlangAtom(playerName);
		OtpErlangInt erlangX = new OtpErlangInt(x);
		OtpErlangInt erlangY = new OtpErlangInt(y);

		argArray[0] = addPlayer;
		argArray[1] = erlName;
		argArray[2] = erlangX;
		argArray[3] = erlangY;
		
		OtpErlangTuple arg = new OtpErlangTuple(argArray);
		OtpErlangObject answer = sendTCP(arg);
		System.out.println(answer);
	}
	
	private OtpErlangTuple destIpToErlang(int[] destIp) {
		int size = destIp.length;
		OtpErlangObject tmp[] = new OtpErlangObject[size];
		for (int i = 0; i < size; i++) {
			tmp[i] = new OtpErlangInt(destIp[i]);
		}
		return new OtpErlangTuple(tmp);
	}
	
	public void move(String name, String dir, int amount) {
		OtpErlangObject argArray[] = new OtpErlangObject[4];

		
		argArray[0] = new OtpErlangAtom("move");
		argArray[1] = new OtpErlangAtom(name);
		argArray[2] = new OtpErlangAtom(dir);
		argArray[3] = new OtpErlangInt(amount);
		
		OtpErlangTuple arg = new OtpErlangTuple(argArray);
		OtpErlangObject answer = sendTCP(arg);
		System.out.println(answer);
	}
/*
	public ArrayList<Player> getAllPos() {
		OtpErlangTuple arg = new OtpErlangTuple(new OtpErlangAtom("getAllPos"));
		OtpErlangObject answer = sendTCP(arg);
		System.out.println("recieved from getAllPos: " + answer);
		OtpErlangList erlangPlayerList = (OtpErlangList) ((OtpErlangTuple) answer).elementAt(1);
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
	}
*/
	public void updatePos(String playerName, PlayerRocket player) {
				OtpErlangObject argArray[] = new OtpErlangObject[2];
				OtpErlangAtom erlName = new OtpErlangAtom(playerName);
				
				argArray[0] = new OtpErlangAtom("getPos");
				argArray[1] = erlName;
				OtpErlangTuple arg = new OtpErlangTuple(argArray);
				System.out.println("arg: " + arg);

				OtpErlangTuple answer = (OtpErlangTuple) sendTCP(arg);
				OtpErlangTuple coordinates = (OtpErlangTuple) answer.elementAt(1);
				int x;
				int y;
				try {
					x =  ((OtpErlangLong) coordinates.elementAt(0)).intValue();
					y =  ((OtpErlangLong) coordinates.elementAt(1)).intValue();
					player.setCoordinates(x, y);
					System.out.println(x);
					System.out.println(y);
				} catch (OtpErlangRangeException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
	
	public void restart(int x, int y) {
		OtpErlangTuple clear = new OtpErlangTuple(new OtpErlangAtom("clear"));
		sendTCP(clear);
		addPlayer("player1", x, y);
	}
	
	/*public static void main(String[] args) {
		Jinterface_bank_client client = new Jinterface_bank_client("127.0.0.1", 3010);
		//int[] ip = {127,0,0,1};
		int[] newServerIp = {12,3,4,5};
		client.add("Ex5", newServerIp); //, ip);
		String[][] serverList = client.available();
		for (int i = 0; i < serverList.length; i++) {
			System.out.println(serverList[i][0] + serverList[i][1]);
		}
		client.addPlayer("Player1");
		client.move("player1", "up", 5);
		client.getAllPos();
		try {
			client.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		//client.available(ip);
		//client.conn.disconnect();
	}
*/
	private void close() throws IOException {
		this.out.close();
		this.dos.close();
		this.fromServer.close();
		this.socket.close();
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