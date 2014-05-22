package new_version;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;

import com.ericsson.otp.erlang.*; 

public class Jinterface_bank_client {
	private Socket socket; 
	private OutputStream out;
	private DataOutputStream dos;
	private DataInputStream fromServer;
	
	public Jinterface_bank_client(byte[] host, int port) {
		try {
			this.socket = new Socket (InetAddress.getByAddress(host), port);
			System.out.println("Connected to socket :" + socket);
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
	public void sendTCP(OtpErlangObject arg) {
		OtpOutputStream availableStream = new OtpOutputStream(arg);
		byte[] data = arrayPrepend(availableStream);
		try {
			dos.write(data); /*
			//System.out.println("Sending: " + Arrays.toString( data));
			dos.write(data);
			byte[] message = new byte[2048];
			fromServer.read(message);
			//System.out.println("Got back: " + Arrays.toString(message));

			OtpErlangObject answer = (new OtpInputStream(message)).read_any();
			//System.out.println("Answer: " + answer);
			return answer; */
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		/*} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null; */ 
	}
	
	OtpErlangObject getAnswer() {
		OtpErlangObject answer = null;
		byte[] message = new byte[2048];
		try {
			fromServer.read(message);
			answer = (new OtpInputStream(message)).read_any();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return answer;
	}
		
	public void ping() {
		OtpErlangAtom ping = new OtpErlangAtom("ping");
		sendTCP(ping);
		OtpErlangObject pong = getAnswer();
	}
	
	public void join (OtpErlangPid pid) {
		System.out.println("Joining table: " + pid);
		OtpErlangAtom join = new OtpErlangAtom("join_table");
		OtpErlangObject[] arg = new OtpErlangObject[2];
		arg[0] = join;
		arg[1] = pid;
		OtpErlangTuple tuple = new OtpErlangTuple(arg);
		sendTCP(tuple);
		//OtpErlangObject answer = getAnswer();
		//System.out.println("Result:");
		//System.out.println(answer);
	}
	
	public void add() {
		OtpErlangAtom add = new OtpErlangAtom("add_table");
		sendTCP(add);
		//OtpErlangObject answer = getAnswer();
		//System.out.println(answer);
	}
	
	public void add(String gameName, String gameType, int maxPlayers) {
		OtpErlangAtom add = new OtpErlangAtom("add_table");
		OtpErlangAtom name = new OtpErlangAtom(gameName);
		OtpErlangAtom type = new OtpErlangAtom(gameType);
		OtpErlangInt max = new OtpErlangInt(maxPlayers);
		OtpErlangObject[] arg = {add, name, type, max};
		OtpErlangTuple tuple = new OtpErlangTuple(arg);

		sendTCP(tuple);
		//OtpErlangObject answer  = getAnswer();
		//System.out.println(answer);
	}
	/*
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

*/
	
	public Object[][] available() {

	    OtpErlangAtom availableAtom = new OtpErlangAtom("browse_tables");
		sendTCP(availableAtom);
		OtpErlangObject answer = getAnswer();

	   OtpErlangList tables =  (OtpErlangList) answer;
	   OtpErlangObject [][] tableList = new OtpErlangObject[tables.arity()][5];

		for (int i = 0; i < tables.arity(); i++) {
			for (int j = 0; j < 5; j++) {
				tableList[i][j] =  ((OtpErlangTuple)tables.elementAt(i)).elementAt(j);
			}
		}
		return tableList;
		//System.out.println("Available: " + tableList[0][0]);
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
		sendTCP(arg);
		//OtpErlangObject answer = getAnswer();
		//System.out.println(answer);
	}
	
	private OtpErlangTuple destIpToErlang(int[] destIp) {
		int size = destIp.length;
		OtpErlangObject tmp[] = new OtpErlangObject[size];
		for (int i = 0; i < size; i++) {
			tmp[i] = new OtpErlangInt(destIp[i]);
		}
		return new OtpErlangTuple(tmp);
	}
	
	public void doAction(String action) {
		OtpErlangAtom doAction = new OtpErlangAtom("do_action");
		OtpErlangAtom actionAtom = new OtpErlangAtom(action);
		OtpErlangList options = new OtpErlangList();
		OtpErlangObject[] actionArray = {actionAtom, options};
		OtpErlangTuple actionTuple = new OtpErlangTuple(actionArray);
		OtpErlangObject[] argArray = {doAction, actionTuple};
		OtpErlangTuple argTuple = new OtpErlangTuple(argArray);
		System.out.println("Doing action");
		sendTCP(argTuple);
		System.out.println("Action done");
	}
	// {{Player_move_factor::int, Grid_limit::int, Vel_limit::int, Friction},
	// [{Name_string, {New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}, Hp, Id} | Rest]}
	public int[] getState(){
		OtpErlangAtom getState = new OtpErlangAtom("get_state");
		sendTCP(getState);
		OtpErlangTuple answer = (OtpErlangTuple) getAnswer();
		OtpErlangTuple state = (OtpErlangTuple) answer.elementAt(1);
		System.out.println("State: " + state);
		System.out.println("Player List :" + state.elementAt(1));
		OtpErlangList playerList = (OtpErlangList) state.elementAt(1);
		OtpErlangTuple player = (OtpErlangTuple) playerList.elementAt(0);
		OtpErlangTuple position = (OtpErlangTuple) player.elementAt(1);
		int x = 0;
		int y = 0;
		try {
			x = ((OtpErlangLong)position.elementAt(0)).intValue();
			y = ((OtpErlangLong)position.elementAt(1)).intValue();
		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		int[] pos = {x, y};
		return pos;
		
	}
	
	/*public void move(String name, String dir, int amount) {
		OtpErlangObject argArray[] = new OtpErlangObject[4];

		
		argArray[0] = new OtpErlangAtom("move");
		argArray[1] = new OtpErlangAtom(name);
		argArray[2] = new OtpErlangAtom(dir);
		argArray[3] = new OtpErlangInt(amount);
		
		OtpErlangTuple arg = new OtpErlangTuple(argArray);
		OtpErlangObject answer = sendTCP(arg);
		System.out.println(answer);
	}
	*/
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
	/* public void updatePos(String playerName, PlayerRocket player) {
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
	*/
	public void restart(String name, int x, int y) {
		OtpErlangAtom remove = new OtpErlangAtom("removePlayer");
		OtpErlangAtom playerName = new OtpErlangAtom(name);
		OtpErlangObject[] arg = {remove, playerName};
		OtpErlangTuple tuple = new OtpErlangTuple(arg);
		sendTCP(tuple);
		addPlayer(name, x, y);
	}
	
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