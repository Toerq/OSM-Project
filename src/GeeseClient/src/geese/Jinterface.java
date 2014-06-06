package geese;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Enumeration;

import javax.swing.JOptionPane;

import com.ericsson.otp.erlang.*; 

/**
 * Handle the communication between the client application and the server
 * 
 * @author Niklas Hokenstrom, Jonas Nilson
 */
public class Jinterface {
	private Socket socket; 
	private OutputStream out;
	private DataOutputStream dos;
	private DataInputStream fromServer;

	/**
	 * Creates a new Jinterface communication object with a TCP socket 
	 * connected to server: (host, port), wher host is the servers ip and port is the portnumber
	 * @param host The ip adress that Jinterface object will connect to
	 * @param port The port number that the Jinterface object will connect to
	 */
	public Jinterface(byte[] host, int port) {
		try {			
			this.socket = new Socket();
			socket.connect(new InetSocketAddress(InetAddress.getByAddress(host), port), 1000);
			socket.setSoTimeout(5000);

			this.out = socket.getOutputStream();
			this.dos = new DataOutputStream(out);
			this.fromServer = new DataInputStream(socket.getInputStream());
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			Main.ip = null;
			JOptionPane.showMessageDialog(null, "Could not connect");
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			Main.ip = null;
			JOptionPane.showMessageDialog(null, "Could not connect");
			e.printStackTrace();
		}
	}

	/**
	 * Sends arg via TCP to the server
	 * 
	 * @param arg Argument sent to the server
	 */
	public void send(OtpErlangObject arg) {
		OtpOutputStream availableStream = new OtpOutputStream(arg);
		byte[] data = Utility.arrayPrepend(availableStream);
		try {
			dos.write(data);

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Reads the answer from the server via TCP
	 * 
	 * @return answer
	 */
	OtpErlangObject getAnswer() {
		OtpErlangObject answer = null;
		byte[] message = new byte[2048];
		try {
			fromServer.read(message);
			answer = (new OtpInputStream(message)).read_any();
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return answer;
	}

	/**
	 * Sends the atom 'ping' to the server
	 */
	public void ping() {
		OtpErlangAtom ping = new OtpErlangAtom("ping");
		send(ping);
		OtpErlangObject pong = getAnswer();
	}



	/**
	 * Attempts to join a table (game session)
	 * 
	 * @param pid The pid of the table to join
	 * @return true if join was successful, else false
	 */
	public boolean join (OtpErlangPid pid) {
		OtpErlangAtom join = new OtpErlangAtom("join_table");
		OtpErlangObject[] arg = new OtpErlangObject[2];
		arg[0] = join;
		arg[1] = pid;
		OtpErlangTuple tuple = new OtpErlangTuple(arg);
		send(tuple);
		OtpErlangObject answer = getAnswer();
		if (answer.toString().equals("join_succeeded")) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 * Adds a table (game session) to the server
	 * 
	 * @param gameName title of the table
	 * @param gameType type of game
	 * @param maxPlayers Maximum number of players
	 */
	public void add(String gameName, String gameType, int maxPlayers) {
		OtpErlangAtom add = new OtpErlangAtom("add_table");
		OtpErlangAtom name = new OtpErlangAtom(gameName);
		OtpErlangAtom type = new OtpErlangAtom(gameType);
		OtpErlangInt max = new OtpErlangInt(maxPlayers);
		OtpErlangObject[] arg = {add, name, type, max};
		OtpErlangTuple tuple = new OtpErlangTuple(arg);
		send(tuple);
	}

	/**
	 * Requests the available tables (game sessions) on the server
	 * 
	 * @return Available servers (Object[][])
	 */
	public Object[][] available() {

		OtpErlangAtom availableAtom = new OtpErlangAtom("browse_tables");
		send(availableAtom);
		OtpErlangObject answer = getAnswer();

		OtpErlangList tables =  (OtpErlangList) answer;
		OtpErlangObject [][] tableList = new OtpErlangObject[tables.arity()][5];

		for (int i = 0; i < tables.arity(); i++) {
			for (int j = 0; j < 5; j++) {
				tableList[i][j] =  ((OtpErlangTuple)tables.elementAt(i)).elementAt(j);
			}
		}
		return tableList;
	}

	/**
	 * Requests to change the name of the player
	 * 
	 * @param name Requested player name
	 */
	public void setName(String name) {
		OtpErlangAtom changeName = new OtpErlangAtom("change_name");
		OtpErlangString newName = new OtpErlangString(name);
		OtpErlangObject[] arg = {changeName, newName};
		OtpErlangTuple tuple = new OtpErlangTuple(arg);
		send(tuple);
	}

	/**
	 * Leaves the game (by leaving both the logic game state and the actual table)
	 * 
	 */
	public void removePlayer () {		
		// Leaves the table (and state)
		OtpErlangAtom leave = new OtpErlangAtom("leave_game");
		send(leave);	
	}

	/**
	 * Requests the game to restart
	 */
	public void requestRestart () {
		OtpErlangObject[] argArray = {new OtpErlangAtom("request_restart"), new OtpErlangAtom("argument")};
		OtpErlangList argList = new OtpErlangList(argArray);
		doAction("server", argList);
	}

	/**
	 * Requests to do an action in game state (ex. move, fire)
	 * 
	 * @param action type of action (ex. 'server', 'move')
	 * @param argList Arguments for the requested action (ex. 'respawn_player', 'right'
	 */
	public void doAction(String action, OtpErlangList argList) {
		OtpErlangAtom doAction = new OtpErlangAtom("do_action");
		OtpErlangAtom actionAtom = new OtpErlangAtom(action);
		OtpErlangObject[] actionArray = {actionAtom, argList};
		OtpErlangTuple actionTuple = new OtpErlangTuple(actionArray);
		OtpErlangObject[] argArray = {doAction, actionTuple};
		OtpErlangTuple argTuple = new OtpErlangTuple(argArray);
		send(argTuple);
	}



	/**
	 * Requests the current game state from the server
	 * 
	 * @return The current game state
	 */
	public OtpErlangTuple getState() {
		OtpErlangAtom getState = new OtpErlangAtom("get_state");
		send(getState);
		OtpErlangTuple answer = (OtpErlangTuple) getAnswer();
		while (answer == null) {
			answer = getState();
		}
		OtpErlangTuple state = (OtpErlangTuple) answer.elementAt(1);
		return state;
	}

	/**
	 * Reads the level design from the state 
	 * 
	 * @param state The current state
	 */
	public void updateLevelList (OtpErlangTuple state) {
		OtpErlangList levelList = (OtpErlangList) state.elementAt(2);
		OtpErlangObject[] levelArray = levelList.elements();
		int size = levelArray.length;
		int[][] boxes = new int[size][4];
		OtpErlangTuple box;
		for (int i = 0; i < size; i++) {
			try {
				box = (OtpErlangTuple) levelArray[i];
				boxes[i][0] = ((OtpErlangLong)((OtpErlangTuple) box.elementAt(0)).elementAt(0)).intValue();
				boxes[i][1] = ((OtpErlangLong)((OtpErlangTuple) box.elementAt(0)).elementAt(1)).intValue();
				boxes[i][2] = ((OtpErlangLong)((OtpErlangTuple) box.elementAt(1)).elementAt(0)).intValue();
				boxes[i][3] = ((OtpErlangLong)((OtpErlangTuple) box.elementAt(1)).elementAt(1)).intValue();
			} catch (OtpErlangRangeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		Game.boxes = boxes;
	}

	/**
	 * Updates the games bullet list with new bullets from the state
	 * 
	 * @param state The current state
	 */
	public void updateBulletList(OtpErlangTuple state) {
		OtpErlangList bulletList = (OtpErlangList) state.elementAt(1);
		OtpErlangObject[] bulletArray = bulletList.elements();
		int size = bulletArray.length;
		Integer[][] bullets = new Integer[size][5];
		OtpErlangTuple bullet;
		int bulletLifeTime = 10;
		for (int i = 0; i < size; i++) {
			try {
				bullet = (OtpErlangTuple) bulletArray[i];
				bullets[i][0] = ((OtpErlangLong)((OtpErlangTuple) bullet.elementAt(1)).elementAt(0)).intValue();
				bullets[i][1] = ((OtpErlangLong)((OtpErlangTuple) bullet.elementAt(1)).elementAt(1)).intValue();
				bullets[i][2] = ((OtpErlangLong)((OtpErlangTuple) bullet.elementAt(2)).elementAt(0)).intValue();
				bullets[i][3] = ((OtpErlangLong)((OtpErlangTuple) bullet.elementAt(2)).elementAt(1)).intValue();
				bullets[i][4] = bulletLifeTime;
			} catch (OtpErlangRangeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		Game.bullets = bullets;
	}

	/**
	 * Updates the games player list with new information from the state
	 * 
	 * @param state The current state
	 */
	public void updatePlayerList(OtpErlangTuple state) {
		OtpErlangList playerList = (OtpErlangList) state.elementAt(0);
		OtpErlangObject[] playerArray = playerList.elements();
		int size = playerArray.length;
		int[] id = new int[size];

		for(int i = 0; i < size; i++) {
			updatePlayer(playerArray, i, id);
		}

		Enumeration<Integer> enumKey = Game.players.keys();
		while(enumKey.hasMoreElements()) {
			Integer key = enumKey.nextElement();
			if(!(Utility.contains(id,key))) {
				Game.players.remove(key);
			}
		}
	}

	/**
	 * Updates the player in playerArray with index i.
	 * 
	 * @param state The current state
	 */
	private void updatePlayer(OtpErlangObject[] playerArray, int i, int[] idArray) {
		try {
			OtpErlangTuple player = (OtpErlangTuple) playerArray[i];
			int id = ((OtpErlangPid) player.elementAt(6)).id();
			idArray[i] = id;
			String name = ((OtpErlangString) player.elementAt(0)).toString();
			int x = ((OtpErlangLong)((OtpErlangTuple) player.elementAt(1)).elementAt(0)).intValue();
			int y = ((OtpErlangLong)((OtpErlangTuple) player.elementAt(1)).elementAt(1)).intValue();
			int xVelocity = ((OtpErlangLong)((OtpErlangTuple) player.elementAt(2)).elementAt(0)).intValue();
			int hp = ((OtpErlangLong)player.elementAt(3)).intValue();
			int power = ((OtpErlangLong)player.elementAt(4)).intValue();
			int wins = ((OtpErlangLong)((OtpErlangTuple) player.elementAt(5)).elementAt(0)).intValue();
			int kills = ((OtpErlangLong)((OtpErlangTuple) player.elementAt(5)).elementAt(1)).intValue();
			int deaths = ((OtpErlangLong)((OtpErlangTuple) player.elementAt(5)).elementAt(2)).intValue();

			if(!(Game.players.containsKey(id))){
				Game.players.put(id, new Player(id, name));
			}
			if(id == Game.myId) {
				Game.myPos[0] = x;
				Game.myPos[1] = y;
			}

			Game.players.get(id).setPos(x, y);
			Game.players.get(id).setVel(xVelocity);
			Game.players.get(id).setHP(hp);
			Game.players.get(id).setPower(power);

			Game.players.get(id).setWins(wins);
			Game.players.get(id).setKills(kills);
			Game.players.get(id).setDeaths(deaths);

		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Get a new game state from the server and updates the clients local state 
	 */
	public void updateState() {
		long time = System.currentTimeMillis();
		OtpErlangTuple state = getState();
		long diff = System.currentTimeMillis() - time;
		Game.ping = diff;
		updateLevelList(state);
		updatePlayerList(state);
		updateBulletList(state);
	}
}