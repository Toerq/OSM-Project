package new_version;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

import com.ericsson.otp.erlang.*; 

public class Jinterface_client {
	private Socket socket; 
	private OutputStream out;
	private DataOutputStream dos;
	private DataInputStream fromServer;

	public Jinterface_client(byte[] host, int port) {
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

	public void sendTCP(OtpErlangObject arg) {
		OtpOutputStream availableStream = new OtpOutputStream(arg);
		byte[] data = Utility.arrayPrepend(availableStream);
		try {
			dos.write(data);

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
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

	public void getMyId() {
		OtpErlangAtom getId = new OtpErlangAtom("my_id");
		sendTCP(getId);
		OtpErlangObject id = getAnswer();
		Game.myId = ((OtpErlangPid)id).id();
	}

	public boolean join (OtpErlangPid pid) {
		System.out.println("Joining table: " + pid);
		OtpErlangAtom join = new OtpErlangAtom("join_table");
		OtpErlangObject[] arg = new OtpErlangObject[2];
		arg[0] = join;
		arg[1] = pid;
		OtpErlangTuple tuple = new OtpErlangTuple(arg);
		sendTCP(tuple);
		OtpErlangObject answer = getAnswer();
		System.out.println(answer);
		if (answer.toString().equals("join_succeeded")) {
			return true;
		}
		else {
			return false;
		}
	}


	public void add(String gameName, String gameType, int maxPlayers) {
		OtpErlangAtom add = new OtpErlangAtom("add_table");
		OtpErlangAtom name = new OtpErlangAtom(gameName);
		OtpErlangAtom type = new OtpErlangAtom(gameType);
		OtpErlangInt max = new OtpErlangInt(maxPlayers);
		OtpErlangObject[] arg = {add, name, type, max};
		OtpErlangTuple tuple = new OtpErlangTuple(arg);

		sendTCP(tuple);
	}

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
	}

	public void setName(String name) {
		OtpErlangAtom changeName = new OtpErlangAtom("change_name");
		OtpErlangString newName = new OtpErlangString(name);
		OtpErlangObject[] arg = {changeName, newName};
		OtpErlangTuple tuple = new OtpErlangTuple(arg);
		sendTCP(tuple);
	}

	public void doAction(String action, OtpErlangList argList) {
		OtpErlangAtom doAction = new OtpErlangAtom("do_action");
		OtpErlangAtom actionAtom = new OtpErlangAtom(action);
		OtpErlangObject[] actionArray = {actionAtom, argList};
		OtpErlangTuple actionTuple = new OtpErlangTuple(actionArray);
		OtpErlangObject[] argArray = {doAction, actionTuple};
		OtpErlangTuple argTuple = new OtpErlangTuple(argArray);
		//	System.out.println("Doing action");
		sendTCP(argTuple);
		//System.out.println("Action done");
	}


	// {{Player_move_factor::int, Grid_limit::int, Vel_limit::int, Friction},
	// [{Name_string, {New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}, Hp, Id} | Rest]}


	public OtpErlangTuple getState(){
		OtpErlangAtom getState = new OtpErlangAtom("get_state");
		sendTCP(getState);
		OtpErlangTuple answer = (OtpErlangTuple) getAnswer();
		OtpErlangTuple state = (OtpErlangTuple) answer.elementAt(1);
		return state;
	}

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
		if(bullets.length > 0) {
			//System.out.println("GOT BULLETS!!!");
		}
		Game.bullets = bullets;
	}


	public void updatePlayerList(OtpErlangTuple state) {
		System.out.println();
		System.out.println("State: " + state);
		OtpErlangList playerList = (OtpErlangList) state.elementAt(0);
		OtpErlangObject[] playerArray = playerList.elements();

		int size = playerArray.length;
		System.out.println();
		for(int i = 0; i < size; i++) {
			updatePlayer(playerArray, i);
		}
	}

	private void updatePlayer(OtpErlangObject[] playerArray, int i) {
		try {
			OtpErlangTuple player = (OtpErlangTuple) playerArray[i];
			int id = ((OtpErlangPid) player.elementAt(5)).id();
			String name = ((OtpErlangString) player.elementAt(0)).toString();
			int x = ((OtpErlangLong)((OtpErlangTuple) player.elementAt(1)).elementAt(0)).intValue();
			int y = ((OtpErlangLong)((OtpErlangTuple) player.elementAt(1)).elementAt(1)).intValue();
			int xVelocity = ((OtpErlangLong)((OtpErlangTuple) player.elementAt(2)).elementAt(0)).intValue();
			int hp = ((OtpErlangLong)player.elementAt(3)).intValue();
			int power = ((OtpErlangLong)player.elementAt(4)).intValue();


			if(!(Game.players.containsKey(id))){
				Game.players.put(id, new Player(id, name));
			}
			if(id == Game.myId) {
				Game.myPos[0] = x;
				Game.myPos[1] = y;
				Game.myCenter[0] = x + 6;
				Game.myCenter[1] = Game.height - y - 17;
			}

			Game.players.get(id).setPos(x, y);
			System.out.println(Game.players.get(id).id + " :" + "(" + Game.players.get(id).position[0] + ", " +   Game.players.get(id).position[1] + ")"  );
			Game.players.get(id).setVel(xVelocity);
			Game.players.get(id).setHP(hp);
			Game.players.get(id).setPower(power);
			Game.players.get(id).setDeaths(0);

			/*
			 * Ska fyllas i med riktig statistik längre fram men
			 * vi lägger in knäppa värden så länge bara för att kunna kontrollera att
			 * spelarlistan sorteras korrekt
			 */
			Game.players.get(id).setKills(i);

		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}


	public void updateState() {
		long time = System.currentTimeMillis();
		OtpErlangTuple state = getState();
		long diff = System.currentTimeMillis() - time;
		Game.ping = diff;
		updateLevelList(state);
		updatePlayerList(state);
		updateBulletList(state);
	}


	//Not used - remove?
	/*private void close() throws IOException {
		this.out.close();
		this.dos.close();
		this.fromServer.close();
		this.socket.close();
	}*/

}