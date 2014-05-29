package new_version;

import java.awt.image.BufferedImage;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Hashtable;

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
	
	public void initMailbox(Jinterface_client client) {
		
	}
	public void sendTCP(OtpErlangObject arg) {
		OtpOutputStream availableStream = new OtpOutputStream(arg);
		byte[] data = arrayPrepend(availableStream);
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
		}	//public static int R, G, B;
		return answer;
	}
		
	public void ping() {
		OtpErlangAtom ping = new OtpErlangAtom("ping");
		sendTCP(ping);
		OtpErlangObject pong = getAnswer();
	}

	public void getMyId() {
		OtpErlangAtom getId = new OtpErlangAtom("my_id");
		System.out.println(getId);
		sendTCP(getId);
		OtpErlangObject id = getAnswer();
		System.out.println(id);
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
	
	public void add() {
		OtpErlangAtom add = new OtpErlangAtom("add_table");
		sendTCP(add);
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
		
		//OtpErlangList optionList = new OtpErlangList(argList);
		/*int size = argList.length;
		OtpErlangObject[] options = new OtpErlangObject[size];

		for (int i = 0; i < size; i++) {
			options[i] = new OtpErlangAtom(argList[i]);
		}
		
		OtpErlangList optionList = new OtpErlangList(options);
		*/
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
		//System.out.println()
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
	
	
	public boolean containsArray(Hashtable<Integer, BufferedImage[]> table, BufferedImage[] imgs) {
		int i = 0;
		Enumeration<BufferedImage[]> elements = table.elements();
		while(elements.hasMoreElements()) {
			BufferedImage[] cmp =  elements.nextElement();
			System.out.println(cmp);
			if (Arrays.equals(imgs, cmp)) {
				return true;
			}
			System.out.println("containsArray index: " + i);
			i++;
		}
		return false;
	}
	
	public void updatePlayerList(OtpErlangTuple state) {
		System.out.println("State: " + state);
		//System.out.println(state.elementAt(0));
		OtpErlangList playerList = (OtpErlangList) state.elementAt(0);
		//System.out.println("Player List : " + playerList);
		OtpErlangObject[] playerArray = playerList.elements();
		
		int size = playerArray.length;
		int[][] positions= new int[size][2];
		int[][] velocity = new int[size][2];
		String[] names = new String[size];
		int[] hp = new int[size];
		int[] id = new int[size];
		int[] power = new int[size];
		
		OtpErlangPid idTmp;
		OtpErlangTuple player; 
		OtpErlangTuple position;
		OtpErlangTuple vel;
		//OtpErlangInt power;
		
		for(int i = 0; i < size; i++) {
			player = (OtpErlangTuple) playerArray[i];
			names[i] = ((OtpErlangString) player.elementAt(0)).toString();
			position = (OtpErlangTuple) player.elementAt(1);
			vel = (OtpErlangTuple) player.elementAt(2);
			idTmp = (OtpErlangPid) player.elementAt(5);
			
			try {
				hp[i] = ((OtpErlangLong)player.elementAt(3)).intValue();
				power[i] = ((OtpErlangLong)player.elementAt(4)).intValue();
				positions[i][0] = ((OtpErlangLong)position.elementAt(0)).intValue();
				positions[i][1] = ((OtpErlangLong)position.elementAt(1)).intValue();
				velocity[i][0] = ((OtpErlangLong)vel.elementAt(0)).intValue();
				velocity[i][1] = ((OtpErlangLong)vel.elementAt(1)).intValue();
				id[i] = idTmp.id();
				if (id[i] == Game.myId) {
					Game.myPos = positions[i];
				}

				if (!(Game.images.containsKey(id[i]))) {
					int j = 0;
					BufferedImage[] imgs = new BufferedImage[2];
					do {
						imgs[0] = PlayerRocket.playerImgLeft[j];
						imgs[1] = PlayerRocket.playerImgRight[j];
						j++;
						System.out.println(j);
					} while(containsArray(Game.images, imgs));

					
					/*while(Game.images.contains(PlayerRocket.playerImgLeft[j])) {
						j++;
					}*/
					//BufferedImage[] imgs = {PlayerRocket.playerImgLeft[j], PlayerRocket.playerImgRight[j]};
					Game.images.put(id[i], imgs);
				}
			} catch (OtpErlangRangeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		Game.playerHp = hp;
		Game.playerId = id;
		Game.playerNames = names;
		Game.playerPos = positions;
		Game.playerPow = power;
		Game.playerVel = velocity;
		//Game.positions.put(id, );
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