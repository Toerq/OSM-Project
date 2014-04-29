
public class Player {
	private int x;
	private int y;
	String playerName;

	public Player(int x, int y, String playerName) {
		this.x = x;
		this.y = y;
		this.playerName = playerName;
	}

	public void addPlayerToServer(int[] destIp, Jinterface_bank_client client) {
        client.addPlayer(destIp, this.playerName);
	}
	
	public void setCoordinates(int x, int y) {
		this.x = x;
		this.y = y;
	}
	
	public String getPlayerName() {
		return playerName;
	}

	public int getX() {
		return x;
	}

	public void setX(int x) {
		this.x = x;
	}

	public int getY() {
		return y;
	}

	public void setY(int y) {
		this.y = y;
	}
	
	
	
}
