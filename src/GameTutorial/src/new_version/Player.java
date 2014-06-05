package new_version;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

public class Player {

	/**
	 * Image of the player
	 */
	BufferedImage playerImgRight;
	BufferedImage playerImgLeft;

	/**
	 * Width of player.
	 */
	int playerImgWidth;
	/**
	 * Height of player.
	 */
	int playerImgHeight = 35;

	int id;
	String name;
	int[] position = new int[2];
	int xVelocity;
	int HP;
	int power;
	
	int kills;
	int deaths;
	int wins;

	public Player(int id, String name){
		this.id = id;
		this.name = name;
		System.out.println("Loading Player Content");
		LoadContent();
	}

	private void LoadContent(){
		for (int i = 0; i < Game.playerImages.length; i++) {
			System.out.println("Image nr " + i + "taken: " + Utility.containsArray(Game.takenPlayerImages, Game.playerImages[i]));
			if(!(Utility.containsArray(Game.takenPlayerImages, Game.playerImages[i]))){
				playerImgLeft = Game.playerImages[i][0];
				playerImgRight = Game.playerImages[i][1];
				Game.takenPlayerImages.add(Game.playerImages[i]);
				break;
			}
		}
	}

	public void setPos(int x, int y){
		this.position[0] = x;
		this.position[1] = y;
	}

	public void setVel(int vel){
		this.xVelocity = vel;
	}

	public void setHP(int hp){
		this.HP = hp;
	}

	public void setPower(int pow){
		this.power = pow;
	}

	public void setKills(int kills){
		this.kills = kills;
	}
	
	public void setWins(int wins) {
		this.wins = wins;
	}

	public void setDeaths(int deaths){
		this.deaths = deaths;
	}

	public void draw(Graphics2D g2d){  	
		int base = Game.height - playerImgHeight;
		int x = position[0] - 18;
		int y = base - position[1];
		drawImage(g2d, x, y);
		drawName(g2d, x, y);
		drawHpBar(g2d, x, y);
		drawPowerBar(g2d, x, y);
		/*if(id == Game.myId){
			Utility.drawCircle(g2d, Game.myCenter[0], Game.myCenter[1], Game.shootingRadius);
		}*/
	}

	private void drawImage(Graphics2D g2d, int x, int y) {
		if (HP < 1) {
			float opacity = 0.5f;
			g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity));
		}
		
		if (xVelocity < 0 ) {
			g2d.drawImage(playerImgLeft, x, y, null);
		}
		else {
			g2d.drawImage(playerImgRight, x, y, null);
		}
		g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1));
	}

	private void drawName(Graphics2D g2d, int x, int y) {
		g2d.setColor(Color.white);
		g2d.drawString(name, x -6 , y - 22);
	}

	private void drawPowerBar(Graphics2D g2d, int x, int y) {
		g2d.setColor(Color.white);
		g2d.drawRect(x - 6, y - 9, 50, 4);
		g2d.fillRect(x - 6, y - 9, power/2, 5);
	}

	private void drawHpBar(Graphics2D g2d, int x, int y) {
		g2d.setColor(Color.red);
		g2d.drawRect(x - 6, y - 16, 50, 4);
		g2d.fillRect(x - 6, y - 16, HP/2, 5);
	}

	public String toString(){
		return this.id + ": " + this.position[0] + ", " + this.position[1];
	}

}


