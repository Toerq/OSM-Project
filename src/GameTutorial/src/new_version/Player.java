package new_version;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

/**
 * Represents a player in the current game
 * 
 * @author Niklas Hökenström, Jonas Nilson
 *
 */
public class Player {

	/**
	 * Images of the player
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
	/**
	 * The players ID on the server.
	 */
	int id;
	/**
	 * The name of the player.
	 */
	String name;
	/**
	 * The current position of the player.
	 */
	int[] position = new int[2];
	/**
	 * The players velocity in the X-axis.
	 */
	int xVelocity;
	/**
	 * Current health points of the player.
	 */
	int HP;
	/**
	 * The players current power (decreases when shots are fired, increased on movement). 
	 */
	int power;
	
	/**
	 * Counting the players kills.
	 */
	int kills;
	/**
	 * Counting the players deaths.
	 */
	int deaths;
	/**
	 * Counting rounds won by the player.
	 */
	int wins;

	/**
	 * Constructs a Player object with a unique id and a name.
	 * 
	 * @param id unique id for the player object (obtained from the server).
	 * @param name Player object name (can be changed by the user).
	 */
	public Player(int id, String name){
		this.id = id;
		this.name = name;
		LoadContent();
	}

	/**
	 * Assigns the Player object a unique image from the ones available.
	 */
	private void LoadContent(){
		for (int i = 0; i < Game.playerImages.length; i++) {
			if(!(Utility.containsArray(Game.takenPlayerImages, Game.playerImages[i]))){
				playerImgLeft = Game.playerImages[i][0];
				playerImgRight = Game.playerImages[i][1];
				Game.takenPlayerImages.add(Game.playerImages[i]);
				break;
			}
		}
	}

	/**
	 * Sets the players position in the X- and Y-axis.
	 * @param x integer value of X-axis position.
	 * @param y integer value of Y-axis position.
	 */
	public void setPos(int x, int y){
		this.position[0] = x;
		this.position[1] = y;
	}

	/**
	 * Sets the players X-axis velocity.
	 * @param vel integer value of X-axis velocity.
	 */
	public void setVel(int vel){
		this.xVelocity = vel;
	}

	/**
	 * Sets the players health points (HP).
	 * @param hp integer value of health points (HP).
	 */
	public void setHP(int hp){
		this.HP = hp;
	}

	/**
	 * Sets the power (used for firing the weapon)
	 * @param pow integer value of power.
	 */
	public void setPower(int pow){
		this.power = pow;
	}

	/**
	 * Sets the amount of kills 
	 * @param kills integer value of kills
	 */
	public void setKills(int kills){
		this.kills = kills;
	}
	
	/**
	 * Sets the amount of wins
	 * @param wins integer value of wins
	 */
	public void setWins(int wins) {
		this.wins = wins;
	}

	/**
	 * Sets the amount of deaths
	 * @param deaths integer value of deaths
	 */
	public void setDeaths(int deaths){
		this.deaths = deaths;
	}

	/**
	 * Draws the player object
	 * @param g2d Graphics2D
	 */
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

	/**
	 * Draws the player image. If the player is dead, use 50% opacity.
	 * @param g2d Graphics2D
	 * @param x X-axis coordinate
	 * @param y Y-axis coordinate
	 */
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

	/**
	 * Draws the player name above the correct player 
	 * @param g2d Graphics2D
	 * @param x X-axis coordinate of the player
	 * @param y Y-axis coordinate of the player
	 */
	private void drawName(Graphics2D g2d, int x, int y) {
		g2d.setColor(Color.white);
		g2d.drawString(name, x -6 , y - 22);
	}

	/**
	 * Draws the power bar above the correct player
	 * @param g2d Graphics2D
	 * @param x X-axis coordinate of the player
	 * @param y Y-axis coordinate of the player
	 */
	private void drawPowerBar(Graphics2D g2d, int x, int y) {
		g2d.setColor(Color.white);
		g2d.drawRect(x - 6, y - 9, 50, 4);
		g2d.fillRect(x - 6, y - 9, power/2, 5);
	}

	/**
	 * Draws the health points bar above the correct player
	 * @param g2d Graphics2D
	 * @param x X-axis coordinate of the player
	 * @param y Y-axis coordinate of the player
	 */
	private void drawHpBar(Graphics2D g2d, int x, int y) {
		g2d.setColor(Color.red);
		g2d.drawRect(x - 6, y - 16, 50, 4);
		g2d.fillRect(x - 6, y - 16, HP/2, 5);
	}

	/**
	 * Returns the id and position of the player as a String
	 */
	public String toString(){
		return this.id + ": " + this.position[0] + ", " + this.position[1];
	}

}


