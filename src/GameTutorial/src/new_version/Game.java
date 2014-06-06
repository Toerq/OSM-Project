package new_version;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import com.ericsson.otp.erlang.*; 

/**
 * Actual game.
 * 
 * @author 
 */

public class Game {

	static int height = 680;
	static int width = 1280;
	
	public static int myId;
	public static int[] myPos = new int[2];
	public static long ping;

	public static BufferedImage[][] playerImages = new BufferedImage[12][2];
	public static ArrayList<BufferedImage[]> takenPlayerImages = new ArrayList<BufferedImage[]>();
	public static Hashtable<Integer, Player> players = new Hashtable<Integer, Player>();

	public static int[][] boxes;
	private ArrayList<Integer[]> bulletList;
	public static Integer[][] bullets;

	/**
	 * Creates a new Game
	 */
	public Game() {		
		Framework.gameState = Framework.GameState.GAME_CONTENT_LOADING;
		Thread threadForInitGame = new Thread() {
			@Override
			public void run(){
				// Load game files
				loadContent();

				// Sets variables and objects for the game.
				initialize();

				Framework.gameState = Framework.GameState.PLAYING;
			}
		};
		threadForInitGame.start();
	}


	/**
	 * Set variables and objects for the game.
	 */
	private void initialize()
	{
		bulletList = new ArrayList<Integer[]>();
		Main.client.updateState();
	}

	/**
	 * Load game files - images, sounds, ...
	 */
	private void loadContent()
	{
		try
		{          
			URL playerImgLeftUrl[] = new URL[12];
			URL playerImgRightUrl[] = new URL[12];
			BufferedImage tmp;

			for (int i = 0; i < 12; i++) {
				playerImgLeftUrl[i] = this.getClass().getResource("/new_images/player_" + (i+1) + "_rev.png");
				playerImgRightUrl[i] = this.getClass().getResource("/new_images/player_" + (i+1) + ".png");
				tmp = ImageIO.read(playerImgLeftUrl[i]);
				Game.playerImages[i][0] = Utility.toCompatibleImage(tmp);
				tmp = ImageIO.read(playerImgRightUrl[i]);
				Game.playerImages[i][1] = Utility.toCompatibleImage(tmp);
			}
		}
		catch (IOException ex) {
			Logger.getLogger(Game.class.getName()).log(Level.SEVERE, null, ex);
		}
	}



	/**
	 * Update game logic.
	 * 
	 * @param gameTime gameTime of the game.
	 * @param mousePosition current mouse position.
	 */
	public void updateGame(long gameTime, Point mousePosition)
	{
		action(mousePosition);
		Main.client.updateState();
		removeOldBullets();
		addNewBullets();
	}

	/**
	 * Add new bullets to bulletList
	 */
	private void addNewBullets() {
		for (int i = 0; i < bullets.length; i ++) {
			bulletList.add(bullets[i]);
		}
	}

	/**
	 * Remove old bullets from bulletList
	 */
	private void removeOldBullets() {
		for (int i = 0; i < bulletList.size(); i++) {
			if(bulletList.get(i)[4] <= 0) {
				bulletList.remove(i);
			}
		}
	}
	
	/**
	 * Perform an action in game depending on keyboard and mouse events
	 * 
	 * @param mousePosition The mouse position
	 */
	public void action(Point mousePosition)
	{
		OtpErlangList argList;
		if(Canvas.mouseButtonState(MouseEvent.BUTTON1)) {
				fire(mousePosition);	
		}
		else if (Canvas.keyboardKeyState(KeyEvent.VK_P)) {
			Main.client.requestRestart();
		}
		else if (Canvas.keyboardKeyState(KeyEvent.VK_SPACE)) {
			argList = new OtpErlangList(new OtpErlangAtom("normal"));
			Main.client.doAction("jump", argList);
		}
		else if (Canvas.keyboardKeyState(KeyEvent.VK_CONTROL)) {
			argList = new OtpErlangList(new OtpErlangAtom("weak"));
			Main.client.doAction("jump", argList);
		}
		else if (Canvas.keyboardKeyState(KeyEvent.VK_W)) {
			argList = new OtpErlangList(new OtpErlangAtom("strong"));
			Main.client.doAction("jump", argList);
		}
		else if (Canvas.keyboardKeyState(KeyEvent.VK_A)) {
			argList = new OtpErlangList(new OtpErlangAtom("left"));
			Main.client.doAction("move", argList);
		}
		else if (Canvas.keyboardKeyState(KeyEvent.VK_D)) {
			argList = new OtpErlangList(new OtpErlangAtom("right"));
			Main.client.doAction("move", argList);
		}
		else if (Canvas.keyboardKeyState(KeyEvent.VK_R)) {
			OtpErlangObject[] argArray = {new OtpErlangAtom("respawn_player"), new OtpErlangAtom("argument")};
			argList = new OtpErlangList(argArray);
			Main.client.doAction("server", argList);
		}
		else {
			argList = new OtpErlangList(new OtpErlangAtom("stop"));
			Main.client.doAction("move", argList);
		}
		try {
			Thread.sleep(5);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Fire the weapon in game in the direction of the mouse position
	 * 
	 * @param mousePosition The current mouse position
	 */
	private void fire(Point mousePosition) {
		OtpErlangList argList;
		OtpErlangInt type = new OtpErlangInt(20);
		OtpErlangInt x = new OtpErlangInt(mousePosition.x);
		OtpErlangInt y = new OtpErlangInt(Game.height - mousePosition.y);
		OtpErlangObject[] posArray = {x,  y};
		OtpErlangTuple posTuple = new OtpErlangTuple(posArray);
		OtpErlangObject[] argArray = {type, posTuple};
		argList = new OtpErlangList(argArray);
		Main.client.doAction("fire", argList);

	}

	/**
	 * Draw the game to the screen.
	 * 
	 * @param g2d Graphics2D
	 * @param mousePosition current mouse position.
	 */
	public void draw(Graphics2D g2d, Point mousePosition)
	{
		//g2d.drawImage(backgroundImg, 0, 0, Framework.frameWidth, Framework.frameHeight, null);
		drawPlatforms(g2d);
		drawBullets(g2d);
		drawStatistics(g2d);
		drawPlayers(g2d);
	}

	/**
	 * Draw the players
	 * 
	 * @param g2d
	 */
	private void drawPlayers(Graphics2D g2d) {
		ArrayList<Player> playerList = new ArrayList<Player>(players.values());
		for (int i = 0; i < playerList.size(); i ++) {
			playerList.get(i).draw(g2d);
		}
	}

	/**
	 * Draw the statistics of the game
	 * 
	 * @param g2d
	 */
	private void drawStatistics(Graphics2D g2d) {
		g2d.setColor(Color.white);
		g2d.drawString("Ping: " , 5, 15);
		g2d.drawString(Long.toString(Game.ping) , 50, 15);
		g2d.drawString("ms" , 85, 15);

		g2d.drawString("My Position: " , 5, 30);
		g2d.drawString(Game.myPos[0] + ", " + Game.myPos[1] , 100, 30);

		g2d.drawString("Wins", Game.width - 200, 15);
		g2d.drawString("Players" , Game.width - 300, 15);
		g2d.drawString("Kills" , Game.width - 145, 15);
		g2d.drawString("Deaths" , Game.width - 105, 15);
		ArrayList<Player> playerList = new ArrayList<Player>(players.values());

		Collections.sort(playerList, new Comparator<Player>() {
			@Override
			public int compare(Player player1, Player player2) {
				return player2.kills - player1.kills;
			}
		});

		Player currentPlayer;
		for(int i = 0; i < playerList.size(); i ++) {
			currentPlayer = playerList.get(i);
			g2d.setColor(Color.gray);
			g2d.drawString(currentPlayer.name, Game.width - 300, 30 + i*15);
			g2d.setColor(Color.yellow);
			g2d.drawString(Integer.toString(currentPlayer.wins), Game.width - 200, 30 + i*15);
			g2d.setColor(Color.green);
			g2d.drawString(Integer.toString(currentPlayer.kills) , Game.width - 145, 30 + i*15);
			g2d.setColor(Color.red);
			g2d.drawString(Integer.toString(currentPlayer.deaths) , Game.width - 105, 30 + i*15);
		}
	}
	
	/**
	 * Draw the bullets
	 * 
	 * @param g2d
	 */
	private void drawBullets(Graphics2D g2d) {
		int R = (int) (Math.random() * (255));
		int G = (int) (Math.random() * (255));
		int B = (int) (Math.random() * (255));

		g2d.setColor(new Color(R,G,B));
		
		for (int i = 0; i < bulletList.size(); i++) {      
			
			g2d.drawLine(bulletList.get(i)[0], Game.height - bulletList.get(i)[1], bulletList.get(i)[2], Game.height - bulletList.get(i)[3]);
			bulletList.get(i)[4]--;
		}
	}

	/**
	 * Draw the game platforms
	 * 
	 * @param g2d
	 */
	private void drawPlatforms(Graphics2D g2d) {
		int x0, x1, y0, y1;
		for (int i = 0; i < boxes.length; i++){
			x0 = boxes[i][0];
			y0 = boxes[i][1];
			x1 = boxes[i][2];
			y1 = boxes[i][3];
			
			g2d.setColor(new Color(0,255,128));
			g2d.drawRect(x0, Game.height - y1, x1 - x0, y1 - y0);
		}
	}
}