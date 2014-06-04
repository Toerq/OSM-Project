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
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Actual game.
 * 
 * @author www.gametutorial.net
 */

public class Game {

	public static int height = 680;
	public static int width = 1280;

	public static int myId;
	public static int[] myPos = new int[2];
	public static int shootingRadius = 250;
	public static int[] myCenter = new int[2];
	public static long ping;

	public static BufferedImage[][] playerImages = new BufferedImage[12][2];
	public static ArrayList<BufferedImage[]> takenPlayerImages = new ArrayList<BufferedImage[]>();
	public static Hashtable<Integer, Player> players = new Hashtable<Integer, Player>();

	public static int[][] boxes;
	private ArrayList<Integer[]> bulletList;
	public static Integer[][] bullets;


	public Game()
	{
		Framework.gameState = Framework.GameState.GAME_CONTENT_LOADING;
		Thread threadForInitGame = new Thread() {
			@Override
			public void run(){
				// Load game files (images, sounds, ...)
				// System.out.println("Loading content...");
				LoadContent();

				// Sets variables and objects for the game.
				//System.out.println("Initialize...");
				Initialize();

				Framework.gameState = Framework.GameState.PLAYING;
			}
		};
		threadForInitGame.start();
	}


	/**
	 * Set variables and objects for the game.
	 */
	private void Initialize()
	{
		bulletList = new ArrayList<Integer[]>();
		System.out.println("Creating Player...");
		Main.client.updateState();
	}

	/**
	 * Load game files - images, sounds, ...
	 */
	private void LoadContent()
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
			System.out.println("images:" + playerImages);
		}
		catch (IOException ex) {
			Logger.getLogger(Game.class.getName()).log(Level.SEVERE, null, ex);
		}
	}


	/**
	 * Restart game - reset some variables.
	 */
	public void RestartGame()
	{
		//  playerRocket.ResetPlayer();
	}


	/**
	 * Update game logic.
	 * 
	 * @param gameTime gameTime of the game.
	 * @param mousePosition current mouse position.
	 */
	public void UpdateGame(long gameTime, Point mousePosition)
	{
		move(mousePosition);
		Main.client.updateState();
		removeOldBullets();
		addNewBullets();
	}


	private void addNewBullets() {
		for (int i = 0; i < bullets.length; i ++) {
			bulletList.add(bullets[i]);
		}
	}


	private void removeOldBullets() {
		for (int i = 0; i < bulletList.size(); i++) {
			if(bulletList.get(i)[4] == 0) {
				bulletList.remove(i);
			}
		}
	}

	public void move(Point mousePosition)
	{
		OtpErlangList argList;
		if(Canvas.mouseButtonState(MouseEvent.BUTTON1))
		{
			if(mousePosition.distance((double) Game.myCenter[0], (double) Game.myCenter[1]) < (double) Game.shootingRadius){
				fire(mousePosition);	
			}	
		}

		else if (Canvas.keyboardKeyState(KeyEvent.VK_SPACE)) {
			argList = new OtpErlangList(new OtpErlangAtom("normal"));
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

	private void fire(Point mousePosition) {
		OtpErlangList argList;
		OtpErlangInt type = new OtpErlangInt(25);
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
	public void Draw(Graphics2D g2d, Point mousePosition)
	{
		drawBoxes(g2d);
		drawBullets(g2d);

		drawStatistics(g2d);

		drawPlayers(g2d);

	}


	private void drawPlayers(Graphics2D g2d) {
		ArrayList<Player> playerList = new ArrayList<Player>(players.values());
		for (int i = 0; i < playerList.size(); i ++) {
			playerList.get(i).draw(g2d);
		}
	}


	private void drawStatistics(Graphics2D g2d) {
		g2d.setColor(Color.white);
		g2d.drawString("Ping: " , 5, 15);
		g2d.drawString(Long.toString(Game.ping) , 50, 15);
		g2d.drawString("ms" , 85, 15);

		g2d.drawString("My Position: " , 5, 30);
		g2d.drawString(Game.myPos[0] + ", " + Game.myPos[1] , 100, 30);

		g2d.drawString("Players" , Game.width - 250, 15);
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
			g2d.drawString(currentPlayer.name, Game.width - 250, 30 + i*15);
			g2d.setColor(Color.green);
			g2d.drawString(Integer.toString(currentPlayer.kills) , Game.width - 145, 30 + i*15);
			g2d.setColor(Color.red);
			g2d.drawString(Integer.toString(currentPlayer.deaths) , Game.width - 105, 30 + i*15);
		}
	}


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


	private void drawBoxes(Graphics2D g2d) {
		int x0, x1, y0, y1;
		//g2d.setColor(new Color(250,0,28));
		for (int i = 0; i < boxes.length; i++){
			x0 = boxes[i][0];
			y0 = boxes[i][1];
			x1 = boxes[i][2];
			y1 = boxes[i][3];
			
			g2d.setColor(new Color(0,255,128));
			g2d.drawRect(x0, Game.height - y1, x1 - x0, y1 - y0);
		}
	}


	/**
	 * Draw the game over screen.
	 * 
	 * @param g2d Graphics2D
	 * @param mousePosition Current mouse position.
	 * @param gameTime Game time in nanoseconds.
	 */
	/* public void DrawGameOver(Graphics2D g2d, Point mousePosition, long gameTime)
    {
        Draw(g2d, mousePosition);

        g2d.drawString("Press space or enter to restart.", Framework.frameWidth / 2 - 100, Framework.frameHeight / 3 + 70);

        if(playerRocket.landed)
        {
            g2d.drawString("You have successfully landed!", Framework.frameWidth / 2 - 100, Framework.frameHeight / 3);
            g2d.drawString("You have landed in " + gameTime / Framework.secInNanosec + " seconds.", Framework.frameWidth / 2 - 100, Framework.frameHeight / 3 + 20);
        }
        else
        {
            g2d.setColor(Color.red);
            g2d.drawString("You have crashed the rocket!", Framework.frameWidth / 2 - 95, Framework.frameHeight / 3);
            g2d.drawImage(redBorderImg, 0, 0, Framework.frameWidth, Framework.frameHeight, null);
        }
    }*/
}