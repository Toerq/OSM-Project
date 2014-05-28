package new_version;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;

/**
 * Actual game.
 * 
 * @author www.gametutorial.net
 */

public class Game {

    /**
     * The space rocket with which player will have to land.
     */
	public static int height = 680;
	public static int[][] boxes;
	public static String[] playerNames;
	public static int[][] playerPos;
	public static int[] playerId;
	public static int[][] playerVel;
	public static int[] playerHp;
	private ArrayList<Integer[]> bulletList;
	public static Integer[][] bullets;
	public static int[] playerPow;
	public static Hashtable <Integer, BufferedImage[]> images = new Hashtable <Integer, BufferedImage[]>();
	//public static int R, G, B;
	
	private PlayerRocket playerRocket;
    
    /**
     * Landing area on which rocket will have to land.
     */
    private LandingArea landingArea;
    
    /**
     * Game background image.
     */
    private BufferedImage backgroundImg;
    
    /**
     * Red border of the frame. It is used when player crash the rocket.
     */
    private BufferedImage redBorderImg;
    

    public Game()
    {
    	 //System.out.println("Game state = " + Framework.gameState);
        Framework.gameState = Framework.GameState.GAME_CONTENT_LOADING;
      //  System.out.println("Game state = " + Framework.gameState);
        Thread threadForInitGame = new Thread() {
            @Override
            public void run(){
                // Sets variables and objects for the game.
            	//System.out.println("Initialize...");
                Initialize();
                // Load game files (images, sounds, ...)
               // System.out.println("Loading conent...");
                LoadContent();
              //  System.out.println("Game state = " + Framework.gameState);
                Framework.gameState = Framework.GameState.PLAYING;
               // System.out.println("Game state = " + Framework.gameState);
            }
        };
        threadForInitGame.start();
    }
    
    
   /**
     * Set variables and objects for the game.
     */
    private void Initialize()
    {
    	//R= 50;
    	//G = 50;
    	//B = 50;
    	bulletList = new ArrayList<Integer[]>();
    	System.out.println("Creating Player Rocket...");
    	playerRocket = new PlayerRocket();
    	System.out.println("Creating Landing Area...");
        landingArea  = new LandingArea();
        Main.client.updateState();
    }
    
    /**
     * Load game files - images, sounds, ...
     */
    private void LoadContent()
    {
        try
        {
            URL backgroundImgUrl = this.getClass().getResource("resources/images/background.jpg");
            backgroundImg = ImageIO.read(backgroundImgUrl);
            
            URL redBorderImgUrl = this.getClass().getResource("resources/images/red_border.png");
            redBorderImg = ImageIO.read(redBorderImgUrl);
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
        // Move the rocket
    	//System.out.println("Updating game");
    	playerRocket.Update(mousePosition);
    	for (int i = 0; i < bulletList.size(); i++) {
    		if(bulletList.get(i)[4] == 0) {
    			bulletList.remove(i);
    		}
    	}
    	for (int i = 0; i < bullets.length; i ++) {
    		bulletList.add(bullets[i]);
    	}
        //System.out.println("Player rocket updated");
    }
    
    /**
     * Draw the game to the screen.
     * 
     * @param g2d Graphics2D
     * @param mousePosition current mouse position.
     */
    public void Draw(Graphics2D g2d, Point mousePosition)
    {
      //  g2d.drawImage(backgroundImg, 0, 0, Framework.frameWidth, Framework.frameHeight, null);
        //landingArea.Draw(g2d);
        int x0, x1, y0, y1;
        g2d.setColor(new Color(250,0,28));
        for (int i = 0; i < boxes.length; i++){
        	//g2d.drawRect(20,50, 100, 100);
        	//g2d.drawRect(100,200, 100, 100);
        	x0 = boxes[i][0];
        	y0 = boxes[i][1];
        	x1 = boxes[i][2];
        	y1 = boxes[i][3];
        	g2d.drawRect(100,200, 10, 30);
        	//System.out.println("x0: " + x0 + ", y0: " + y0 + ", x:1 " + x1 + ", y1 :" + y1);
        	int R = (int) (Math.random() * (255));
        	int G = (int) (Math.random() * (255));
        	int B = (int) (Math.random() * (255));
        	//B = B + 4;
        	//G = (G + 4*(B/255)) % 255;
        	//R = (R + 4*(G/255)) % 255;
        	//B = B % 255;

        	g2d.setColor(new Color(R,G,B));
        	g2d.drawRect(x0, Game.height - y1, x1 - x0, y1 - y0);
        }
        System.out.print("Bullet list: ");
        for (int i = 0; i < bulletList.size(); i++) {
        	System.out.print(Arrays.toString(bulletList.get(i)));
       
        	g2d.drawLine(bulletList.get(i)[0], Game.height - bulletList.get(i)[1], bulletList.get(i)[2], Game.height - bulletList.get(i)[3]);
        	bulletList.get(i)[4]--;
        }
     	//System.out.println();
        //System.out.println(images);
        playerRocket.Draw(g2d);
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