package new_version;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * The space rocket with which player will have to land.
 * 
 * @author www.gametutorial.net
 */

public class PlayerRocket {
    

    /**
     * X coordinate of the rocket.
     */
    public int x;
    /**
     * Y coordinate of the rocket.
     */
    public int y;
    
            
    /**
     * Image of the rocket in air.
     */
   // private int shootingRadius = 250;
    //private int[] myCenter = new int[2];
    //private BufferedImage player;
    public static BufferedImage[] playerImgRight;
    public static BufferedImage[] playerImgLeft;
  
    
    /**
     * Width of rocket.
     */
    public static int playerImgWidth;
    /**
     * Height of rocket.
     */
    public static int playerImgHeight;
    
    
    public PlayerRocket()
    {
    	System.out.println("Initializing Player Rocket");
        Initialize();
        System.out.println("Loading Player Rocket Content");
        LoadContent();
        
    }
    
    
    private void Initialize()
    {
    	Main.client.getState();
    	//myCenter[0] = 
       //x = pos[0];
       //y = pos[1];
    }

    private BufferedImage toCompatibleImage(BufferedImage image)
    {
    	//Get current GraphicsConfiguration
        GraphicsConfiguration gfx_config
                = GraphicsEnvironment
                .getLocalGraphicsEnvironment()
                .getDefaultScreenDevice()
                .getDefaultConfiguration();

        /*
         * if image is already compatible and optimized for current system
         * settings, simply return it
         */
        if (image.getColorModel().equals(gfx_config.getColorModel()))
        {
            image.setAccelerationPriority(1.0f);
            return image;
        }

        // image is not optimized, so create a new image that is
        BufferedImage new_image = gfx_config.createCompatibleImage(image.getWidth(), image.getHeight(), image.getTransparency());

        // get the graphics context of the new image to draw the old image on
        Graphics2D g2d = (Graphics2D) new_image.getGraphics();

        // actually draw the image and dispose of context no longer needed
        g2d.drawImage(image, 0, 0, null);
        g2d.dispose();

        new_image.setAccelerationPriority(1.0f);

        // return the new optimized image
        return new_image;
    }

    private void LoadContent()
    {
    	/*GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice device = env.getDefaultScreenDevice();
        GraphicsConfiguration config = device.getDefaultConfiguration();
        //BufferedImage buffy = config.createCompatibleImage(width, height, Transparency.TRANSLUCENT);
    	 */
    	playerImgLeft = new BufferedImage[12];
    	playerImgRight = new BufferedImage[12];
    	BufferedImage tmp;
    	//BufferedImage[] tmp = new BufferedImage[12];
    	try
    	{
    		//URL playerUrl= this.getClass().getResource("resources/images/player.png");
    		//URL playerUrl= this.getClass().getResource("resources/images/player_" + 1 + ".png");
    		//BufferedImage tmp =ImageIO.read(playerUrl);
    		//player = toCompatibleImage(tmp);
    		URL[] playerImgLeftUrl = new URL[12];
    		URL[] playerImgRightUrl = new URL[12];
    		for (int i = 0; i < 12; i++) {
    			playerImgLeftUrl[i] = this.getClass().getResource("resources/new_images/player_" + (i+1) + "_rev.png");
    			playerImgRightUrl[i] = this.getClass().getResource("resources/new_images/player_" + (i+1) + ".png");

    			//playerImgLeftUrl[i] = this.getClass().getResource("resources/new_images/player_1.png");
    			//playerImgRightUrl[i] = this.getClass().getResource("resources/new_images/player_1.png");

    			tmp = ImageIO.read(playerImgLeftUrl[i]);
    			playerImgLeft[i] = toCompatibleImage(tmp);
    			tmp = ImageIO.read(playerImgRightUrl[i]);
    			playerImgRight[i] = toCompatibleImage(tmp);
    		}
    		playerImgWidth = playerImgLeft[0].getWidth();
    		playerImgHeight = playerImgLeft[0].getHeight();

    	}
    	catch (IOException ex) {
    		Logger.getLogger(PlayerRocket.class.getName()).log(Level.SEVERE, null, ex);
    	}
    }

    /**
     * Here we set up the rocket when we starting a new game.
     */
    /*    public void ResetPlayer()
    {

    }
     */ 

    /**
     * Here we move the rocket.
     */
    public void Update(Point mousePosition)
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
    	// Updates position
    	Main.client.updateState();
    	Game.myCenter[0] = Game.myPos[0] + 6;
    	Game.myCenter[1] = Game.height - Game.myPos[1] - 17;

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

 // Convenience method to draw from center with radius
    public void drawCircle(Graphics2D cg, int xCenter, int yCenter, int r) {
    cg.drawOval(xCenter-r, yCenter-r, 2*r, 2*r);
    }//end drawCircle
    
    
    public void Draw(Graphics2D g2d)
    {
    	g2d.setColor(Color.white);
    	g2d.drawString("Ping: " , 5, 15);
    	g2d.drawString(Long.toString(Game.ping) , 50, 15);
    	g2d.drawString("ms" , 65, 15);
    	
    	g2d.drawString("My Position: " , 5, 30);
    	g2d.drawString(Game.myPos[0] + ", " + Game.myPos[1] , 100, 30);
    	
    	g2d.drawString("Players" , Game.width - 200, 15);
    	g2d.drawString("Kills" , Game.width - 125, 15);
    	g2d.drawString("Deaths" , Game.width - 85, 15);
    	for(int i = 0; i < Game.playerNames.length; i ++) {
    		g2d.drawString(Game.playerNames[i] , Game.width - 200, 30 + i*15);
    		g2d.drawString("0" , Game.width - 125, 30 + i*15);
    		g2d.drawString("0" , Game.width - 85, 30 + i*15);
    	}
    	
    	int[][] players = Game.playerPos;
    	int base = Game.height - playerImgHeight;
    	for(int i = 0; i < players.length; i++ ) {   	
    		//System.out.println("Player " + i + " : (" + players[i][0] + ", " + players[i][1] + ")");
    		int x = players[i][0] - 18;
    		int y = base - players[i][1];
    		if (Game.playerVel[i][0] < 0 ) {
    			g2d.drawImage(Game.images.get(Game.playerId[i])[0], x, y, null);
    		}
    		else {
    			g2d.drawImage(Game.images.get(Game.playerId[i])[1], players[i][0] - 18, base - players[i][1], null);
    		}
    		//g2d.setColor(Color.white);
  
    		g2d.drawString(Game.playerNames[i], x -6 , y - 22);
    		//g2d.drawRect(100, 100, 100, 100);
    		g2d.setColor(Color.red);
    		g2d.drawRect(x - 6, y - 16, 50, 4);
    		g2d.fillRect(x - 6, y - 16, (int) (Game.playerHp[i])/2, 5);
    		g2d.setColor(Color.white);
    		g2d.drawRect(x - 6, y - 9, 50, 4);
    		g2d.fillRect(x - 6, y - 9, (int) (Game.playerPow[i])/2, 5);
    		//g2d.drawLine(10, 10, 100, 100);
    	}
    	drawCircle(g2d, Game.myCenter[0], Game.myCenter[1], Game.shootingRadius);
    }

}