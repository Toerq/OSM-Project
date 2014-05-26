package new_version;

import java.awt.Color;
import java.awt.Graphics2D;
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
    private BufferedImage rocketImg;
  
    
    /**
     * Width of rocket.
     */
    public static int rocketImgWidth;
    /**
     * Height of rocket.
     */
    public static int rocketImgHeight;
    
    
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
       //x = pos[0];
       //y = pos[1];
    }

    private void LoadContent()
    {
    	try
    	{
    		URL rocketImgUrl = this.getClass().getResource("resources/images/rocket.png");
    		rocketImg = ImageIO.read(rocketImgUrl);
    		rocketImgWidth = rocketImg.getWidth();
    		rocketImgHeight = rocketImg.getHeight();

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
    	/*if(Canvas.mouseButtonState(MouseEvent.BUTTON1))
    	{
    		fire(mousePosition);
    		//Main.client.doAction("move_down", argList);
    	}
    	*/
    	/*else if(Canvas.keyboardKeyState(KeyEvent.VK_W)) {
    		argList = new OtpErlangList();
    		Main.client.doAction("move_down", argList);
    	}*/
    	
    	if (Canvas.keyboardKeyState(KeyEvent.VK_A)) {
    		argList = new OtpErlangList(new OtpErlangAtom("left"));
    		Main.client.doAction("move", argList);
    	}

    	else if (Canvas.keyboardKeyState(KeyEvent.VK_D)) {
    		argList = new OtpErlangList(new OtpErlangAtom("right"));
    		Main.client.doAction("move", argList);
    	}
    	
    	else if (Canvas.keyboardKeyState(KeyEvent.VK_SPACE)) {
    		argList = new OtpErlangList(new OtpErlangAtom("normal"));
    		Main.client.doAction("jump", argList);
    	}
    	/*else if(Canvas.keyboardKeyState(KeyEvent.VK_S)) {
    		argList = new OtpErlangList();
    		Main.client.doAction("move_up", argList);
    	}*/

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
    	//Main.client.getState();
    	Main.client.updateState();
    	//System.out.println("Updated with new state");
    	//x = xy[0];
    	//y = xy[1];

    }


	private void fire(Point mousePosition) {
		OtpErlangList argList;
		OtpErlangInt x = new OtpErlangInt(mousePosition.x);
		OtpErlangInt y = new OtpErlangInt(mousePosition.y);
		OtpErlangObject[] posArray = {x, y};
		OtpErlangTuple posTuple = new OtpErlangTuple(posArray);
		argList = new OtpErlangList(posTuple);
		Main.client.doAction("fire", argList);
	}
    
    public void Draw(Graphics2D g2d)
    {
    	g2d.setColor(Color.white);
    	g2d.drawString("Rocket coordinates: " + x + " : " + y, 5, 15);
    	int[][] players = Game.playerPos;
    	int base = Game.height - rocketImgHeight;
    	for(int i = 0; i < players.length; i++ ) {   	
    		System.out.println("Player " + i + " : (" + players[i][0] + ", " + players[i][1] + ")");
    	g2d.drawImage(rocketImg, players[i][0], base - players[i][1], null);
    	g2d.drawString(Game.playerNames[i],players[i][0] , base - 10 -players[i][1]);
    	}
    }
    
}