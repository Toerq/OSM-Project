package new_version;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;

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
    public int rocketImgWidth;
    /**
     * Height of rocket.
     */
    public int rocketImgHeight;
    
    
    public PlayerRocket()
    {
    	System.out.println("Initializing Player Rocket");
        Initialize();
        System.out.println("Loading Player Rocket Content");
        LoadContent();
        
    }
    
    
    private void Initialize()
    {
       int[] pos = Main.client.getState();
       x = pos[0];
       y = pos[1];
    }
    
    private void LoadContent()
    {
        try
        {
            URL rocketImgUrl = this.getClass().getResource("/moon_lander/resources/images/rocket.png");
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
    public void Update()
    {
    		System.out.println("Updating player rocket");
    	if(Canvas.keyboardKeyState(KeyEvent.VK_W))
    		Main.client.doAction("move_down");
    
    	else if (Canvas.keyboardKeyState(KeyEvent.VK_A))
    		Main.client.doAction("move_left");
    
    	else if (Canvas.keyboardKeyState(KeyEvent.VK_D))
    		Main.client.doAction("move_right");

    	else if(Canvas.keyboardKeyState(KeyEvent.VK_S))
    		Main.client.doAction("move_up");
    	
    	else
    		Main.client.doAction("stop");
    		
        try {
			Thread.sleep(5);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        // Updates position
    	int[] xy = Main.client.getState();
    	System.out.println("Updated with new state");
    	x = xy[0];
    	y = xy[1];
        
    }
    
    public void Draw(Graphics2D g2d)
    {
    	g2d.setColor(Color.white);
    	g2d.drawString("Rocket coordinates: " + x + " : " + y, 5, 15);
    	g2d.drawImage(rocketImg, x, y, null);
    }
    
}
