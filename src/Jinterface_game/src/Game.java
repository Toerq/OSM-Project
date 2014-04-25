
//import InputHandler;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import javax.swing.JFrame;

import com.ericsson.otp.erlang.OtpErlangDecodeException;

/**
 * Main class for the game
 */
public class Game extends JFrame
{       
        /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
		boolean isRunning = true;
        int fps = 30;
        int windowWidth = 500;
        int windowHeight = 500;
       
        BufferedImage backBuffer;
        Insets insets;
        InputHandler input;
       
        int x = 0;
       
        public static void main(String[] args)
        {
                Game game = new Game();
                Jinterface_bank_client client = new Jinterface_bank_client("enode", "erlang");
                int[] arr = {127, 0, 0, 1};
                client.addPlayer(arr, "player1");
                game.run(client);
                System.exit(0);
                
        }
       
        /**
         * This method starts the game and runs it in a loop
         */
        public void run(Jinterface_bank_client client)
        {
                initialize();
               
                while(isRunning)
                {
                        long time = System.currentTimeMillis();
                       
                        update(client);
                        draw();
                       
                        //  delay for each frame  -   time it took for one frame
                        time = (1000 / fps) - (System.currentTimeMillis() - time);
                       
                        if (time > 0)
                        {
                                try
                                {
                                        Thread.sleep(time);
                                }
                                catch(Exception e){}
                        }
                }
               
                setVisible(false);
        }
       
        /**
         * This method will set up everything need for the game to run
         */
        void initialize()
        {
                setTitle("Game Tutorial");
                setSize(windowWidth, windowHeight);
                setResizable(false);
                setDefaultCloseOperation(EXIT_ON_CLOSE);
                setVisible(true);
               
                insets = getInsets();
                setSize(insets.left + windowWidth + insets.right,
                                insets.top + windowHeight + insets.bottom);
               
                backBuffer = new BufferedImage(windowWidth, windowHeight, BufferedImage.TYPE_INT_RGB);
                input = new InputHandler(this);
        }
       
        /**
         * This method will check for input, move things
         * around and check for win conditions, etc
         * @throws OtpErlangDecodeException 
         */
        void update(Jinterface_bank_client client)
        {
                if (input.isKeyDown(KeyEvent.VK_RIGHT))
                {
                        x += 5;
                	//client.movePlayer("player1", "left", 5);
                }
                if (input.isKeyDown(KeyEvent.VK_LEFT))
                {
                //	client.movePlayer("player1", "right", 5);
                	//x -= 5;
                }
             //   client.getPos("player1");
        }
       
        /**
         * This method will draw everything
         */
        void draw()
        {              
                Graphics g = getGraphics();
               
                Graphics bbg = backBuffer.getGraphics();
               
                bbg.setColor(Color.WHITE);
                bbg.fillRect(0, 0, windowWidth, windowHeight);
               
                bbg.setColor(Color.BLACK);
                bbg.drawOval(x, 10, 20, 20);
               
                g.drawImage(backBuffer, insets.left, insets.top, this);
        } 
}