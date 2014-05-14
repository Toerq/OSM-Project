
//import InputHandler;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.ArrayList;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

import com.ericsson.otp.erlang.OtpErlangDecodeException;

/**
 * Main class for the game
 */
public class Game extends JFrame
{       
	static String  player123 = "player1";
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
       
        public Game() {
        	
            setTitle("Game Tutorial");
            setSize(windowWidth, windowHeight);
            setResizable(false);
            setDefaultCloseOperation(EXIT_ON_CLOSE);

            insets = getInsets();
            setSize(insets.left + windowWidth + insets.right,
            		insets.top + windowHeight + insets.bottom);

            backBuffer = new BufferedImage(windowWidth, windowHeight, BufferedImage.TYPE_INT_RGB);
            input = new InputHandler(this);
            //setFocusable(true);
            setVisible(true);
        }

        
        public static void main(String[] args)
        {
        		int[] local = {127,0,0,1};
                final Jinterface_bank_client client = new Jinterface_bank_client("127.0.0.1", 3010);
                client.add("newServ", local);
                client.available();
                final Player clientPlayer = new Player(10,10, "player1");
                clientPlayer.addPlayerToServer(local, client);
                Player player2 = new Player(20,20, "player2");
                player2.addPlayerToServer(local, client);
               
                final Game game = new Game();
                game.run(client, clientPlayer);
        		//System.exit(0);
        }
       
        /**
         * This method starts the game and runs it in a loop
         */
        public void run(final Jinterface_bank_client client, final Player clientPlayer)
        {
        	System.out.println("In run method...");
        			while(isRunning)
        			{
        				long time = System.currentTimeMillis();
        				final ArrayList<Player> playerList = client.getAllPos();
        				update(client, clientPlayer);
        				System.out.println("Event is Dispatch thread: " +SwingUtilities.isEventDispatchThread());
        				System.out.println();
        				draw(playerList);

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
       /* void initialize()
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
        }*/
       
        Action createMovementAction (final Jinterface_bank_client client, final Player playerObj, final String direction) {
        	Action movement = new AbstractAction () {
        		@Override
        		public void actionPerformed(ActionEvent e) {
        			client.move(playerObj.getPlayerName(), direction, 5);
        		}
        	};
        	return movement;
        }
        
        /**
         * This method will check for input, move things
         * around and check for win conditions, etc
         * @throws OtpErlangDecodeException 
         */
        void update(Jinterface_bank_client client, Player playerObj)
        {
        	/***** Utskrifter för debuggning **********
            System.out.println("Focusable " + getRootPane().isFocusable());
			System.out.println("Enabled: " + getRootPane().isEnabled());
			System.out.println("Displayable: " + getRootPane().isDisplayable());
			System.out.println("Visible: " + getRootPane().isVisible());
			requestFocus();
			System.out.println("Request focus in window: " + getRootPane().requestFocusInWindow());
			System.out.println("Request focus in window: " + requestFocusInWindow());
			System.out.println("Focused : " + isFocused());
			System.out.println("Focus Owner: " + getFocusOwner());
			*/
				//System.out.println("__________Updating_________");
                if (input.isKeyDown(KeyEvent.VK_RIGHT))
                {
                        //x += 5;
                	client.move(playerObj.getPlayerName(), "right", 5);
                }
                if (input.isKeyDown(KeyEvent.VK_LEFT))
                {
                	client.move(playerObj.getPlayerName(), "left", 5);
                //	x -= 5;
                }
                if (input.isKeyDown(KeyEvent.VK_DOWN))
                {
                        //x += 5;
                	client.move(playerObj.getPlayerName(), "down", 5);
                }
                if (input.isKeyDown(KeyEvent.VK_UP))
                {
                	client.move(playerObj.getPlayerName(), "up", 5);
                //	x -= 5;
                }
               client.updatePos(playerObj.getPlayerName(), playerObj);
               
        }
       
        /**
         * This method will draw everything
         */
         void draw(ArrayList<Player> playerList)
        {       
                Graphics g = getGraphics();
               
                Graphics bbg = backBuffer.getGraphics();
               
                bbg.setColor(Color.WHITE);
                bbg.fillRect(0, 0, windowWidth, windowHeight);
               
                bbg.setColor(Color.BLACK);
                for(Player player : playerList) {
                	bbg.drawOval(player.getX(), player.getY(), 20, 20);
                }
               
                g.drawImage(backBuffer, insets.left, insets.top, this);
        } 
}