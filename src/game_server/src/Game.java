
//import InputHandler;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.ArrayList;

import javax.swing.JFrame;

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
       
        public static void main(String[] args)
        {
                Game game = new Game();
                Jinterface_bank_client client = new Jinterface_bank_client("enode", "erlang");
                int[] ip = {127, 0, 0, 1};
                client.add("newServ", ip, ip);
                client.available(ip);
                Player clientPlayer = new Player(10,10, "player1");
                clientPlayer.addPlayerToServer(ip, client);
                Player player2 = new Player(20,20, "player2");
                player2.addPlayerToServer(ip, client);
                ArrayList<Player> playerList = new ArrayList<Player>();
                //playerList.add(player1);
                //playerList.add(player2);
                
                game.run(client, clientPlayer);
                System.exit(0);
                
        }
       
        /**
         * This method starts the game and runs it in a loop
         */
        public void run(Jinterface_bank_client client, Player clientPlayer)
        {
                initialize();
               
                while(isRunning)
                {
                        long time = System.currentTimeMillis();
                        ArrayList<Player> playerList = client.getAllPos();
                        
                        update(client, clientPlayer);
                        draw(playerList);
                      //  draw(playerList);
                       
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
        void update(Jinterface_bank_client client, Player playerObj)
        {
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
                	client.move(playerObj.getPlayerName(), "up", 5);
                }
                if (input.isKeyDown(KeyEvent.VK_UP))
                {
                	client.move(playerObj.getPlayerName(), "down", 5);
                //	x -= 5;
                }
               client.updatePos(playerObj.getPlayerName(), playerObj);
        }
       
        /**
         * This method will draw everything
         */
        // void draw(ArrayList<Player> playerList)
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