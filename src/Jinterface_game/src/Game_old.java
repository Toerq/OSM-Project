

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;

import javax.swing.*;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class Game_old extends JFrame {

	int x, y;
	
	public static void main(String[] args) {
		Jinterface_bank_client client = new Jinterface_bank_client("enode", "erlang");
		int[] arr = {1,2,3,4};
		//client.addPlayer(arr,"player 1");
		new Game_old(client);
	}
	
	public Game_old(Jinterface_bank_client client) {
		setSize(400,400);
		setDefaultCloseOperation(3);
		setVisible(true);
		setResizable(false);
		addKeyListener(new Input());
		x = 100;
		y = 100;
	}
	
	public void paint(Graphics g) {//, Jinterface_bank_client client) {
		Image offScreen = createImage(getWidth(), getHeight());
		draw(offScreen.getGraphics());//, client);
		
		g.drawImage(offScreen, 0, 0, null);
	}
	
	public void draw(Graphics g) {//, Jinterface_bank_client client) {
		g.setColor(Color.BLACK);
		g.fillRect(0, 0, 400, 400);
		int[] arr = new int[2];
		
		g.setColor(Color.BLUE);
		g.fillRect(x, y, 50, 50);
		
		repaint();
	}
	
	private class Input implements KeyListener {
		

		@Override
		public void keyPressed(KeyEvent e) {
			int keyCode = e.getKeyCode();
			
			//if (keyCode == e.VK_UP) {
			//	y -= 5;
			//}
			
			switch(keyCode) {
			case KeyEvent.VK_UP: y -= 7;
			break;
			case KeyEvent.VK_LEFT: x -= 7;
			break;
			case KeyEvent.VK_RIGHT: x += 7;
			break;
			case KeyEvent.VK_DOWN: y += 7;
			break;
			}
			
		}

		@Override
		public void keyReleased(KeyEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void keyTyped(KeyEvent arg0) {
			// TODO Auto-generated method stub
			
		}
	}
}
