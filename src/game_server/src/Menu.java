import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

public class Menu extends JFrame
{
	private static final int WIDTH = 400;
	private static final int HEIGHT = 300;
	
	private JLabel dConnect;
	private JTextField IP;
	private JButton serverBrowser, connect;
	
	public Menu()
	{
		dConnect = new JLabel("Connect direct to server with IP: ", SwingConstants.RIGHT);		
		IP = new JTextField(10);

		
		//SPecify handlers for each button and add (register) ActionListeners to each button.
		serverBrowser = new JButton("Server Browser");
		
		connect = new JButton("Connect");
		connectButtonHandler cbHandler = new connectButtonHandler();
		connect.addActionListener(cbHandler);
		
		setTitle("Sample Title: Area of a Rectangle");
		Container panel = getContentPane();
		GroupLayout layout = new GroupLayout(panel);
		panel.setLayout(layout);
		
		layout.setAutoCreateGaps(true);
		layout.setAutoCreateContainerGaps(true);
		
		
		layout.setHorizontalGroup(
				layout.createSequentialGroup()
				.addComponent(dConnect)
				.addComponent(IP)
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
						.addComponent(connect)
						.addComponent(serverBrowser))
		);
		
		layout.setVerticalGroup(
				   layout.createSequentialGroup()
				      .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
				           .addComponent(dConnect)
				           .addComponent(IP)
				           .addComponent(connect))
				      .addComponent(serverBrowser)
				);
		
		setSize(WIDTH, HEIGHT);
		setVisible(true);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
	}
	
	private int[] stringToIp(String s) {
		System.out.println(s);
		String[] tokens = s.split("\\.");
		System.out.println(tokens.length);
		System.out.println(tokens);
		int[] ip = new int[4];
		for (int i = 0; i < 4; i++) {
			System.out.println(tokens[i]);
			ip[i] = Integer.parseInt(tokens[i]); 
		}
		return ip;
	}
	
	private class connectButtonHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			
			String ipString = (IP.getText()); //We use the getText & setText methods to manipulate the data entered into those fields.
			System.out.println(ipString);
			int [] ip = stringToIp(ipString);
			
			Game game = new Game();
            Jinterface_bank_client client = new Jinterface_bank_client("enode", "erlang");
            //client.add("newServ", ip, ip);
            client.available(ip);
            Player clientPlayer = new Player(20,20, "player4");
            clientPlayer.addPlayerToServer(ip, client);
            game.run(client, clientPlayer);
            System.exit(0);
		}
	}
	/*
	public class ExitButtonHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			System.exit(0);
		}
	}
	*/
	public static void main(String [] args) {
		Menu menu = new Menu();
	}
	}