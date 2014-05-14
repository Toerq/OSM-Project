import java.awt.*;
import javax.swing.*;

import java.awt.event.*;

public class Menu extends JFrame
{
	private static final int WIDTH = 400;
	private static final int HEIGHT = 300;

	private JLabel dConnect;
	private JTextField ip;
	private JButton serverBrowser, connect;

	public Menu()
	{
		dConnect = new JLabel("Connect direct to server with IP: ", SwingConstants.RIGHT);		
		ip = new JTextField(30);


		//SPecify handlers for each button and add (register) ActionListeners to each button.
		serverBrowser = new JButton("Server Browser");
		sbButtonHandler sbHandler = new sbButtonHandler();
		serverBrowser.addActionListener(sbHandler);

		connect = new JButton("Connect");
		connectButtonHandler cbHandler = new connectButtonHandler();
		connect.addActionListener(cbHandler);

		Container panel = getContentPane();
		GroupLayout layout = new GroupLayout(panel);
		panel.setLayout(layout);

		layout.setAutoCreateGaps(true);
		layout.setAutoCreateContainerGaps(true);


		layout.setHorizontalGroup(
				layout.createSequentialGroup()
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
						.addComponent(dConnect)
						.addComponent(serverBrowser))
						.addComponent(ip)
						.addComponent(connect)
				);

		layout.setVerticalGroup(
				layout.createSequentialGroup()
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
						.addComponent(dConnect)
						.addComponent(ip)
						.addComponent(connect))
						.addComponent(serverBrowser)
				);

		setSize(WIDTH, HEIGHT);
		setVisible(true);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
	}


	private class connectButtonHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			//SwingUtilities.invokeLater(new Runnable() {
			//public void run() {
			String ipString = (ip.getText()); //We use the getText & setText methods to manipulate the data entered into those fields.
			System.out.println(ipString);
			int [] ip = Utility.stringToIp(ipString);

			Jinterface_bank_client client = new Jinterface_bank_client(ipString, 3010);
			//client.add("newServ", ip, ip);
			//client.available(ip);
			Player clientPlayer = new Player(50,50, "player4");
			clientPlayer.addPlayerToServer(ip, client);
			Game game = new Game();
			game.run(client, clientPlayer);
			//System.exit(0);
			//}
			//});
		}
	}

	private class sbButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			System.out.println(getFocusOwner());
			BrowserMenu bm = new BrowserMenu();
		}
	}


	public static void main(String [] args) {
		SwingUtilities.invokeLater(new Runnable () {
		@Override	
			public void run() {
				Menu menu = new Menu();
			}
		});
	}
}