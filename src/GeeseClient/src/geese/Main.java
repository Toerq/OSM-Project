package geese;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.table.*;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

/**
 * The main window of the application
 * 
 * @author Niklas Hokenstrom, Jonas Nilson
 */
public class Main implements ActionListener {
	static byte [] ip = null;
	public static Jinterface client = null;
	JFrame f;
	JScrollPane table;
	String playerName;
	String[] columnNames = {"ID", "Name", "Game Type", "Connected Players/Max Players", ""};
	Object[][] data;
	Action join;
	JButton connectButton, addTableButton, refreshButton, pingButton, changeNameButton;
	Framework framework = null;

	/**
	 * Creates the main application window
	 * 
	 * @param name The players name
	 */
	Main(String name) {
		playerName = name;
		f = new JFrame("GEESE - " + name);
		f.setSize(1280, 720);
		f.setLocationRelativeTo(null);
		f.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);

		table = new JScrollPane();
		JPanel container = createContentPanel();
		f.setContentPane(container);  

		f.addComponentListener(new ComponentAdapter() {
			@Override
			public void componentHidden(ComponentEvent e) {
				Framework.gameState = Framework.GameState.GAMEOVER;

				try {
					Thread.sleep(500);
				} catch (InterruptedException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				Main.client.removePlayer();               
				f.dispose();
				System.exit(0);
			}
		});

		f.setVisible(true);
	} 

	/**
	 * Creates the standard content panel of the application
	 * 
	 * @return The content panel
	 */
	private JPanel createContentPanel() {
		createButtons();
		JPanel panel = new JPanel();
		panel.add(addTableButton);
		JPanel container = (JPanel) f.getContentPane();

		if(Main.ip == null) {
			setSimpleLayout(container);
		} else {
			setCompleteLayout(container);
		}

		return container;
	}

	/** Sets the layout of container to a simple layout 
	 * 
	 * @param container
	 */
	private void setSimpleLayout(JPanel container) {
		GroupLayout layout = new GroupLayout(container);

		layout.setAutoCreateGaps(true);
		layout.setAutoCreateContainerGaps(true);

		layout.setHorizontalGroup(
				layout.createSequentialGroup()

				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
						.addComponent(table)
						.addGroup(layout.createSequentialGroup()
								.addComponent(connectButton)
								.addComponent(changeNameButton)))
				);
		layout.setVerticalGroup(
				layout.createSequentialGroup()
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
						.addComponent(table))
						.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
								.addComponent(connectButton)
								.addComponent(changeNameButton))
				);
		container.setLayout(layout);
	}

	/** Sets the layout of container to a complete layout 
	 * 
	 * @param container
	 */
	private void setCompleteLayout(JPanel container) {
		GroupLayout layout = new GroupLayout(container);

		layout.setAutoCreateGaps(true);
		layout.setAutoCreateContainerGaps(true);

		layout.setHorizontalGroup(
				layout.createSequentialGroup()

				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
						.addComponent(table)
						.addGroup(layout.createSequentialGroup()
								.addComponent(connectButton)
								.addComponent(addTableButton)
								.addComponent(refreshButton)
								.addComponent(pingButton)
								.addComponent(changeNameButton)))

				);
		layout.setVerticalGroup(
				layout.createSequentialGroup()
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
						.addComponent(table))
						.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
								.addComponent(connectButton)
								.addComponent(addTableButton)
								.addComponent(refreshButton)
								.addComponent(pingButton)
								.addComponent(changeNameButton))
				);
		container.setLayout(layout);
	}

	/**
	 * Creates the buttons and button handlers of the application
	 */
	private void createButtons() {
		connectButton = new JButton("Connect to IP");
		ConnectButtonHandler connectHandler = new ConnectButtonHandler();
		connectButton.addActionListener(connectHandler);

		addTableButton = new JButton("Add new table");
		AddButtonHandler addHandler = new AddButtonHandler();
		addTableButton.addActionListener(addHandler);

		refreshButton = new JButton("Refresh");
		RefreshButtonHandler rfHandler = new RefreshButtonHandler();
		refreshButton.addActionListener(rfHandler);

		pingButton = new JButton("Ping ?? ms");
		PingButtonHandler pingHandler = new PingButtonHandler();
		pingButton.addActionListener(pingHandler);

		changeNameButton = new JButton("Change Player Name");
		ChangeNameButtonHandler changeNameHandler = new ChangeNameButtonHandler();
		changeNameButton.addActionListener(changeNameHandler);
	}

	private class AddButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {

			String[] types = {"Pixel Wars", "Type 1", "Type 2", "Type 3", "Type 4", "Type 5"};
			String[] max = {"2", "3", "4", "5", "6", "7" , "8", "9",  "10", "11", "12",};
			JComboBox<String> typeCombo = new JComboBox<String>(types);
			JComboBox<String> maxCombo = new JComboBox<String>(max);

			JTextField field1 = new JTextField("My Table");
			JPanel panel = new JPanel(new GridLayout(0, 1));
			panel.add(new JLabel("Name: "));
			panel.add(field1);
			panel.add(new JLabel("Type:"));
			panel.add(typeCombo);
			panel.add(new JLabel("Max Players:"));
			panel.add(maxCombo);
			int result = JOptionPane.showConfirmDialog(null, panel, "Test",
					JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE);

			String name = field1.getText();
			String type = typeCombo.getSelectedItem().toString();
			int maxPlayers = Integer.parseInt(maxCombo.getSelectedItem().toString());

			client.add(name, type, maxPlayers);
			try {
				Thread.sleep(100);
			} catch (InterruptedException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			table = createGameList();
			f.getContentPane().removeAll();
			f.setContentPane(createContentPanel());
			f.validate();
		}
	}
	private class ConnectButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			JTextField field1 = new JTextField("127.0.0.1");
			JTextField field2 = new JTextField("3010");
			JPanel panel = new JPanel(new GridLayout(0, 1));
			panel.add(new JLabel("IP: "));
			panel.add(field1);
			panel.add(new JLabel("Port: "));
			panel.add(field2);

			int result = JOptionPane.showConfirmDialog(null, panel, "Test",
					JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE);

			String ipString = field1.getText();
			String portString = field2.getText();
			Main.ip = Utility.stringToIp(ipString);
			Main.client = new Jinterface(Main.ip, Integer.parseInt(portString));

			if(ip != null) {
				try {
					Thread.sleep(100);	
					Main.client.setName(playerName);
					Thread.sleep(100);
				} catch (InterruptedException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				updateMyId();
				table = createGameList();

				f.setTitle("GEESE - " + playerName + " - Connected to: " + Utility.ipArrayToString(ip));
			} else {
				f.setTitle("GEESE - " + playerName);
				table = new JScrollPane();
			}

			f.getContentPane().removeAll();
			f.setContentPane(createContentPanel());
			f.validate();
		}
	}

	private class ChangeNameButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			JTextField field1 = new JTextField("");
			JPanel panel = new JPanel(new GridLayout(0, 1));
			panel.add(new JLabel("Name: "));
			panel.add(field1);
			int result = JOptionPane.showConfirmDialog(null, panel, "Test",
					JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE);

			String name = field1.getText();
			playerName = name;

			if(Main.ip != null) {
				client.setName(name);
				f.setTitle("GEESE - " + playerName + " - Connected to: " + Utility.ipArrayToString(ip));
			} else {
				f.setTitle("GEESE - " + playerName);
			}

			f.getContentPane().removeAll();
			f.setContentPane(createContentPanel());
			f.validate();
		}
	}

	private class RefreshButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			table = createGameList();
			f.getContentPane().removeAll();
			f.setContentPane(createContentPanel());
			f.validate();
		}
	}

	private class PingButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			long time = System.currentTimeMillis();
			client.ping();
			long diff = System.currentTimeMillis() - time;
			pingButton.setText("Ping: " + diff + " ms");
		}
	}

	/**
	 * Creates the list of current games
	 * 
	 * @return a JScrollPane containing the list of games
	 */
	private JScrollPane createGameList() {
		Object[][] tmp = client.available();
		final OtpErlangPid[] pids = new OtpErlangPid[tmp.length];
		data = new Object[tmp.length][5];
		for(int i = 0; i < tmp.length; i++) {
			for (int j = 0; j < 3; j++) {
				data[i][j] = tmp[i][j].toString();
			}
			data[i][3] = (Object) (tmp[i][3].toString() + "/" + tmp[i][4].toString());
			pids[i] = (OtpErlangPid) tmp[i][0];
			data[i][4] = "Join";
		}
		JTable table = new JTable(new DefaultTableModel(data, columnNames));

		//Set the column sorting functionality on
		table.setAutoCreateRowSorter(true);

		JScrollPane tableScrollPane = new JScrollPane();
		tableScrollPane.getViewport().add(table);

		table.setGridColor(Color.BLACK);
		table.setBackground(Color.WHITE);

		join =new AbstractAction()
		{
			@Override
			public void actionPerformed(ActionEvent e) {
				int modelRow = Integer.valueOf( e.getActionCommand() );

				boolean join_succeeded = client.join(pids[modelRow]);
				if (join_succeeded) {
					framework = new Framework();
					f.setContentPane(framework);
					f.validate();
					framework.requestFocusInWindow();
				}
			}
		};

		ButtonColumn buttonColumn = new ButtonColumn(table, join, 4);
		buttonColumn.setMnemonic(KeyEvent.VK_D);
		return tableScrollPane;
	}
	
	/**
	 * Updates myId
	 */
	private void updateMyId() {
		OtpErlangAtom getId = new OtpErlangAtom("my_id");
		Main.client.send(getId);
		OtpErlangObject id = Main.client.getAnswer();
		Game.myId = ((OtpErlangPid)id).id();
	}

	public void actionPerformed(ActionEvent ae) {
		String comStr = ae.getActionCommand();
	}

	public static void main(String args[]) {
		SwingUtilities.invokeLater(new Runnable () {
			@Override	
			public void run() {
				new Main("player1");
			}
		});
	}
}
