package new_version;

import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.util.Arrays;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;

import com.ericsson.otp.erlang.OtpErlangPid;

public class Main implements ActionListener {
	//static byte [] ip = {(byte) 130, (byte) 238, (byte) 93, (byte) 252};
	static byte [] ip = null;
	//public static Jinterface_client client = new Jinterface_client(ip, 3010);
	//static byte [] ip = null;
	public static Jinterface_client client = null;
	JFrame f;
	JScrollPane table;
	String playerName;
	String[] columnNames = {"ID", "Name", "Game Type", "Connected Players/Max Players", ""};
	Object[][] data;
	Action join;
	JButton connectButton, addTableButton, refreshButton, pingButton, changeNameButton;
	Framework framework = null;

	Main(String name) {
		playerName = name;
		//client.setName(playerName);
		//client.getMyId();
		f = new JFrame("GEESE - " + name);
		f.setSize(1280, 720);
		f.setLocationRelativeTo(null);
		
		// JFrame.EXIT_ON_CLOSE = 3
		f.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
		//initializeMenu(f);

		table = new JScrollPane();
		JPanel container = setContentPanel();
		f.setContentPane(container);  
		
		f.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentHidden(ComponentEvent e) {
            	
            	Framework.gameState = Framework.GameState.GAMEOVER;
            	System.out.println("Game over...");
            	
            	try {
					Thread.sleep(500);
				} catch (InterruptedException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
            	Main.client.removePlayer();
                //System.out.println("Replace sysout with your method call");
                
            	f.dispose();
                System.exit(0);
            }
        });

		f.setVisible(true);
	} 

	private JPanel setContentPanel() {

		createButtons();

		JPanel panel = new JPanel();
		panel.add(addTableButton);

		JPanel container = (JPanel) f.getContentPane();
		
		if(Main.ip == null) {
			System.out.println("IP == NULL");
			setSimpleLayout(container);
		} else {
			System.out.println("IP != NULL");
			setCompleteLayout(container);
		}

		return container;
	}

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
			JComboBox typeCombo = new JComboBox(types);
			JComboBox maxCombo = new JComboBox(max);

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
			table = createServerTable();
			f.getContentPane().removeAll();
			f.setContentPane(setContentPanel());
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
			Main.client = new Jinterface_client(Main.ip, Integer.parseInt(portString));
			//System.out.println("After connect: " + client + " - current IP: " + Arrays.toString(Main.ip));
			if(ip != null) {
				Main.client.setName(playerName);
				Main.client.getMyId();
			
				table = createServerTable();

				f.setTitle("GEESE - " + playerName + " - Connected to: " + Arrays.toString(ip));
			} else {
				f.setTitle("GEESE - " + playerName);
				table = new JScrollPane();
			}
			
			f.getContentPane().removeAll();
			f.setContentPane(setContentPanel());
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
				f.setTitle("GEESE - " + playerName + " - Connected to: " + Arrays.toString(ip));
			} else {
				f.setTitle("GEESE - " + playerName);
			}
			
			f.getContentPane().removeAll();
			f.setContentPane(setContentPanel());
			f.validate();
		}
	}
	private class RefreshButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			table = createServerTable();
			f.getContentPane().removeAll();
			f.setContentPane(setContentPanel());
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

	private JScrollPane createServerTable() {
		System.out.println("In createServerTable");
		Object[][] tmp = client.available();
		final OtpErlangPid[] pids = new OtpErlangPid[tmp.length];
		data = new Object[tmp.length][5];
		for(int i = 0; i < tmp.length; i++) {
			for (int j = 0; j < 3; j++) {
				data[i][j] = tmp[i][j].toString();
				System.out.println("Tables: " + data[i][j]);
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
				//JTable table = (JTable)e.getSource();
				int modelRow = Integer.valueOf( e.getActionCommand() );

				boolean join_succeeded = client.join(pids[modelRow]);
				System.out.println("join: " + join_succeeded);
				if (join_succeeded) {
					framework = new Framework();
					f.setContentPane(framework);
					f.validate();
					System.out.println(framework.requestFocusInWindow());
				}
			}
		};

		ButtonColumn buttonColumn = new ButtonColumn(table, join, 4);
		buttonColumn.setMnemonic(KeyEvent.VK_D);
		return tableScrollPane;
	}

	private void initializeMenu(final JFrame f) {
		JMenuBar jmb = new JMenuBar();

		JMenu jmFile = new JMenu("File");
		JMenuItem jmiOpen = new JMenuItem("Open");
		JMenuItem jmiClose = new JMenuItem("Close");
		JMenuItem jmiSave = new JMenuItem("Save");
		JMenuItem jmiExit = new JMenuItem("Exit");
		jmFile.add(jmiOpen);
		jmFile.add(jmiClose);
		jmFile.add(jmiSave);
		jmFile.addSeparator();
		jmFile.add(jmiExit);
		jmb.add(jmFile);

		JMenu jmOptions = new JMenu("Options");
		JMenu a = new JMenu("A");
		JMenuItem b = new JMenuItem("B");
		JMenuItem c = new JMenuItem("C");
		JMenuItem d = new JMenuItem("D");
		a.add(b);
		a.add(c);
		a.add(d);
		jmOptions.add(a);

		JMenu e = new JMenu("E");
		e.add(new JMenuItem("F"));
		e.add(new JMenuItem("G"));
		jmOptions.add(e);

		jmb.add(jmOptions);

		JMenu jmHelp = new JMenu("Help");
		JMenuItem jmiAbout = new JMenuItem("About");
		jmHelp.add(jmiAbout);
		jmb.add(jmHelp);

		jmiOpen.addActionListener(this);
		jmiClose.addActionListener(this);
		jmiSave.addActionListener(this);
		jmiExit.addActionListener(this);
		b.addActionListener(this);
		c.addActionListener(this);
		d.addActionListener(this);
		jmiAbout.addActionListener(this);

		f.setJMenuBar(jmb);
	}

	public void actionPerformed(ActionEvent ae) {
		String comStr = ae.getActionCommand();
		System.out.println(comStr + " Selected");
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
