package new_version;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;

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
	byte [] ip = {127,0,0,1};
	Jinterface_bank_client client = new Jinterface_bank_client(ip, 3011);
	JFrame f;
	JScrollPane table;
	String playerName;
	String[] columnNames = {"ID", "Name", "Game Type", "Connected Players/Max Players", ""};
	Object[][] data;
	Action join;
	JButton addTableButton, refreshButton, pingButton, changeNameButton;

	Main(String name) {
		playerName = name;
		//int[] ip = {127,0,0,1};
		//client.add("Server1", ip);
		f = new JFrame("GEESE - " + name);
		f.setSize(800, 600);
		f.setLocationRelativeTo(null);

		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		//initializeMenu(f);
		
		//f.getContentPane().add(createServerTable());
		
		JPanel container = setContentPanel();
		f.setContentPane(container);  

		//createServerTable(f);
		//f.add(addTableButton);
		
		f.setVisible(true);
	}

	private JPanel setContentPanel() {
		table = createServerTable();
		//f.add(table);
		
		addTableButton = new JButton("Add new table");
		AddButtonHandler addHandler = new AddButtonHandler();
		addTableButton.addActionListener(addHandler);
		//addTableButton.setBounds(10,400, 50, 50);
		
		refreshButton = new JButton("Refresh");
		RefreshButtonHandler rfHandler = new RefreshButtonHandler();
		refreshButton.addActionListener(rfHandler);
		
		pingButton = new JButton("Ping ?? ms");
		PingButtonHandler pingHandler = new PingButtonHandler();
		pingButton.addActionListener(pingHandler);
		
		
		JPanel panel = new JPanel();
		panel.add(addTableButton);
		
		JPanel container = (JPanel) f.getContentPane();
		GroupLayout layout = new GroupLayout(container);
		container.setLayout(layout);

		layout.setAutoCreateGaps(true);
		layout.setAutoCreateContainerGaps(true);


		/*layout.setHorizontalGroup(
				layout.createSequentialGroup()
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
						.addComponent(table)
						.addComponent(addTableButton)
						//.addComponent(refreshButton)
						//.addComponent(pingButton)
				));

		layout.setVerticalGroup(
				layout.createSequentialGroup()
				.addComponent(table)
				.addComponent(addTableButton)
				.addComponent(refreshButton)
				.addComponent(pingButton)
		);
		*/
		/*layout.setHorizontalGroup(
				layout.createParallelGroup(
						
				)
		)*/

		layout.setHorizontalGroup(
				layout.createSequentialGroup()

				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
								.addComponent(table)
				.addGroup(layout.createSequentialGroup()
								.addComponent(addTableButton)
								.addComponent(refreshButton)
								.addComponent(pingButton)))

		);
		layout.setVerticalGroup(
				layout.createSequentialGroup()
						.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
								.addComponent(table))
						.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
								.addComponent(addTableButton)
								.addComponent(refreshButton)
								.addComponent(pingButton))
		);

		return container;
	}
	
	private class AddButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			
				String[] types = {"Type 1", "Type 2", "Type 3", "Type 4", "Type 5"};
				String[] max = {"6", "8", "10", "12", "14"};
			    JComboBox typeCombo = new JComboBox(types);
			    JComboBox maxCombo = new JComboBox(max);
			    
			    JTextField field1 = new JTextField("");
			    //JTextField field2 = new JTextField("");
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
			    /*if (result == JOptionPane.OK_OPTION) {
			        System.out.println(combo.getSelectedItem()
			            + " " + field1.getText()
			            + " " + field2.getText());
			    } else {
			        System.out.println("Cancelled"); 
			    }*/
			    
			client.add(name, type, maxPlayers);
			f.getContentPane().removeAll();
			f.setContentPane(setContentPanel());
			f.validate();
		}
	}
	
	private class RefreshButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
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
			pingButton.setLabel("Ping: " + diff + " ms");
		}
	}
	
	private JScrollPane createServerTable() {
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
				//JTable table = (JTable)e.getSource();
				int modelRow = Integer.valueOf( e.getActionCommand() );

				/*
				String ipString = (String)table.getValueAt(modelRow,1);
				System.out.println(ipString);
				byte[] ip = Utility.stringToIp(ipString);
				Framework framework = new Framework();
				Framework.serverIP = ip;
				Framework.playerName = playerName;
				f.setContentPane(framework);
				f.validate();
				System.out.println(framework.requestFocusInWindow());
				*/
				client.join(pids[modelRow]);
				Framework framework = new Framework();
				f.setContentPane(framework);
				f.validate();
				System.out.println(framework.requestFocusInWindow());
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
