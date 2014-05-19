package moon_lander;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;

public class Main implements ActionListener {
	byte [] ip = {127,0,0,1};
	Jinterface_bank_client client = new Jinterface_bank_client(ip, 3010);
	String playerName;
	String[] columnNames = {"Server name", "Ip", ""};
	Object[][] data;
	Action join;

	Main(String name) {
		playerName = name;
		int[] ip = {127,0,0,1};
		client.add("Server1", ip);
		final JFrame f = new JFrame("GEESE - " + name);
		f.setSize(800, 600);
		f.setLocationRelativeTo(null);

		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		initializeMenu(f);

		createServerTable(f);
		f.setVisible(true);
	}
	
	
	
	private void createServerTable(final JFrame f) {
		Object[][] tmp = client.available();
		data = new Object[tmp.length][3];
		for(int i = 0; i < tmp.length; i++) {
			for (int j = 0; j < 2; j++) {
				data[i][j] = tmp[i][j];
			}
			data[i][2] = "Join";
		}
		JTable table = new JTable(new DefaultTableModel(data, columnNames));

		//Set the column sorting functionality on
		table.setAutoCreateRowSorter(true);

		JScrollPane tableScrollPane = new JScrollPane(table);
		f.add(tableScrollPane);

		table.setGridColor(Color.BLACK);
		table.setBackground(Color.WHITE);

		join =new AbstractAction()
		{

			@Override
			public void actionPerformed(ActionEvent e) {
				JTable table = (JTable)e.getSource();
				int modelRow = Integer.valueOf( e.getActionCommand() );

				String ipString = (String)table.getValueAt(modelRow,1);
				System.out.println(ipString);
				byte[] ip = Utility.stringToIp(ipString);
				Framework framework = new Framework();
				Framework.serverIP = ip;
				Framework.playerName = playerName;
				f.setContentPane(framework);
				f.validate();
				System.out.println(framework.requestFocusInWindow());
			}
		};

		ButtonColumn buttonColumn = new ButtonColumn(table, join, 2);
		buttonColumn.setMnemonic(KeyEvent.VK_D);
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

