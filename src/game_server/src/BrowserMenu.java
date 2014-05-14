import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;



public class BrowserMenu extends JFrame {
	
	Jinterface_bank_client client = new Jinterface_bank_client("127.0.0.1", 3010);
	//int browserIp[] = {127,0,0,1};
	String[] columnNames = {"Server name", "Ip", ""};
	Object[][] data;
	
	public BrowserMenu() {
		Object[][] tmp = client.available();
		data = new Object[tmp.length][3];
		for(int i = 0; i < tmp.length; i++) {
			for (int j = 0; j < 2; j++) {
				data[i][j] = tmp[i][j];
			}
			data[i][2] = "Join";
		}
		
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.setTitle("Creating a Table Example");
        this.setSize(700,200);
      
        //This will center the JFrame in the middle of the screen
        this.setLocationRelativeTo(null);
        
        //Create the JTable using the ExampleTableModel implementing 
        //the AbstractTableModel abstract class
        JTable table = new JTable(new DefaultTableModel(data, columnNames));
        
        //Set the column sorting functionality on
        table.setAutoCreateRowSorter(true);
        
        //Uncomment the next line if you want to turn the grid lines off
      //  table.setShowGrid(false);
        
        //Change the colour of the table - black for gridlines 
        //white for background
        table.setGridColor(Color.BLACK);
        table.setBackground(Color.WHITE);
        
        //Place the JTable object in a JScrollPane for a scrolling table
        JScrollPane tableScrollPane = new JScrollPane(table);

        add(tableScrollPane);
        setVisible(true);
        
    	Action join = new AbstractAction()
		{

			@Override
			public void actionPerformed(ActionEvent e) {
				JTable table = (JTable)e.getSource();
		        int modelRow = Integer.valueOf( e.getActionCommand() );
		        
		        String ipString = (String)table.getValueAt(modelRow,1);
		        int[] ip = Utility.stringToIp(ipString);
		        
		        Game game = new Game();
		        Player clientPlayer = new Player(20,20, "player1");
		        System.out.println(getFocusOwner());
                
		        clientPlayer.addPlayerToServer(ip, client);
                game.run(client, clientPlayer);
                System.exit(0);
				
			}
		};
		
		ButtonColumn buttonColumn = new ButtonColumn(table, join, 2);
		buttonColumn.setMnemonic(KeyEvent.VK_D);
	}
	
	
	public static void main(String [] args){
		
		BrowserMenu bm = new BrowserMenu();
		
	}
	
}
