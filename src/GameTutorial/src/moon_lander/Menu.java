package moon_lander;

import javax.swing.*;

import java.awt.Container;
import java.awt.event.*;

public class Menu extends JFrame
{
	private static final int WIDTH = 400;
	private static final int HEIGHT = 300;
	private String name;
	private JLabel chooseName;
	private JTextField textField;
	private JButton ok;

	public Menu()
	{
		chooseName = new JLabel("Choose player name: ", SwingConstants.RIGHT);		
		textField = new JTextField(10);


		//SPecify handlers for each button and add (register) ActionListeners to each button.
		ok = new JButton("OK");
		okButtonHandler sbHandler = new okButtonHandler();
		ok.addActionListener(sbHandler);

		Container panel = getContentPane();
		GroupLayout layout = new GroupLayout(panel);
		panel.setLayout(layout);

		layout.setAutoCreateGaps(true);
		layout.setAutoCreateContainerGaps(true);


		layout.setHorizontalGroup(
				layout.createSequentialGroup()
						.addComponent(chooseName)
						.addComponent(textField)
						.addComponent(ok)
				);

		layout.setVerticalGroup(
				layout.createSequentialGroup()
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
						.addComponent(chooseName)
						.addComponent(textField)
						.addComponent(ok)
				));

		setSize(WIDTH, HEIGHT);
		setLocationRelativeTo(null);
		setVisible(true);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
	}


	private class okButtonHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			name = (textField.getText());
			getRootPane().setVisible(false);
			Main game = new Main(name);
			
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