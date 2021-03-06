package geese;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;

/**
 * Framework that controls the game (Game.java) that created it, update it and draw it on the screen.
 * Taken and modified from www.gametutorial.net
 *
 * @author Niklas Hokenstrom, Jonas Nilson
 */

public class Framework extends Canvas {

	/**
	 * Width of the frame.
	 */
	public static int frameWidth;
	/**
	 * Height of the frame.
	 */
	public static int frameHeight;

	/**
	 * Time of one second in nanoseconds.
	 * 1 second = 1 000 000 000 nanoseconds
	 */
	public static final long secInNanosec = 1000000000L;

	/**
	 * Time of one millisecond in nanoseconds.
	 * 1 millisecond = 1 000 000 nanoseconds
	 */
	public static final long milisecInNanosec = 1000000L;

	/**
	 * FPS - Frames per second
	 * How many times per second the game should update?
	 */
	private final int GAME_FPS = 32;
	/**
	 * Pause between updates. It is in nanoseconds.
	 */
	private final long GAME_UPDATE_PERIOD = secInNanosec / GAME_FPS;

	/**
	 * Possible states of the game
	 */
	public static enum GameState{STARTING, VISUALIZING, GAME_CONTENT_LOADING, MAIN_MENU, OPTIONS, PLAYING, GAMEOVER, DESTROYED}
	/**
	 * Current state of the game
	 */
	public static GameState gameState;

	/**
	 * Elapsed game time in nanoseconds.
	 */
	private long gameTime;
	// It is used for calculating elapsed time.
	private long lastTime;

	// The actual game
	private Game game;
	
	// The aim cursor
	private Cursor aimCursor;

	/**
	 * Image for menu.
	 */
	private BufferedImage startScreen;
	
	/**
	 * Creates a new framework for the application
	 */
	public Framework ()
	{
		super();
		gameState = GameState.VISUALIZING;
		// If you will draw your own mouse cursor or if you just want that mouse cursor disapear, 
		// insert "true" into if condition and mouse cursor will be removed.
	
		if(true)
		{
			URL aimCursorUrl = this.getClass().getResource("/new_images/aim.png");
			BufferedImage aimCursorImg = null;
			try {
				aimCursorImg = ImageIO.read(aimCursorUrl);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			aimCursor = Toolkit.getDefaultToolkit().createCustomCursor(aimCursorImg, new Point(0, 0), null);
			this.setCursor(aimCursor);
		}
		
		//We start game in new thread.
		Thread gameThread = new Thread() {
			@Override
			public void run(){
				gameLoop();
			}
		};
		gameThread.start();
	}


	/**
	 * Load files - images, sounds, ...
	 * This method is intended to load files for this class, files for the actual game can be loaded in Game.java.
	 */
	private void loadContent()
	{
		try
		{
			URL startScreenUrl = this.getClass().getResource("/new_images/startscreen.png");
			startScreen = ImageIO.read(startScreenUrl);
		}
		catch (IOException ex) {
			Logger.getLogger(Framework.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	/**
	 * In specific intervals of time (GAME_UPDATE_PERIOD) the game is updated and then the game is drawn on the screen.
	 */
	private void gameLoop()
	{
		// This two variables are used in VISUALIZING state of the game. We used them to wait some time so that we get correct frame/window resolution.
		long visualizingTime = 0, lastVisualizingTime = System.nanoTime();

		// This variables are used for calculating the time that defines for how long we should put threat to sleep to meet the GAME_FPS.
		long beginTime, timeTaken, timeLeft;

		while(true)
		{
			beginTime = System.nanoTime();

			switch (gameState)
			{
			case PLAYING:
				gameTime += System.nanoTime() - lastTime;
				game.updateGame(gameTime, mousePosition());
				lastTime = System.nanoTime();
				break;
			case GAMEOVER:
				//...
				break;
			case MAIN_MENU:
				//...
				break;
			case STARTING:
				// Load files - images, sounds, ...
				loadContent();

				// When all things that are called above finished, we change game status to main menu.
				gameState = GameState.MAIN_MENU;
				break;
			case VISUALIZING:
				// On Ubuntu OS (when I tested on my old computer) this.getWidth() method doesn't return the correct value immediately (eg. for frame that should be 800px width, returns 0 than 790 and at last 798px). 
				// So we wait one second for the window/frame to be set to its correct size. Just in case we
				// also insert 'this.getWidth() > 1' condition in case when the window/frame size wasn't set in time,
				// so that we although get approximately size.
				if(this.getWidth() > 1 && visualizingTime > secInNanosec)
				{
					frameWidth = this.getWidth();
					frameHeight = this.getHeight();
					
					// When we get size of frame we change status.
					gameState = GameState.STARTING;
				}
				else
				{
					visualizingTime += System.nanoTime() - lastVisualizingTime;
					lastVisualizingTime = System.nanoTime();
				}
				break;
			}

			// Repaint the screen.
			repaint();

			// Here we calculate the time that defines for how long we should put threat to sleep to meet the GAME_FPS.
			timeTaken = System.nanoTime() - beginTime;
			timeLeft = (GAME_UPDATE_PERIOD - timeTaken) / milisecInNanosec; // In milliseconds
			// If the time is less than 10 milliseconds, then we will put thread to sleep for 10 millisecond so that some other thread can do some work.
			if (timeLeft < 10) 
				timeLeft = 10; //set a minimum
			try {
				//Provides the necessary delay and also yields control so that other thread can do work.
				Thread.sleep(timeLeft);
			} catch (InterruptedException ex) { }
		}
	}

	/**
	 * Draw the game to the screen. It is called through repaint() method in GameLoop() method.
	 */
	@Override
	public void draw(Graphics2D g2d)
	{
		switch (gameState)
		{
		case PLAYING:
			game.draw(g2d, mousePosition());
			break;
		case GAMEOVER:
			break;
		case MAIN_MENU:
			g2d.drawImage(startScreen, 0, 0, frameWidth, frameHeight, null);
			break;
		}
	}

	/**
	 * Starts new game.
	 */
	private void newGame()
	{
		// We set gameTime to zero and lastTime to current time for later calculations.
		gameTime = 0;
		lastTime = System.nanoTime();
		game = new Game();
	}

	/**
	 * Returns the position of the mouse pointer in game frame/window.
	 * If mouse position is null than this method return 0,0 coordinate.
	 * 
	 * @return Point of mouse coordinates.
	 */
	private Point mousePosition()
	{
		try
		{
			Point mp = this.getMousePosition();
			if(mp != null) {
				mp = new Point(this.getMousePosition().x + 13, this.getMousePosition().y + 13);
				return mp;
			}
			else
				return new Point(0, 0);
		}
		catch (Exception e)
		{
			return new Point(0, 0);
		}
	}

	/**
	 * This method is called when keyboard key is released.
	 * 
	 * @param e KeyEvent
	 */
	@Override
	public void keyReleasedFramework(KeyEvent e)
	{
		switch (gameState)
		{
		case MAIN_MENU:
			newGame();
			break;
		}
	}

	/**
	 * This method is called when mouse button is clicked.
	 * 
	 * @param e MouseEvent
	 */
	@Override
	public void mouseClicked(MouseEvent e)
	{

	}

}
