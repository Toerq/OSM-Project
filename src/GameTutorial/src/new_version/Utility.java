package new_version;

import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Arrays;
import com.ericsson.otp.erlang.OtpOutputStream;

public class Utility {
	
	/**
	 * Converts a string to a byte array representing the corresponding ip adress
	 * 
	 * @param s The string to be converted
	 * @return The ip byte array
	 */
	public static byte[] stringToIp(String s) {
		System.out.println(s);
		String[] tokens = s.replaceFirst("^\\{", "").split("\\.|\\,|\\{|\\}");
		byte[] ip = new byte[4];
		try {
			for (int i = 0; i < 4; i++) {
				System.out.println(tokens[i]);
				
					ip[i] = (byte) Integer.parseInt(tokens[i]); 
			}
		}catch(Exception e) {
			ip = null;
		}
		return ip;
	}
	
	/**
	 * Prepends the byte '131' to the byte array tmp
	 * 
	 * @param tmp The byte array to be prepended
	 * @return tmp with the byte '131' prepended
	 */
	private static byte[] arrayPrepend(byte[] tmp) {
		 byte[] prepend = {(byte)131};
		 byte[] data = new byte[prepend.length + tmp.length];
		 System.arraycopy(prepend, 0, data, 0, prepend.length);
		 System.arraycopy(tmp, 0, data, prepend.length, tmp.length);
		return data;
	}
	
	/**
	 * Returns the corresponding byte array of availableStream with the byte '131' prepended to it
	 * The reason for this is to get a byte representation of a message that the server will understand
	 * 
	 * @param availableStream the stream to be converted to a byte array
	 * @return The corresponding byte array to availableStream with '131' prepended to it
	 */
	public static byte[] arrayPrepend(OtpOutputStream availableStream) {
		byte[] data = availableStream.toByteArray();
		return arrayPrepend(data);
	}
   
	/**
	 * Makes image compatible to local graphics environment
	 * 
	 * @param image The image to make compatible
	 * @return The new compatible version of the image
	 */
	public static BufferedImage toCompatibleImage(BufferedImage image)
    {
    	//Get current GraphicsConfiguration
        GraphicsConfiguration gfx_config
                = GraphicsEnvironment
                .getLocalGraphicsEnvironment()
                .getDefaultScreenDevice()
                .getDefaultConfiguration();

        /*
         * if image is already compatible and optimized for current system
         * settings, simply return it
         */
        if (image.getColorModel().equals(gfx_config.getColorModel()))
        {
            image.setAccelerationPriority(1.0f);
            return image;
        }

        // image is not optimized, so create a new image that is
        BufferedImage new_image = gfx_config.createCompatibleImage(image.getWidth(), image.getHeight(), image.getTransparency());

        // get the graphics context of the new image to draw the old image on
        Graphics2D g2d = (Graphics2D) new_image.getGraphics();

        // actually draw the image and dispose of context no longer needed
        g2d.drawImage(image, 0, 0, null);
        g2d.dispose();

        new_image.setAccelerationPriority(1.0f);

        // return the new optimized image
        return new_image;
    }

	/**
	 * Check if the list of BufferedImage arrays 'list' contains the specified image array
	 * 
	 * @param list The list of arrays
	 * @param imgs The array of images
	 * @return true if the list contains the array, otherwise false is returned
	 */
	public static boolean containsArray(ArrayList<BufferedImage[]> list, BufferedImage[] imgs) {
		for (int i = 0; i < list.size(); i++) {
			if (Arrays.equals((list.get(i)), imgs)){
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Check if the int array 'arr' contains the Integer 'i'
	 *  
	 * @param arr 
	 * @param i 
	 * @return true if 'arr' contains 'i' and false otherwise
	 */
	public static boolean contains(final int[] arr, final Integer i) {
	    for ( final int e : arr )
	        if ( e == i || i != null && i.equals( e ) )
	            return true;
	    return false;
	}
	
	/**
	 * Converts an byte array to a string representing the corresponding ip adress
	 * 
	 * @param ip The byte array to be converted
	 * @return The ip string
	 */
	public static String ipArrayToString(byte[] ip) {
		String ipString = new String();
		for(int i = 0; i < ip.length; i++) {
			if(ip[i] < 0) {
				ipString = ipString + (ip[i]+256) + ".";
			} else {
				ipString = ipString + ip[i] + ".";
			}
		}
		return ipString;
	}

}