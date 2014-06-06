package new_version;

import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.awt.image.BufferedImage;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;

import com.ericsson.otp.erlang.OtpOutputStream;


public class Utility {
	
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
	public static byte[] arrayPrepend(byte[] tmp) {
		 byte[] prepend = {(byte)131};
		 byte[] data = new byte[prepend.length + tmp.length];
		 System.arraycopy(prepend, 0, data, 0, prepend.length);
		 System.arraycopy(tmp, 0, data, prepend.length, tmp.length);
		return data;
	}
	
	public static byte[] arrayPrepend(OtpOutputStream availableStream) {
		byte[] data = availableStream.toByteArray();
		return arrayPrepend(data);
	}
   
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

	// Convenience method to draw from center with radius
	public static void drawCircle(Graphics2D cg, int xCenter, int yCenter, int r) {
		cg.drawOval(xCenter-r, yCenter-r, 2*r, 2*r);
	}//end drawCircle

	public static boolean containsArray(ArrayList<BufferedImage[]> list, BufferedImage[] imgs) {
		for (int i = 0; i < list.size(); i++) {
			if (Arrays.equals((list.get(i)), imgs)){
				return true;
			}
		}
		return false;
	}
	
	public static boolean contains(final int[] id, final Integer key) {
	    for ( final int e : id )
	        if ( e == key || key != null && key.equals( e ) )
	            return true;
	    return false;
	}
	
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