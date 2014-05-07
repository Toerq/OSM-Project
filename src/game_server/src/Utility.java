
public class Utility {
	
	public static int[] stringToIp(String s) {
		System.out.println(s);
		String[] tokens = s.replaceFirst("^\\{", "").split("\\.|\\,|\\{|\\}");
		
		int[] ip = new int[4];
		for (int i = 0; i < 4; i++) {
			System.out.println(tokens[i]);
			ip[i] = Integer.parseInt(tokens[i]); 
		}
		return ip;
	}
	
}
