import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.LinkedList;

/**
 * @author river226
 */
public class Iom {

	private static String filename;
	private static LinkedList<Byte> music;
	private static Mp3file m;
	private static File f; 

	public static void main(String[] args) {
		filename = ".\\src\\test.mp3";
		try {
			readInFile();
			flipFile();
			writeFile();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private static void writeFile() throws FileNotFoundException, IOException {
		FileOutputStream fos = new FileOutputStream(f.getCanonicalPath());
		System.out.println("write");
		while(!music.isEmpty())	
			fos.write(music.pop());
	}

	private static void flipFile() {
		m = new Mp3file();
		music = m.filp(music);
	}

	private static void readInFile() throws IOException{
		music = new LinkedList<Byte>();
		f = new File(filename);
		System.out.println("read");
		System.out.println(f.getCanonicalPath());
		FileInputStream fis = new FileInputStream(f.getCanonicalPath());
		while(fis.available() > 0)  
			music.add((byte) fis.read()); 
		fis.close();
	}
}
