import java.util.LinkedList;

public class Mp3file {
	private int counter, sync, bitrate, frequency;
	private short mode, modeX, emph;
	private MPEGVersion id;
	private MPEGLayer layer;
	private boolean protect, home;

	public Mp3file() {
		counter = 0;
	}

	public LinkedList<Byte> filp(LinkedList<Byte> music) {
		processHeader(grabHeader(music));
		if(protect) counter += 2; // Clear CRC, which is 16 bits
		// TODO: Deal with Side Information
		// TODO: Isolate Main/Ancillary data
		// TODO: Flip all main data bits
		return music;
	}

	/**
	 * Header Section starts v
	 */

	private Byte[] grabHeader(LinkedList<Byte> music) {
		Byte[] fh = new Byte[4];
		fh[0] = music.get(counter++);
		fh[1] = music.get(counter++);
		fh[2] = music.get(counter++);
		fh[3] = music.get(counter++);
		return fh;
	}

	private void processHeader(Byte[] fh) {
		sync = syncValue(fh[0], fh[1]);
		id = idValue(fh[1]);
		layer = layerValue(fh[1]);
		protect = isProtected(fh[1]);
		bitrate = bitrateValue(fh[2]);
		frequency = freqValue(fh[2]);
		mode = modeValue(fh[3]);
		copycheck(fh[3]);
		home = homeValue(fh[3]);
		emph = emphasis(fh[3]);
	}

	private int syncValue(Byte fh1, Byte fh2) {
		int t = fh1 << 4;
		fh2 = (byte)((fh2 & 0xf0) >> 4);
		return t | fh2;
	}

	private MPEGVersion idValue(Byte fh) {
		if((fh & 0x8) == 0x8)	return MPEGVersion.MPEG1;
		else	return MPEGVersion.MPEG2;
	}

	private MPEGLayer layerValue(Byte fh) {
		int t = fh & 0x6;
		if(t == 0x2) return MPEGLayer.Layer3;
		else if(t==0x4) return MPEGLayer.Layer2;
		else return MPEGLayer.Layer1;
	}

	private boolean isProtected(Byte fh) {return (fh & 0x1) == 0x1;}

	private int bitrateValue(Byte b) {return layer.getBitrate(id, b);}

	private int freqValue(byte b) {
		int t = ((b & 0x0c) >> 2);
		if(MPEGVersion.MPEG1 == id) {
			if(t == 0) return 44100;
			else if (t == 1) return 48000;
			else	return 32000;
		} else {
			if(t == 0) return 22050;
			else if (t == 1) return 24000;
			else	return 16000;
		}
	}

	private short modeValue(byte b) {
		short m = ((b & 0xb0) >> 6);
		if(m == 1) modeX = ((b & 0x30) >> 4);
		else modeX = 0; // We don't care about the mode extension value if m != 1
		return m;
	}

	private void copycheck(byte b) {
		if((b & 0x08) == 8)
		 // Throw exception
	}

	private boolean homeValue(byte b) {return (b & 0x04) == 4;}

	private short emphasis(byte b) {return (short)(b & 0x03);}

	 /**
	  * Header section ends ^
		* Process the side information for this file. v
		*/
}
