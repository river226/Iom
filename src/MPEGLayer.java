
public enum MPEGLayer {
	Layer1, Layer2, Layer3;
	
	public int getBitrate(MPEGVersion m, byte b) {
		
		if(this == Layer1)
			return layer1BR(b);
		else if (this == Layer2) 
			return layer2BR(b);
		else
			return layer3BR(m, b);
	}

	private int layer1BR(byte b) {
		b = (byte) ((b & 0xf0) >> 4);
		return (b * 32);
	}

	private int layer2BR(byte b) {
		int x = ((b & 0xf0) >> 4);
		switch(x){
		case(2):
			return 48;
		case(3):
			return 56;
		case(4):
			return 64;
		case(5):
			return 80;
		case(6):
			return 96;
		case(7):
			return 112;
		case(8):
			return 128;
		case(9):
			return 160;
		case(10):
			return 192;
		case(11):
			return 224;
		case(12):
			return 256;
		case(13):
			return 320;
		case(14):
			return 384;
		default:
			return 32;
		}	
	}

	private int layer3BR(MPEGVersion m, byte b) {
		int x = ((b & 0xf0) >> 4);
		if(m == MPEGVersion.MPEG1){
			switch(x){
			case(2):
				return 40;
			case(3):
				return 48;
			case(4):
				return 56;
			case(5):
				return 64;
			case(6):
				return 80;
			case(7):
				return 96;
			case(8):
				return 112;
			case(9):
				return 128;
			case(10):
				return 160;
			case(11):
				return 192;
			case(12):
				return 224;
			case(13):
				return 256;
			case(14):
				return 320;
			default:
				return 32;
			}
		} else {
			switch(x){
			case(2):
				return 16;
			case(3):
				return 24;
			case(4):
				return 32;
			case(5):
				return 64;
			case(6):
				return 80;
			case(7):
				return 56;
			case(8):
				return 64;
			case(9):
				return 128;
			case(10):
				return 160;
			case(11):
				return 112;
			case(12):
				return 128;
			case(13):
				return 256;
			case(14):
				return 320;
			default:
				return 8;
			}
		}
	}
}
