package com.agnity.ph.ainscf.lidb;

public class IcdcConstants {
	private IcdcConstants() {
	}

	public static final int ICDC_MANDATORY_LEN = 22;
	// APPLICATION ERRROR
	public static final int TOTAL_FIXED_LEN_APP_ERR = 4;
	public static final int TOTAL_FIXED_LEN_PROTOCOL_ERR = 3;
	
	// fixed fields for ICDC Query response encoding
	public static final byte PARAMETER_SET_ID = (byte) 0xF2;
	public static final byte ICDR_IDENTIFIER_TAG1 = (byte) 0xDF;
	public static final byte ICDR_IDENTIFIER_TAG2 = 0x56;
	public static final byte ICDR_LENGTH = 0x01;

}
