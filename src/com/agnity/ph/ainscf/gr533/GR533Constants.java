package com.agnity.ph.ainscf.gr533;

public interface GR533Constants {
	
	// ACG Parameter id
	public static byte ACG_PARAMETER_SET_ID = (byte) 0xF2;
	
	// ACG Tags
	public static byte GR533_ACG_TAG1 = (byte) 0xDF;
	public static byte GR533_ACG_TAG2 = 0x47; 
	public static int GR533_ACG_LEN   = 6;
	public static byte GR533_ACG_INDICATOR_LEN = 0x03;
	
	
	// ACG Digits
	public static byte GR533_ACG_DIGITS_TAG   = (byte) 0x84;
	public static byte GR533_ACG_DIGITS_TYPE  = 0x01; 
	public static byte GR533_ACG_DIGITS_NOA   = 0x00;
	public static byte GR533_ACG_DIGITS_NP    = 0x21;
	
	//send Notification Parameter id
	public static byte SEND_NOTFICATION_PARAMETER_SET_ID = (byte) 0xF2;
	public static int SEND_NOTFICATION_PARAMETER_SET_LEN = 7;
	
	//echo Data Tags
	public static byte ECHO_DATA_TAG1 = (byte) 0xDF;
	public static byte ECHO_DATA_TAG2 = (byte) 0x43;
	public static int ECHO_DATA_LEN =4;
	
	//Termination Notification Tags
	public static byte TN_PACKAGE_TYPE = (byte)0xE1;
	public static int TN_TOTAL_LEN = 29;
	public static byte TN_TRANSACTION_ID = (byte)0xC7;
	public static byte TN_TRANSACTION_ID_LEN = (byte)0x00;
	public static byte TN_COMPONENT_SEQ_ID = (byte)0xE8;
	public static byte TN_COMPONENT_SEQ_LEN = (byte)0x19;
	
	public static byte TN_COMPONENT_TYPE_ID = (byte)0xEA;
	public static byte TN_COMPONENT_LEN = (byte)0x17;
	
	public static byte TN_COMPONENT_ID = (byte)0xCF;
	public static byte TN_COMPONENT_ID_LEN = (byte)0x00;
	
	public static byte TN_PARAMETER_ID = (byte)0xF2;
	public static byte TN_PARAMETER_LEN = (byte)0x13;
	
	public static byte TN_TERMINATION_ID1 = (byte)0xDF;
	public static byte TN_TERMINATION_ID2 = (byte)0x46;
	public static byte TN_TERMINATION_ID_LEN = (byte)0x01;
	
	//connect Time Tags
	public static byte CONNECT_TIME_TAG1 = (byte) 0xDE;
	public static byte CONNECT_TIME_TAG2 = (byte) 0x42;
	public static int CONNECT_TIME_LEN =5;
	
	//error code tags
	public static byte ERROR_CODES_ID = (byte)0xD4;
	public static byte ERROR_CODES_LEN = (byte)0x01;
	
	//STR tags
	public static byte GR533_PLAY_ANN_TAG1 = (byte)0xF2;
	public static byte GR533_PLAY_ANN_LEN = (byte)0x03;
	public static byte GR533_PLAY_ANN_TAG2 = (byte)0x82;
	public static byte GR533_PLAY_ANN_INDICATOR_LEN= (byte)0x01;

	
}
