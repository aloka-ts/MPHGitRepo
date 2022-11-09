package com.agnity.ph.ainscf.lidb;

public final class GnConstants {

	private GnConstants() {
		
	}
	
	public static final byte GN_NOA_PUBLIC_PRESENTATION_ALLOWED=0x00;
	
	public static final byte GN_NOA_PUBLIC_PRESENTATION_RESTRICTED=0x02;
	
	//Generic Name Query fixed values
	public static final byte GN_PARAMETER_SET_ID = (byte) 0xF2;
	public static final byte GN_GENERIC_NAME_ID=(byte) 0x97;
	public static final byte GN_COMPONENT_ID_IDENTIFIER=(byte) 0xCF;
	public static final byte GN_COMPONENT_ID_LENGTH=0x01;
	public static final byte GN_OPCODE_ID = (byte) 0xD0;
	public static final byte GN_OPCODE_LENGTH = 0x02;
	public static final byte GN_OPCODE_ACG_TAG1=0x07;
	public static final byte GN_OPCODE_ACG_TAG2=0x01;
	public static final byte GN_DIGITS_IDENTIFIER_ANI_CALLING_PARTY_TAG1=0x49;
	public static final byte GN_DIGITS_IDENTIFIER_ANI_CALLING_PARTY_TAG2=(byte) 0xDF;
	public static final byte GN_DIGITS_LENGTH=0x07;
	public static final byte GN_ACG_INDICATOR_ID_TAG1=0x47;
	public static final byte GN_ACG_INDICATOR_ID_TAG2=(byte) 0xDF;
	public static final byte GN_ACG_INDICATOR_LENGTH=0x03;

	
	//mandatory fixed lengths
	public static final int GN_PARAMETER_SET_ID_LENGTH = 2;
	public static final int GN_DIAL_TONE_SERVICE_REQUESTER_ID_LENGTH = 4;
	public static final int GN_COMPONENT_ID_IDENTIFIER_LENGTH=1;
	public static final int GN_COMPONENT_ID_LENGTH_LEN=1;
	public static final int GN_FIXED_OCTET_OF_LENGTH_ONE=1;
	public static final int GN_FIXED_OCTET_OF_LENGTH_TWO=2;
	public static final int GN_FIXED_OCTET_OF_LENGTH_THREE=3;
	public static final int GN_FIXED_OCTET_OF_LENGTH_FOUR=4;





	//presentation type
	public static final byte GN_PRESENTATION_TYPE_PRESENTATION_ALLOWED=0x00;
	public static final byte GN_PRESENTATION_TYPE_PRESENTATION_RESTRICTED=0x01;
	public static final byte GN_PRESENTATION_TYPE_BLOCKING_TOGGLE=0x02;
	public static final byte GN_PRESENTATION_TYPE_NO_INDICATION=0x03;

	//generic name availability
	public static final byte GN_NAME_AVAILABLE=0x00;
	public static final byte GN_NAME_NOT_AVAILABLE=0x01;
    
	//Generic Name error response constants
	public static final int FIXED_LENGTH_FOR_GN_ERROR_RESPONSE = 16;
	public static final int FIXED_LENGTH_FOR_GN_REJECT_RESPONSE = 18;

	public static final byte GN_OPERATION_FAMILY = 0x07;
	public static final byte GN_OPERATION_SPECIFIER = 0x01;

}
