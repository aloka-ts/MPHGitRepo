package com.agnity.ph.ainscf.lidb;

public final class OlnsConstants {

	private OlnsConstants() {

	}
	public static final int OLNS_PARAM_SET_LEN = 2;
	public static final int OLNS_ORIG_BILLING_OR_SERV_IND_LEN = 6;
	public static final int OLNS_ADDITIONAL_ORIG_BILLING_OR_SERV_IND_LEN = 5;
	public static final int OLNS_TREATMENT_IND_LENGTH_OCTET = 4;
	public static final int OLNS_SERV_EQP_IND_LENGTH_OCTET = 4;
	public static final int OLNS_ORIG_IC_IND_LENGTH_OCTET = 5;
	public static final int OLNS_ORIG_IC_DIGITS_LENGTH_OCTET = 9;
	public static final int OLNS_ORIG_INC_DIGITS_LENGTH_OCTET = 9;
	public static final int OLNS_FOREIGN_LANG_ID_LENGTH_OCTET = 4;
	public static final int OLNS_GENERIC_NAME_LENGTH_OCTET = 3;
	public static final int OLNS_ALPHANUMERIC_STRING_LENGTH_OCTET = 3;
	public static final byte OLNS_DISALWD_CARD_ISSUER_CODE_LENGTH_OCTET = 7;
	public static final int OLNS_ORIG_LIST_SERV_IND_LENGTH_OCTET = 4;
	public static final int OLNS_ACC_OWNER_LEN_OCTET = 8;
	public static final int OLNS_BILLING_SER_PROV_LEN_OCTET = 8;
	//Error Length
	public static final int TOTAL_FIXED_LEN_APP_ERR = 4;
	
	public static final byte OLNS_PARAM_SET_ID = (byte) 0xF2;
	public static final byte OLNS_ORIG_BILLING_SER_IND_TAG1 = (byte) 0xDF;
	public static final byte OLNS_ORIG_BILLING_SER_IND_TAG2 = (byte) 0x5E;
	public static final byte OLNS_ORIG_BILLING_SER_IND_LENGTH = 0x03;
	public static final byte OLNS_TREATMENT_IND_TAG1 = (byte) 0xDF;
	public static final byte OLNS_TREATMENT_IND_TAG2 = (byte) 0x6C;
	public static final byte OLNS_TREATMENT_IND_LENGTH = (byte) 0x01;
	public static final byte OLNS_SERV_EQP_IND_TAG1 = (byte) 0xDF;
	public static final byte OLNS_SERV_EQP_IND_TAG2 = (byte) 0x68;
	public static final byte OLNS_SERV_EQP_IND_LENGTH = (byte) 0x01;
	public static final byte OLNS_ORIG_IC_IND_TAG1 = (byte) 0xDF;
	public static final byte OLNS_ORIG_IC_IND_TAG2 = 0x4B;
	public static final byte OLNS_ORIG_IC_IND_LENGTH = 0x02;
	public static final byte OLNS_ADDITIONAL_ORIG_BILLING_SER_TAG1 = (byte) 0xDF;
	public static final byte OLNS_ADDITIONAL_ORIG_BILLING_SER_TAG2 = 0x79;
	public static final byte OLNS_ADDITIONAL_ORIG_BILLING_SER_LENGTH = 0x02;
	public static final byte OLNS_ORIG_IC_DIGITS_TAG1 = (byte) 0xDF;
	public static final byte OLNS_ORIG_IC_DIGITS_TAG2 = (byte) 0x49;
	public static final byte OLNS_ORIG_IC_DIGITS_LENGTH = (byte) 0x06;
	public static final byte OLNS_ORIG_IC_DIGITS_TYPE = 0x19;
	public static final byte OLNS_ORIG_IC_DIGITS_NOA = 0x00;
	public static final byte OLNS_ORIG_IC_DIGITS_NUM_PLAN = 0x01;
	public static final byte OLNS_ORIG_IC_NUM_DIGITS = 0x04;
	public static final byte OLNS_ORIG_INC_DIGITS_TAG1 = (byte) 0xDF;
	public static final byte OLNS_ORIG_INC_DIGITS_TAG2 = (byte) 0x49;
	public static final byte OLNS_ORIG_INC_DIGITS_LENGTH = (byte) 0x06;
	public static final byte OLNS_ORIG_INC_DIGITS_TYPE = 0x19;
	public static final byte OLNS_ORIG_INC_DIGITS_NOA = 0x00;
	public static final byte OLNS_ORIG_INC_DIGITS_NUM_PLAN = 0x01;
	public static final byte OLNS_ORIG_INC_NUM_DIGITS = 0x04;
	public static final byte OLNS_FOREIGN_LANG_ID_TAG1 = (byte) 0xDF;
	public static final byte OLNS_FOREIGN_LANG_ID_TAG2 = 0x74;
	public static final byte OLNS_FOREIGN_LANG_ID_LENGTH = 0x01;
	public static final byte OLNS_GENERIC_NAME_ID = (byte) 0x97;
	public static final byte OLNS_ALPHANUMERIC_STRING_ID_TAG1 = (byte) 0xDF;
	public static final byte OLNS_ALPHANUMERIC_STRING_ID_TAG2 = (byte) 0x48;
	public static final byte OLNS_DISALWD_CARD_ISSUER_CODE_TAG1 = (byte) 0xDF;
	public static final byte OLNS_DISALWD_CARD_ISSUER_CODE_TAG2 = 0x49;
	public static final byte OLNS_DISALWD_CARD_ISSUER_CODE_LENGTH = 0x07;
	public static final byte OLNS_DISALWD_CARD_ISSUER_CODE_TYPE = 0x18;
	public static final byte OLNS_DISALWD_CARD_ISSUER_CODE_NOA = 0x00;
	public static final byte OLNS_DISALWD_CARD_ISSUER_CODE_ENCDNG_SCHEME = 0x01;
	public static final byte OLNS_DISALWD_CARD_ISSUER_CODE_NUM_DIGIT = 0x06;
	public static final byte OLNS_ORIG_LIST_SERV_IND_TAG1 = (byte) 0xDF;
	public static final byte OLNS_ORIG_LIST_SERV_IND_TAG2 = 0x7A;
	public static final byte OLNS_ORIG_LIST_SERV_IND_LENGTH = 0x01;
	public static final byte OLNS_ACC_OWNER_TAG1 = (byte) 0xDF;
	public static final byte OLNS_ACC_OWNER_TAG2 = (byte) 0xC1;
	public static final byte OLNS_ACC_OWNER_TAG3 = 0x02;
	public static final byte OLNS_ACC_OWNER_LENGTH = 0x04;
	public static final byte OLNS_BILLING_SERV_PROV_TAG1 = (byte) 0xDF;
	public static final byte OLNS_BILLING_SERV_PROV_TAG2 = (byte) 0xC1;
	public static final byte OLNS_BILLING_SERV_PROV_TAG3 = 0x03;
	public static final byte OLNS_BILLING_SERV_PROV_LENGTH = 0x04;
	public static final byte OLNS_WSOI_TAG1 = (byte) 0xDF;
	public static final byte OLNS_WSOI_TAG2 = (byte) 0xC1;
	public static final byte OLNS_WSOI_TAG3 = (byte) 0x26;
	public static final byte OLNS_WSOI_LENGTH = (byte) 0x01;

}
