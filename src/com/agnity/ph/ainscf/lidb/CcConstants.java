package com.agnity.ph.ainscf.lidb;

public class CcConstants {

	private CcConstants() {
	}
	public static final int CC_MANDATORY_LEN = 33;
	public static final byte INDICATING_SCREENED_DATA = 0;
	public static final byte INDICATING_STABLE_RECORD_BNA_AVAILABLE = 1;
	public static final byte INDICATING_STABLE_RECORD_BNA_NA = 2;
	public static final byte INDICATING_TRANSACTIONAL_RECORD_BNA_AVAILABLE = 3;
	public static final byte INDICATING_TRANACATIONAL_RECORD_BNA_NA = 4;
	public static final byte INDICATING_DEFAULT_RECORD_BNA_AVAILABLE = 5;
	public static final byte INDICATING_DEFAULT_RECORD_BNA_NA = 6;
	//public static final byte CC_CCSAN_LEN = 0x01;
	// mandatory fixed lengths
	public static final int CC_PARAM_SET_ID_LEN = 2;

	public static final int CC_COMPANY_ID_LEN = 5;

	public static final int CC_REC_STATUS_IND_LEN = 4;
	
	public static final int CC_PRI_LEN = 1;

	public static final int CC_O_DIGIT_IDENTIFIER_LEN = 9;

	public static final int CC_INTERCPT_IND_LEN = 4;

	public static final int CC_IC_INDICATOR_LEN = 5;

	public static final int CC_PRIM_PREF_IC_LEN = 9;

	public static final int CC_ALT_PREF_IC_LEN = 9;

	public static final int CC_PREF_INC_LEN = 9;

	public static final int CC_REF_DIFIT_IDENTIFIER_LEN = 7;

	public static final int CC_ACC_OWNER_LEN = 8;

	public static final int CC_BILLING_SER_PROV_LEN = 8;

	// fixed fields for CC Query response encoding
	public static final byte PARAMETER_SET_ID = (byte) 0xF2;
	public static final byte COMPANY_ID_IDENTIFIER_TAG1 = (byte) 0xDF;
	public static final byte COMPANY_ID_IDENTIFIER_TAG2 = 0x53;

	public static final byte COMPANY_ID_LENGTH = 0x02;

	public static final byte RECORD_STATUS_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte RECORD_STATUS_INDICATOR_ID_TAG2 = (byte) 0x67;

	public static final byte CCSAN_TAG1 = (byte) 0xDF;
	public static final byte CCSAN_TAG2 = (byte) 0x4D;
	public static final byte CCSAN_LENGTH = 0x01;
	
	public static final byte PIN_RESTRICTION_IND_TAG1 = (byte) 0xDF;
	public static final byte PIN_RESTRICTION_IND_TAG2 = (byte) 0x61;
	public static final byte PIN_RESTRICTION_IND_LENGTH = 0x01;
	
	public static final byte RECORD_STATUS_INDICATOR_ID_LENGTH = 0x01;

	public static final byte DIGITS_IDENTIFIER_TAG1 = (byte) 0xDF;
	public static final byte DIGITS_IDENTIFIER_TAG2 = 0x49;
	public static final byte DIGITS_IDENTIFIER_LENGTH = 0x06;

	public static final byte IC_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte IC_INDICATOR_ID_TAG2 = (byte) 0x57;
	public static final byte IC_INDICATOR_LENGTH = 0x02;
	
	public static final byte CSDI_TAG1 = (byte) 0xDF;
	public static final byte CSDI_TAG2 = 0x4C;
	public static final byte CC_CSDI_LEN = 0x01;

	public static final byte MATCH_TAG1 = (byte) 0xDF;
	public static final byte MATCH_TAG2 = 0x5D;
	public static final byte CC_MATCH_LEN = 0x01;

	public static final byte PSDI_TAG1 = (byte) 0xDF;
	public static final byte PSDI_TAG2 = 0x62;
	public static final byte CC_PSDI_LEN = 0x01;

	public static final byte PIN_TAG1 = (byte) 0xDF;
	public static final byte PIN_TAG2 = 0x60;
	public static final byte CC_PIN_LEN = 0x02;

	public static final int CC_TRUE_BILLING_NUM_LEN = 13;
	
	public static final byte DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_TAG1 = (byte) 0xDF;
	public static final byte DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_TAG2 = 0x49;
	public static final byte PRIMARY_PREFERRED_IC_DIGITS_IDENTIFIER_LENGTH = 0x06;
	public static final byte DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_NOA = (byte) 0x00;

	public static final byte DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_TAG1 = (byte) 0xDF;
	public static final byte DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_TAG2 = 0x49;
	public static final byte ALTERNATE_PREFERRED_IC_DIGITS_IDENTIFIER_LENGTH = 0x06;
	public static final byte DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_NOA = (byte) 0x00;

	public static final byte DIGITS_IDENTIFIER_ID_PREFERRED_INC_TAG1 = (byte) 0xDF;
	public static final byte DIGITS_IDENTIFIER_ID_PREFERRED_INC_TAG2 = 0x49;
	public static final byte PREFERRED_INC_DIGITS_IDENTIFIER_LENGTH = 0x06;
	public static final byte DIGITS_IDENTIFIER_ID_PREFERRED_INC_NOA = (byte) 0x00;

	public static final byte DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG1 = (byte) 0xDF;
	public static final byte DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG2 = 0x49;

	public static final byte ACCOUNT_OWNER_IDENTIFIER_TAG1 = (byte) 0xDF;
	public static final byte ACCOUNT_OWNER_IDENTIFIER_TAG2 = (byte) 0xC1;
	public static final byte ACCOUNT_OWNER_IDENTIFIER_TAG3 = 0x02;
	public static final byte ACCOUNT_OWNER_LENGTH = 0x04;

	public static final byte BILLING_SERVICE_PROVIDER_ID_TAG1 = (byte) 0xDF;
	public static final byte BILLING_SERVICE_PROVIDER_ID_TAG2 = (byte) 0xC1;
	public static final byte BILLING_SERVICE_PROVIDER_ID_TAG3 = 0x03;
	
	public static final byte DIGITS_IDENTIFIER_ID_TRUE_BILLING_NUMBER_TAG1 = (byte) 0xDF;
	public static final byte DIGITS_IDENTIFIER_ID_TRUE_BILLING_NUMBER_TAG2 = 0x49;
	public static final byte BILLING_SERVICE_PROVIDER_LENGTH = 0x04;

	// APPLICATION ERRROR
	public static final int TOTAL_FIXED_LEN_APP_ERR = 4;
	public static final int TOTAL_FIXED_LEN_PROTOCOL_ERR = 3;
}
