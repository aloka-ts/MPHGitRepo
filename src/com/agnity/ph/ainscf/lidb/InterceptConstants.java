package com.agnity.ph.ainscf.lidb;

public class InterceptConstants {
	private InterceptConstants() {
	}
	public static final int INTRCPT_MANDATORY_LEN = 14;
	public static final byte INDICATING_SCREENED_DATA = 0;
	public static final byte INDICATING_STABLE_RECORD_BNA_AVAILABLE = 1;
	public static final byte INDICATING_STABLE_RECORD_BNA_NA = 2;
	public static final byte INDICATING_TRANSACTIONAL_RECORD_BNA_AVAILABLE = 3;
	public static final byte INDICATING_TRANACATIONAL_RECORD_BNA_NA = 4;
	public static final byte INDICATING_DEFAULT_RECORD_BNA_AVAILABLE = 5;
	public static final byte INDICATING_DEFAULT_RECORD_BNA_NA = 6;

	// mandatory fixed lengths
	public static final int INTRCPT_PARAM_SET_ID_LEN = 2;

	public static final int INTRCPT_COMPANY_ID_LEN = 5;

	public static final int INTRCPT_REC_STATUS_IND_LEN = 4;

	public static final int INTRCPT_COL_TERM_LINE_IND_LEN = 4;

	public static final int INTRCPT_THRD_NUM_ACPT_IND_LEN = 4;

	public static final int INTRCPT_SER_EQP_IND_LEN = 4;

	public static final int INTRCPT_TRTMNT_IND_LEN = 4;

	public static final int INTRCPT_O_DIGIT_IDENTIFIER_LEN = 9;

	public static final int INTRCPT_INTERCPT_IND_LEN = 4;

	public static final int INTRCPT_IC_INDICATOR_LEN = 5;

	public static final int INTRCPT_PRIM_PREF_IC_LEN = 9;

	public static final int INTRCPT_ALT_PREF_IC_LEN = 9;

	public static final int INTRCPT_PREF_INC_LEN = 9;

	public static final int INTRCPT_REF_DIFIT_IDENTIFIER_LEN = 7;

	public static final int INTRCPT_ACC_OWNER_LEN = 8;

	public static final int INTRCPT_BILLING_SER_PROV_LEN = 8;

	// fixed fields for Intercept Query response encoding
	public static final byte PARAMETER_SET_ID = (byte) 0xF2;
	public static final byte COMPANY_ID_IDENTIFIER_TAG1 = (byte) 0xDF;
	public static final byte COMPANY_ID_IDENTIFIER_TAG2 = 0x53;

	public static final byte COMPANY_ID_LENGTH = 0x02;

	public static final byte RECORD_STATUS_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte RECORD_STATUS_INDICATOR_ID_TAG2 = (byte) 0x67;

	public static final byte RECORD_STATUS_INDICATOR_ID_LENGTH = 0x01;

	public static final byte INTERCEPT_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte INTERCEPT_INDICATOR_ID_TAG2 = (byte) 0x5A;
	public static final byte INTERCEPT_INDICATOR_LENGTH = 0x01;

	public static final byte DIGITS_IDENTIFIER_TAG1 = (byte) 0xDF;
	public static final byte DIGITS_IDENTIFIER_TAG2 = 0x49;
	public static final byte DIGITS_IDENTIFIER_LENGTH = 0x06;

	public static final byte DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG1 = (byte) 0xDF;
	public static final byte DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG2 = 0x49;

	// APPLICATION ERRROR
	public static final int TOTAL_FIXED_LEN_APP_ERR = 4;
	public static final int TOTAL_FIXED_LEN_PROTOCOL_ERR = 3;

}
