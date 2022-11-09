
/*******************************************************************************
 *   Copyright (c) 2014 Agnity, Inc. All rights reserved.
 *   
 *   This is proprietary source code of Agnity, Inc. 
 *   
 *   Agnity, Inc. retains all intellectual property rights associated 
 *   with this source code. Use is subject to license terms.
 *   
 *   This source code contains trade secrets owned by Agnity, Inc.
 *   Confidentiality of this computer program must be maintained at 
 *   all times, unless explicitly authorized by Agnity; Inc.
 *******************************************************************************/
package com.agnity.ph.ainscf.lidb;

import static com.agnity.ph.common.enums.PersistanceType.PERSISTABLE;

/**
 * @author vikas singh
 * @implNote This class is used at time of encoding BNS Query
 * 
 */
public final class BnsConstants {

	private BnsConstants() {
	}

	public static final int BNS_MANDATORY_LEN = 28;
	public static final byte INDICATING_SCREENED_DATA = 0;
	public static final byte INDICATING_STABLE_RECORD_BNA_AVAILABLE = 1;
	public static final byte INDICATING_STABLE_RECORD_BNA_NA = 2;
	public static final byte INDICATING_TRANSACTIONAL_RECORD_BNA_AVAILABLE = 3;
	public static final byte INDICATING_TRANACATIONAL_RECORD_BNA_NA = 4;
	public static final byte INDICATING_DEFAULT_RECORD_BNA_AVAILABLE = 5;
	public static final byte INDICATING_DEFAULT_RECORD_BNA_NA = 6;

	// mandatory fixed lengths
	public static final int BNS_PARAM_SET_ID_LEN = 2;

	public static final int BNS_COMPANY_ID_LEN = 5;

	public static final int BNS_REC_STATUS_IND_LEN = 4;

	public static final int BNS_COL_ACPT_IND_LEN = 4;

	public static final int BNS_THRD_NUM_ACPT_IND_LEN = 4;

	public static final int BNS_SER_EQP_IND_LEN = 4;

	public static final int BNS_TRTMNT_IND_LEN = 4;

	public static final int BNS_O_DIGIT_IDENTIFIER_LEN = 9;

	public static final int BNS_INTERCPT_IND_LEN = 4;

	public static final int BNS_IC_INDICATOR_LEN = 5;

	public static final int BNS_PRIM_PREF_IC_LEN = 9;

	public static final int BNS_ALT_PREF_IC_LEN = 9;

	public static final int BNS_PREF_INC_LEN = 9;

	public static final int BNS_REF_DIFIT_IDENTIFIER_LEN = 7;

	public static final int BNS_ACC_OWNER_LEN = 8;

	public static final int BNS_BILLING_SER_PROV_LEN = 8;

	// fixed fields for BNS Query response encoding
	public static final byte PARAMETER_SET_ID = (byte) 0xF2;
	public static final byte COMPANY_ID_IDENTIFIER_TAG1 = (byte) 0xDF;
	public static final byte COMPANY_ID_IDENTIFIER_TAG2 = 0x53;

	public static final byte COMPANY_ID_LENGTH = 0x02;

	public static final byte RECORD_STATUS_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte RECORD_STATUS_INDICATOR_ID_TAG2 = (byte) 0x67;

	public static final byte RECORD_STATUS_INDICATOR_ID_LENGTH = 0x01;

	public static final byte COLLECT_ACCEPTANCE_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte COLLECT_ACCEPTANCE_INDICATOR_ID_TAG2 = 0x52;
	public static final byte COLLECT_ACCEPTANCE_INDICATOR_LENGTH = 0x01;

	public static final byte THIRD_NUMBER_ACCEPTANCE_INDCIATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte THIRD_NUMBER_ACCEPTANCE_INDCIATOR_ID_TAG2 = 0x6A;

	public static final byte THIRD_NUMBER_ACCEPTANCE_INDCIATOR_ID_LENGTH = 0x01;

	public static final byte TREATMENT_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte TREATMENT_INDICATOR_ID_TAG2 = 0x6C;

	public static final byte TREATMENT_INDICATOR_LENGTH = 0x01;

	public static final byte SERVICE_OR_EQUIPMENT_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte SERVICE_OR_EQUIPMENT_INDICATOR_ID_TAG2 = 0x68;
	public static final byte SERVICE_OR_EQUIPMENT_INDICATOR_LENGTH = 0x01;

	public static final byte INTERCEPT_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte INTERCEPT_INDICATOR_ID_TAG2 = (byte) 0x5A;
	public static final byte INTERCEPT_INDICATOR_LENGTH = 0x01;

	public static final byte DIGITS_IDENTIFIER_TAG1 = (byte) 0xDF;
	public static final byte DIGITS_IDENTIFIER_TAG2 = 0x49;
	public static final byte DIGITS_IDENTIFIER_LENGTH = 0x06;

	public static final byte IC_INDICATOR_ID_TAG1 = (byte) 0xDF;
	public static final byte IC_INDICATOR_ID_TAG2 = (byte) 0x57;
	public static final byte IC_INDICATOR_LENGTH = 0x02;

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

	public static final byte BILLING_SERVICE_PROVIDER_LENGTH = 0x04;

	// APPLICATION ERRROR
	public static final int TOTAL_FIXED_LEN_APP_ERR = 4;
	public static final int TOTAL_FIXED_LEN_PROTOCOL_ERR = 3;

}
