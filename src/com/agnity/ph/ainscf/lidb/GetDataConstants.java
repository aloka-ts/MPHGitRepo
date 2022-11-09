package com.agnity.ph.ainscf.lidb;

public class GetDataConstants {

	private GetDataConstants() {
		
	}
	public static final byte GETDATA_PARAM_SET_ID = (byte) 0xF2;
	public static final byte GETDATA_LIDB_DATA_ELEMENT_BLOCK_TAG1 = (byte) 0xFF;
	public static final byte GETDATA_LIDB_DATA_ELEMENT_BLOCK_TAG2 = (byte) 0x7E;
	public static final byte GETDATA_STD_ELEMENT_SEQUENCE_TAG = (byte) 0x30;
	public static final byte GETDATA_LIDB_ELEMENT_ID_TAG = (byte) 0x02;
	
	//TAG value for each elements
	public static final byte GETDATA_ACCOUNT_OWNER_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ACCOUNT_OWNER_TAG2 = (byte) 0x82;
	public static final byte GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_TAG  =  (byte) 0x79;
	public static final byte GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_TAG2 =  (byte) 0x95;
	public static final byte GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_TAG2 =  (byte) 0x96;
	public static final byte GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_TAG2 =  (byte) 0x97;
	public static final byte GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_TAG2 =  (byte) 0x98;
	public static final byte GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG2 =  (byte) 0x99;
	public static final byte GETDATA_ALPHANUMERIC_STRING_TAG  =  (byte) 0x48;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_TAG2 =  (byte) 0x9F;
	public static final byte GETDATA_BILLING_SERVICE_PROVIDER_TAG1 = (byte) 0x00;
	public static final byte GETDATA_BILLING_SERVICE_PROVIDER_TAG2 =  (byte) 0x83;
	public static final byte GETDATA_SERVICE_DENIAL_INDICATOR_TAG  =  (byte) 0x4C;
	public static final byte GETDATA_COLLECT_ACCEPTANCE_INDICATOR_TAG  =  (byte) 0x52;
	public static final byte GETDATA_DIVERSION_ROUTING_NUMBER_TAG1 = (byte) 0x00;
	public static final byte GETDATA_DIVERSION_ROUTING_NUMBER_TAG2 =  (byte) 0xA8;
	public static final byte GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_TAG  =  (byte) 0x74;
	public static final byte GETDATA_GENERIC_NAME_TAG  =  (byte) 0x17;
	public static final byte GETDATA_IC_INDICATORS_TAG  =  (byte) 0x57;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_TAG2 =  (byte) 0x84;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_TAG2 =  (byte) 0x85;
	public static final byte GETDATA_PREFERRED_INC_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_PREFERRED_INC_INDICATOR_TAG2 =  (byte) 0x86;
	public static final byte GETDATA_ILP_CIC_INDICATOR_TAG  =  (byte) 0x78;
	public static final byte GETDATA_ILP_CIC_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ILP_CIC_TAG2 =  (byte) 0x94;
	public static final byte GETDATA_INTERCEPT_INDICATOR_TAG  =  (byte) 0x5A;
	public static final byte GETDATA_ORIG_BILLING_SERVICE_INDICATORS_TAG  =  (byte) 0x5E;
	public static final byte GETDATA_ORIG_COLLECT_BILLING_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_COLLECT_BILLING_INDICATOR_TAG2 =  (byte) 0x8B;
	public static final byte GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_TAG2 =  (byte) 0x8C;
	public static final byte GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_TAG2 =  (byte) 0x8D;
	public static final byte GETDATA_ORIG_CREDIT_CARD_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_CREDIT_CARD_INDICATOR_TAG2 =  (byte) 0x8E;
	public static final byte GETDATA_ORIG_FREE_DA_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_FREE_DA_INDICATOR_TAG2 =  (byte) 0x8F;
	public static final byte GETDATA_ORIG_SPECIAL_BNS_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_SPECIAL_BNS_INDICATOR_TAG2 =  (byte) 0x90;
	public static final byte GETDATA_ORIG_SENTPAID_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_SENTPAID_INDICATOR_TAG2 =  (byte) 0x91;
	public static final byte GETDATA_ORIG_DACC_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_DACC_INDICATOR_TAG2 =  (byte) 0x92;
	public static final byte GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG2 =  (byte) 0x93;
	public static final byte GETDATA_ORIG_IC_INDICATORS_TAG  =  (byte) 0x4B;
	public static final byte GETDATA_ORIG_IC_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_IC_INDICATOR_TAG2 =  (byte) 0x87;
	public static final byte GETDATA_ORIG_INC_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_INC_INDICATOR_TAG2 =  (byte) 0x88;
	public static final byte GETDATA_ORIG_IC_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_IC_TAG2 =  (byte) 0x89;
	public static final byte GETDATA_ORIG_INC_TAG1 = (byte) 0x00;
	public static final byte GETDATA_ORIG_INC_TAG2 =  (byte) 0x8A;
	public static final byte GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_TAG  =  (byte) 0x7A;
	public static final byte GETDATA_PREFERRED_INC_TAG1 = (byte) 0x00;
	public static final byte GETDATA_PREFERRED_INC_TAG2 =  (byte) 0xA0;
	public static final byte GETDATA_PREFERRED_CODE_LIST_TAG1 = (byte) 0x00;
	public static final byte GETDATA_PREFERRED_CODE_LIST_TAG2 =  (byte) 0xA9;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_TAG1 = (byte) 0x00;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_TAG2 =  (byte) 0x9E;
	public static final byte GETDATA_RAO_TAG1 = (byte) 0x00;
	public static final byte GETDATA_RAO_TAG2 =  (byte) 0x9A;
	public static final byte GETDATA_RECORD_STATUS_INDICATOR_TAG  =  (byte) 0x67;
	public static final byte GETDATA_REFERRAL_NUMBER_TAG1 = (byte) 0x00;
	public static final byte GETDATA_REFERRAL_NUMBER_TAG2 =  (byte) 0xA1;
	public static final byte GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_TAG  =  (byte) 0x68;
	public static final byte GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_TAG  =  (byte) 0x6A;
	public static final byte GETDATA_TREATMENT_INDICATOR_TAG  =  (byte) 0x6C;
	public static final byte GETDATA_TRUE_BILLING_NUMBER_TAG  =  (byte) 0x64;
	public static final byte GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_TAG2 =  (byte) 0xA6;
	public static final byte GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_TAG1 = (byte) 0x00;
	public static final byte GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_TAG2 =  (byte) 0xA7;

	// SEQUENCE Length for each elements
	public static final byte GETDATA_ACCOUNT_OWNER_SEQ_LEN = (byte) 0x0A;
	public static final byte GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_SEQ_LEN = 0x07;
	public static final byte GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_SEQ_LEN = 0x07;
	//public static final byte GETDATA_ALPHANUMERIC_STRING_SEQ_LEN = Variable length;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_SEQ_LEN = (byte) 0x0A;
	public static final byte GETDATA_BILLING_SERVICE_PROVIDER_SEQ_LEN = (byte) 0x0A;
	public static final byte GETDATA_SERVICE_DENIAL_INDICATOR_SEQ_LEN = 0x06;
	public static final byte GETDATA_COLLECT_ACCEPTANCE_INDICATOR_SEQ_LEN = 0x06;
	//public static final byte GETDATA_DIVERSION_ROUTING_NUMBER_SEQ_LEN = Variable length;
	public static final byte GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_SEQ_LEN = 0x06;
	//public static final byte GETDATA_GENERIC_NAME_SEQ_LEN = Variable length;
	public static final byte GETDATA_IC_INDICATORS_SEQ_LEN = 0x07;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_PREFERRED_INC_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ILP_CIC_INDICATOR_SEQ_LEN = 0x06;
	public static final byte GETDATA_ILP_CIC_SEQ_LEN = 0x0A;
	public static final byte GETDATA_INTERCEPT_INDICATOR_SEQ_LEN = 0x06;
	public static final byte GETDATA_ORIG_BILLING_SERVICE_INDICATORS_SEQ_LEN = 0x08;
	public static final byte GETDATA_ORIG_COLLECT_BILLING_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_CREDIT_CARD_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_FREE_DA_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_SPECIAL_BNS_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_SENTPAID_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_DACC_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_IC_INDICATORS_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_IC_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_INC_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_ORIG_IC_SEQ_LEN = 0x0A;
	public static final byte GETDATA_ORIG_INC_SEQ_LEN = 0x0A;
	public static final byte GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_SEQ_LEN = 0x06;
	public static final byte GETDATA_PREFERRED_INC_SEQ_LEN = 0x0A;
	//public static final byte GETDATA_PREFERRED_CODE_LIST_SEQ_LEN = Variable length;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_SEQ_LEN = 0x0A;
	public static final byte GETDATA_RAO_SEQ_LEN = 0x09;
	public static final byte GETDATA_RECORD_STATUS_INDICATOR_SEQ_LEN = 0x06;
	//public static final byte GETDATA_REFERRAL_NUMBER_SEQ_LEN = Variable length;
	public static final byte GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_SEQ_LEN = 0x06;
	public static final byte GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_SEQ_LEN = 0x06;
	public static final byte GETDATA_TREATMENT_INDICATOR_SEQ_LEN = 0x06;
	//public static final byte GETDATA_TRUE_BILLING_NUMBER_SEQ_LEN = Variable length;
	public static final byte GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_SEQ_LEN = 0x07;
	public static final byte GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_SEQ_LEN = 0x07;
	// LIDBElementIdentifier Length for each elements
	public static final byte GETDATA_ACCOUNT_OWNER_TAG_LEN = 0x02;
	public static final byte GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_TAG_LEN = 0x01;
	public static final byte GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ALPHANUMERIC_STRING_TAG_LEN = 0x01;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_TAG_LEN = 0x02;
	public static final byte GETDATA_BILLING_SERVICE_PROVIDER_TAG_LEN = 0x02;
	public static final byte GETDATA_SERVICE_DENIAL_INDICATOR_TAG_LEN = 0x01;
	public static final byte GETDATA_COLLECT_ACCEPTANCE_INDICATOR_TAG_LEN = 0x01;
	public static final byte GETDATA_DIVERSION_ROUTING_NUMBER_TAG_LEN = 0x02;
	public static final byte GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_TAG_LEN = 0x01;
	public static final byte GETDATA_GENERIC_NAME_TAG_LEN = 0x01;
	public static final byte GETDATA_IC_INDICATORS_TAG_LEN = 0x01;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_PREFERRED_INC_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ILP_CIC_INDICATOR_TAG_LEN = 0x01;
	public static final byte GETDATA_ILP_CIC_TAG_LEN = 0x02;
	public static final byte GETDATA_INTERCEPT_INDICATOR_TAG_LEN = 0x01;
	public static final byte GETDATA_ORIG_BILLING_SERVICE_INDICATORS_TAG_LEN = 0x01;
	public static final byte GETDATA_ORIG_COLLECT_BILLING_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_CREDIT_CARD_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_FREE_DA_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_SPECIAL_BNS_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_SENTPAID_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_DACC_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_IC_INDICATORS_TAG_LEN = 0x01;
	public static final byte GETDATA_ORIG_IC_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_INC_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_IC_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_INC_TAG_LEN = 0x02;
	public static final byte GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_TAG_LEN = 0x01;
	public static final byte GETDATA_PREFERRED_INC_TAG_LEN = 0x02;
	public static final byte GETDATA_PREFERRED_CODE_LIST_TAG_LEN = 0x02;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_TAG_LEN = 0x02;
	public static final byte GETDATA_RAO_TAG_LEN = 0x02;
	public static final byte GETDATA_RECORD_STATUS_INDICATOR_TAG_LEN = 0x01;
	public static final byte GETDATA_REFERRAL_NUMBER_TAG_LEN = 0x02;
	public static final byte GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_TAG_LEN = 0x01;
	public static final byte GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_TAG_LEN = 0x01;
	public static final byte GETDATA_TREATMENT_INDICATOR_TAG_LEN = 0x01;
	public static final byte GETDATA_TRUE_BILLING_NUMBER_TAG_LEN = 0x01;
	public static final byte GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_TAG_LEN = 0x02;
	public static final byte GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_TAG_LEN = 0x02;
	
	// LIDBElementData Length for each elements
	public static final byte GETDATA_ACCOUNT_OWNER_LENGTH = 0x04;
	public static final byte GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_LENGTH = 0x02;
	public static final byte GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_LENGTH = 0x01;
	//public static final byte GETDATA_ALPHANUMERIC_STRING_LENGTH = up to 40;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_LENGTH = 0x04;
	public static final byte GETDATA_BILLING_SERVICE_PROVIDER_LENGTH = 0x04;
	public static final byte GETDATA_SERVICE_DENIAL_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_COLLECT_ACCEPTANCE_INDICATOR_LENGTH = 0x01;
	//public static final byte GETDATA_DIVERSION_ROUTING_NUMBER_LENGTH = 9*** variable;
	public static final byte GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_LENGTH = 0x01;
	//public static final byte GETDATA_GENERIC_NAME_LENGTH = up to 16;
	public static final byte GETDATA_IC_INDICATORS_LENGTH = 0x02;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_PREFERRED_INC_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ILP_CIC_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ILP_CIC_LENGTH = 0x04;
	public static final byte GETDATA_INTERCEPT_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_BILLING_SERVICE_INDICATORS_LENGTH = 0x03;
	public static final byte GETDATA_ORIG_COLLECT_BILLING_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_CREDIT_CARD_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_FREE_DA_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_SPECIAL_BNS_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_SENTPAID_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_DACC_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_IC_INDICATORS_LENGTH = 0x02;
	public static final byte GETDATA_ORIG_IC_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_INC_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_ORIG_IC_LENGTH = 0x04;
	public static final byte GETDATA_ORIG_INC_LENGTH = 0x04;
	public static final byte GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_LENGTH = 0x01;
	public static final byte GETDATA_PREFERRED_INC_LENGTH = 0x04;
	//public static final byte GETDATA_PREFERRED_CODE_LIST_LENGTH = Variabl;
	public static final byte GETDATA_PRIMARY_PREFERRED_IC_LENGTH = 0x04;
	public static final byte GETDATA_RAO_LENGTH = 0x03;
	public static final byte GETDATA_RECORD_STATUS_INDICATOR_LENGTH = 0x01;
	//public static final byte GETDATA_REFERRAL_NUMBER_LENGTH = 9 variable;
	public static final byte GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_TREATMENT_INDICATOR_LENGTH = 0x01;
	//public static final byte GETDATA_TRUE_BILLING_NUMBER_LENGTH = variable;
	public static final byte GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_LENGTH = 0x01;
	public static final byte GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_LENGTH = 0x01;
	

	//Total length for each elements sequence including sequence tag and sequence length
	public static final int GETDATA_ACCOUNT_OWNER_LEN_OCTET = 12;
	public static final int GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_LEN_OCTET = 9;
	public static final int GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_LEN_OCTET = 9;
	//public static final int GETDATA_ALPHANUMERIC_STRING_LEN_OCTET = variable;
	public static final int GETDATA_ALTERNATE_PREFERRED_IC_LEN_OCTET = 12;
	public static final int GETDATA_BILLING_SERVICE_PROVIDER_LEN_OCTET = 12;
	public static final int GETDATA_SERVICE_DENIAL_INDICATOR_LEN_OCTET = 8;
	public static final int GETDATA_COLLECT_ACCEPTANCE_INDICATOR_LEN_OCTET = 8;
	//public static final int GETDATA_DIVERSION_ROUTING_NUMBER_LEN_OCTET = variable;
	public static final int GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_LEN_OCTET = 8;
	//public static final int GETDATA_GENERIC_NAME_LEN_OCTET = variable;
	public static final int GETDATA_IC_INDICATORS_LEN_OCTET = 9;
	public static final int GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_PREFERRED_INC_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ILP_CIC_INDICATOR_LEN_OCTET = 8;
	public static final int GETDATA_ILP_CIC_LEN_OCTET = 12;
	public static final int GETDATA_INTERCEPT_INDICATOR_LEN_OCTET = 8;
	public static final int GETDATA_ORIG_BILLING_SERVICE_INDICATORS_LEN_OCTET = 10;
	public static final int GETDATA_ORIG_COLLECT_BILLING_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_CREDIT_CARD_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_FREE_DA_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_SPECIAL_BNS_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_SENTPAID_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_DACC_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_IC_INDICATORS_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_IC_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_INC_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_ORIG_IC_LEN_OCTET = 12;
	public static final int GETDATA_ORIG_INC_LEN_OCTET = 12;
	public static final int GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_LEN_OCTET = 8;
	public static final int GETDATA_PREFERRED_INC_LEN_OCTET = 12;
	//public static final int GETDATA_PREFERRED_CODE_LIST_LEN_OCTET = variable;
	public static final int GETDATA_PRIMARY_PREFERRED_IC_LEN_OCTET = 12;
	public static final int GETDATA_RAO_LEN_OCTET = 10;
	public static final int GETDATA_RECORD_STATUS_INDICATOR_LEN_OCTET = 8;
	//public static final int GETDATA_REFERRAL_NUMBER_LEN_OCTET = variable;
	public static final int GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_LEN_OCTET = 8;
	public static final int GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_LEN_OCTET = 8;
	public static final int GETDATA_TREATMENT_INDICATOR_LEN_OCTET = 8;
	//public static final int GETDATA_TRUE_BILLING_NUMBER_LEN_OCTET = variable;
	public static final int GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_LEN_OCTET = 9;
	public static final int GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_LEN_OCTET = 9;
	
	// Element Data Tag for different encoding types
	public static final byte GETDATA_ELEMENT_DATA_TAG_IA5String = 0x16;
	public static final byte GETDATA_ELEMENT_DATA_TAG_OCTET  = 0x04;
	public static final byte GETDATA_ELEMENT_DATA_TAG_INTEGER = 0x02;
	public static final byte GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_1 = (byte) 0xDF;
	public static final byte GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_2 = 0x7F;
	public static final byte GETDATA_ELEMENT_DATA_TAG_LIDB_DIGITS_1 = (byte) 0xDF;
	public static final byte GETDATA_ELEMENT_DATA_TAG_LIDB_DIGITS_2 = 0x49;
	//Others
	public static final byte GETDATA_ORIG_IC_DIGITS_TYPE = 0x19;
	public static final byte GETDATA_ORIG_IC_DIGITS_NOA = 0x00;
	public static final byte GETDATA_ORIG_IC_DIGITS_NUM_PLAN = 0x01;
	public static final byte GETDATA_ORIG_INC_DIGITS_TYPE = 0x19;
	public static final byte GETDATA_ORIG_INC_DIGITS_NOA = 0x00;
	public static final byte GETDATA_ORIG_INC_DIGITS_NUM_PLAN = 0x01;
	public static final byte GETDATA_LIDB_ELEMENT_DATA_BCD_DIG = 0x03;
	
	public static final byte TRUE_BILLING_NUMBER_TYPE_OF_DIGITS = (byte) 0x0A;
	public static final byte REFERRAL_NUMBER_TYPE_OF_DIGITS = (byte) 0x09;
	public static final byte DIVERSION_ROUTING_NUMBER_TYPE_OF_DIGITS = (byte) 0x1B;
	
	//Error scenarios
	public static final byte GETDATA_LIDB_ELEMENT_ERROR_TAG = (byte) 0x98;
	public static final byte GETDATA_LEE_DATAUNAVAILABLE_LEN = 0x01;
	public static final byte GETDATA_LEE_DATAUNAVAILABLE = 0X06;
	public static final byte GETDATA_LEE_SCREENEDRESPONSE_LEN = 0x02;
	public static final byte GETDATA_LEE_SCREENEDRESPONSE_1 = 0x00;
	public static final byte GETDATA_LEE_SCREENEDRESPONSE_2 = (byte) 0xFA;
	public static final byte GETDATA_LEE_INVALIDTCAPID_LEN = 0x02;
	public static final byte GETDATA_LEE_INVALIDTCAPID_1 = 0x00;
	public static final byte GETDATA_LEE_INVALIDTCAPID_2 = (byte) 0xF5;
	public static final byte GETDATA_LEE_INTERNALPROCESSINGERROR_LEN = 0x02;
	public static final byte GETDATA_LEE_INTERNALPROCESSINGERROR_1 = 0x00;
	public static final byte GETDATA_LEE_INTERNALPROCESSINGERROR_2 = (byte) 0xF9;
	
	//Length of element Sequence excluding the ElmentData Error value (it can be 1 or 2 depends on error type)
	public static final int GETDATA_ACCOUNT_OWNER_ERR_LEN = 8;
	public static final int GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_ERR_LEN = 7;
	public static final int GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ALPHANUMERIC_STRING_ERR_LEN = 7;
	public static final int GETDATA_ALTERNATE_PREFERRED_IC_ERR_LEN = 8;
	public static final int GETDATA_BILLING_SERVICE_PROVIDER_ERR_LEN = 8;
	public static final int GETDATA_SERVICE_DENIAL_INDICATOR_ERR_LEN = 7;
	public static final int GETDATA_COLLECT_ACCEPTANCE_INDICATOR_ERR_LEN = 7;
	public static final int GETDATA_DIVERSION_ROUTING_NUMBER_ERR_LEN = 8;
	public static final int GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_ERR_LEN = 7;
	public static final int GETDATA_GENERIC_NAME_ERR_LEN = 7;
	public static final int GETDATA_IC_INDICATORS_ERR_LEN = 7;
	public static final int GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_PREFERRED_INC_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ILP_CIC_INDICATOR_ERR_LEN = 7;
	public static final int GETDATA_ILP_CIC_ERR_LEN = 8;
	public static final int GETDATA_INTERCEPT_INDICATOR_ERR_LEN = 7;
	public static final int GETDATA_ORIG_BILLING_SERVICE_INDICATORS_ERR_LEN = 7;
	public static final int GETDATA_ORIG_COLLECT_BILLING_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_CREDIT_CARD_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_FREE_DA_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_SPECIAL_BNS_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_SENTPAID_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_DACC_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_IC_INDICATORS_ERR_LEN = 7;
	public static final int GETDATA_ORIG_IC_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_INC_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_ORIG_IC_ERR_LEN = 8;
	public static final int GETDATA_ORIG_INC_ERR_LEN = 8;
	public static final int GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_ERR_LEN = 7;
	public static final int GETDATA_PREFERRED_INC_ERR_LEN = 8;
	public static final int GETDATA_PREFERRED_CODE_LIST_ERR_LEN = 8;
	public static final int GETDATA_PRIMARY_PREFERRED_IC_ERR_LEN = 8;
	public static final int GETDATA_RAO_ERR_LEN = 8;
	public static final int GETDATA_RECORD_STATUS_INDICATOR_ERR_LEN = 7;
	public static final int GETDATA_REFERRAL_NUMBER_ERR_LEN = 8;
	public static final int GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_ERR_LEN = 7;
	public static final int GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_ERR_LEN = 7;
	public static final int GETDATA_TREATMENT_INDICATOR_ERR_LEN = 7;
	public static final int GETDATA_TRUE_BILLING_NUMBER_ERR_LEN = 7;
	public static final int GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_ERR_LEN = 8;
	public static final int GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_ERR_LEN = 8;
	public static final byte GETDATA_INVALID_TCAPID_ERROR_VALUE1 = (byte) 0x00;
	public static final byte GETDATA_INVALID_TCAPID_ERROR_VALUE2 = (byte) 0xF5;
	
}