/*******************************************************************************
 *   Copyright (c) 2011 Agnity, Inc. All rights reserved.
 *   
 *   This is proprietary source code of Agnity, Inc. 
 *   
 *   Agnity, Inc. retains all intellectual property rights associated 
 *   with this source code. Use is subject to license terms.
 *   
 *   This source code contains trade secrets owned by Agnity, Inc.
 *   Confidentiality of this computer program must be maintained at 
 *   all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.ph.mapscf;

/**
 * This class is used to define Release reason codes for a MAP call
 *
 */
public interface MapScfRelReasonCode {

	int TRANSIENT_CALL_FT = 102;

	int EXCEP_IN_DIALOG_IND = 161;
	int EXCEP_IN_COMP_IND = 162;
	int EXCEP_IN_STATE_IND = 165;
	int EXCEP_IN_TCAP_SES_ACT = 166;
	int EXCEP_IN_TCAP_ERR_ACT = 167;
	int EXCEP_IN_TCAP_TIMEOUT = 168;

	int UNEXPECTED_TIMEOUT = 319;

	int UNEXPECTED_BEGIN = 322;
	int UNEXPECTED_CONTINUE = 323;
	int UNEXPECTED_END = 324;

	int END_RCVD_WITH_COMP = 470;
	int END_RCVD_WITHOUT_COMP = 471;
	int UABORT_RCVD = 472;
	int PABORT_NOTICE_RCVD = 473;
	int PRIM_RESULT_RCVD = 475;
	int PRIM_ERROR_RCVD = 476;
	int PRIM_REJECT_RCVD = 477;
	int PROCESS_TACP_ERR_RCVD = 480;

	// Unexpected actions code 501-550
	int UNEXP_ACTION = 501;

	// General Errors 551-600

	int UNKNOWN_DIALOG_RCVD = 556;
	int DIAGLOG_MANDATORY_PARAM_MIS = 557;
	int COMP_MANDATORY_PARAM_MIS = 558;
	int UNKNOWN_COMPIND_RCVD = 559;

	int SERVICE_TYPE_NOT_FOUND = 562;
	int FAILED_TO_INITIALIZE_SRV = 563;

	// Parsing Failures 621-660

	int UNKNOWN_RESULT = 635;

	int ERR_MSG_MAND_PARAM_MISSING = 651;
	int ERR_MSG_PARAM_NOT_SET = 652;
	int ERR_MSG_INVALID_INVOK_ID = 653;
	int REJECT_MSG_MAND_PARAM_MISSING = 654;
	int REJECT_MSG_PARAM_NOT_SET = 655;
	int REJECT_INVALID_INVOK_ID = 656;
	int REJECT_INVALID_PROBLEM_TYPE = 657;
	int UNEXPECTED_SRR = 658;
	int UNEXPECTED_ACR = 659;
	int UNEXPECTED_CONT = 660;
	int ERB_ASN_PARSING_FAIL = 661;
	int UNEXPECTED_NSDM = 668;

}
