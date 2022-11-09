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

package com.agnity.ph.capv2scf;

/**
 * This class contains the Release reson codes that can be used by application/and or Ph 
 *
 */

public interface Capv2ScfRelReasonCode {

		int	RSNRA_RECEIVED					= 101;
		int	TRANSIENT_CALL_FT				= 102;
		int	CALL_REDIRECTED					= 103;
		int	CAP_CALL_CONNECTED				= 104;
		int	INVALID_CONNMODE_TERM_IVR		= 105;
		int	INVALID_CONNMODE_ORIG_IVR		= 106;
		int	INVALID_CONNMODE_TERM			= 107;

		//Failures 111-150
		int	TERM_IVR_CONNECT_FAILED			= 111;
		int	ORIG_IVR_CONNECT_FAILED			= 112;
		int	TERM_IVR_PLAYCOL_FAILED			= 113;
		int	ORIG_IVR_PLAYCOL_FAILED			= 114;
		int	TERM_IVR_PLAY_FAILED			= 115;
		int	ORIG_IVR_PLAY_FAILED			= 116;
		int	TERM_IVR_PLAYREC_FAILED			= 117;
		int	ORIG_IVR_PLAYREC_FAILED			= 118;
		int	ORIG_CONNECTION_FAILED			= 119;
		int	LS_CMD_EXEC_FAILED				= 120;

		//Exception 151-300
		int	EXCEP_IN_INVITE_HANDLING		= 151;
		int	EXCEP_IN_CANCEL_HANDLING		= 152;
		int	EXCEP_IN_BYE_HANDLING			= 153;
		int	EXCEP_IN_ERR_HANDLING			= 154;
		int	EXCEP_IN_PROV_HANDLING			= 155;
		int	EXCEP_IN_SUCC_HANDLING			= 156;
		int	EXCEP_IN_PRAK_HANDLING			= 157;
		int	EXCEP_IN_ACK_HANDLING			= 158;
		int	EXCEP_IN_INFO_HANDLING			= 159;
		int	EXCEP_IN_UPDATE_HANDLING		= 160;

		int	EXCEP_IN_DIALOG_IND				= 161;
		int	EXCEP_IN_COMP_IND				= 162;
		int	EXCEP_IN_RSN_IND				= 163;
		int	EXCEP_IN_ERBCSM_IND				= 164;
		int	EXCEP_IN_STATE_IND				= 165;
		int	EXCEP_IN_TCAP_SES_ACT			= 166;
		int	EXCEP_IN_TCAP_ERR_ACT			= 167;
		int	EXCEP_IN_TCAP_TIMEOUT			= 168;
		int	EXCEP_IN_TIMEOUT_CLBK			= 169;
		int	EXCEP_IN_HANDLE_MSGCLBK			= 170;

		int	EXCEP_ERB_PARAM_OUTOFRANGE		= 171;
		int	EXCEP_ERB_PARSING				= 172;
		int	EXCEP_IN_HANDLE_RESEVENT		= 173;
		int	EXCEP_IN_PROCESS_TIMEOUT		= 174;
		int	EXCEP_IN_HANDLE_MSEVENT_TERM	= 175;
		int	EXCEP_IN_HANDLE_MSEVENT_ORIG	= 176;
		int	EXCEP_IN_ASR_HTTP_GET			= 177;
		int	EXCEP_IN_ANN_COPY				= 178;

		int	EXCEP_DISCON_TERM_IVR			= 181;
		int	EXCEP_DISCON_ORIG_IVR			= 182;
		int	EXCEP_SEND_HTTP_REQ				= 183;
		int	EXCEP_PLAYCOLLECT_TERM			= 184;
		int	EXCEP_PLAYCOLLECT_ORIG			= 185;
		int	EXCEP_PLAY_TERM					= 186;
		int	EXCEP_PLAY_ORIG					= 187;
		int	EXCEP_PLAYREC_ORIG				= 188;
		int	EXCEP_EXEC_LS_CMD				= 189;

		//Unexpected messages 301-400 
		int	UNEXPECTED_INVITE_MSG_ORIG		= 301;
		int	UNEXPECTED_INVITE_MSG_TERM		= 302;
		int	UNEXPECTED_CANCEL_MSG_ORIG		= 303;
		int	UNEXPECTED_CANCEL_MSG_TERM		= 304;
		int	UNEXPECTED_BYE_MSG_ORIG			= 305;
		int	UNEXPECTED_BYE_MSG_TERM			= 306;
		int	UNEXPECTED_ERR_MSG_ORIG			= 307;
		int	UNEXPECTED_ERR_MSG_TERM			= 308;
		int	UNEXPECTED_PROV_MSG_ORIG		= 309;
		int	UNEXPECTED_PROV_MSG_TERM		= 310;

		int	UNEXPECTED_SUCC_MSG_ORIG		= 311;
		int	UNEXPECTED_SUCC_MSG_TERM		= 312;
		int	UNEXPECTED_PRACK_MSG_ORIG		= 313;
		int	UNEXPECTED_PRACK_MSG_TERM		= 314;
		int	UNEXPECTED_ACK_MSG_ORIG			= 315;
		int	UNEXPECTED_ACK_MSG_TERM			= 316;
		int	UNEXPECTED_INFO_MSG_ORIG		= 317;
		int	UNEXPECTED_INFO_MSG_TERM		= 318;
		int	UNEXPECTED_TIMEOUT				= 319;
		int	UNEXPECTED_MSSBB_EVENT_ORIG		= 320;

		int	UNEXPECTED_MSSBB_EVENT_TERM		= 321;
		int	UNEXPECTED_BEGIN				= 322;
		int	UNEXPECTED_CONTINUE				= 323;
		int	UNEXPECTED_END					= 324;
		int	UNEXPECTED_IDP					= 325;
		int	UNEXPECTED_ENC					= 326;
		int	UNEXPECTED_ERB					= 327;

		int	UNEXP_ERB_ORIG_AUTH				= 331;
		int	UNEXP_ERB_ANALIZE_INFO			= 332;
		int	UNEXP_ERB_BUSY					= 333;
		int	UNEXP_ERB_NOANS					= 334;
		int	UNEXP_ERB_OANSWER				= 335;
		int	UNEXP_ERB_ODISC					= 336;
		int	UNEXP_ERB_OABANDON				= 337;

		//Timeout related 401-450
		int	PRACK_TIMED_OUT					= 401;
		int	ACK_TIMED_OUT					= 402;
		int	CORRELATION_TIMEOUT				= 403;
		int	ACT_TEST_TIMEOUT				= 404;
		int	ACCESS_GATEWAY_TIMEOUT			= 405;
		int	NO_ANSWER_TIMEOUT				= 406;

		//Cleanup messages 451-500
		int	CANCEL_RCVD_FROM_ORIG			= 451;
		int	BYE_RCVD_FROM_ORIG				= 452;
		int	BYE_RCVD_FROM_TERM				= 453;
		int	BUSY_RCVD_FROM_TERM				= 454;
		int	NOANS_RCVD_FROM_TERM			= 455;
		int	OTH_ERR_RCVD_FROM_TERM			= 456;
		int	ERR_RCVD_FOR_ORIG_REINV			= 457;
		int	ERR_RCVD_FOR_TERM_REINV			= 458;
		int	BYE_RCVD_FROM_TERM_IVR			= 459;
		int	BYE_RCVD_FROM_ORIG_IVR			= 460;

		int	ERR_RCVD_ON_TERM_IVR			= 461;
		int	ERR_RCVD_ON_ORIG_IVR			= 462;
		int	ERR_RCVD_FOR_TERM_PRACK			= 463;
		int	ERR_RCVD_FOR_ORIG_BYE			= 464;
		int	ERR_RCVD_FOR_TERM_BYE			= 465;

		int END_RCVD_WITH_COMP				= 470;
		int	END_RCVD_WITHOUT_COMP			= 471;
		int	UABORT_RCVD						= 472;
		int	PABORT_NOTICE_RCVD				= 473;
		int	ENTITY_RELEASE_RCVD				= 474;
		int	PRIM_RESULT_RCVD				= 475;
		int	PRIM_ERROR_RCVD					= 476;
		int	PRIM_REJECT_RCVD				= 477;
		int	ERB_ODISC_RCVD					= 478;
		int	ERB_OABANDON_RCVD				= 479;
		int	PROCESS_TACP_ERR_RCVD			= 480;
		
		//Unexpected actions code 501-550
		int	UNEXP_ACTION					= 501;
		int	UNEXP_ACT_RESYNC				= 502;
		int	UNEXP_ACT_PLAYCOL_ORIG			= 503;
		int	UNEXP_ACT_PLAYCOL_TERM			= 504;
		int	UNEXP_ACT_PLAY_TERM				= 505;
		int	UNEXP_ACT_PLAY_ORIG				= 506;
		int	UNEXP_ACT_PLAYREC_ORIG			= 507;
		int	UNEXP_ACT_CONIVR_TERM			= 508;
		int	UNEXP_ACT_CONIVR_ORIG			= 509;
		int	UNEXP_ACT_CONNECT_TERM			= 510;

		//General Errors 551-600
		int	INVITE_RCVD_AFTER_FT			= 551;
		int	INVITE_RCVD_WO_SDP				= 552;
		int	INVITE_RCVD_WO_IAM				= 553;
		int	UNSUPPORTED_SE_HEADER			= 554;
		int	ANN_COPY_OPER_FAILED			= 555;
		int	UNKNOWN_DIALOG_RCVD				= 556;
		int	DIAGLOG_MANDATORY_PARAM_MIS		= 557;
		int	COMP_MANDATORY_PARAM_MIS		= 558;
		int	UNKNOWN_COMPIND_RCVD			= 559;
		int	MISSING_ORIG_INVITE_REQ			= 560;
		int	MISSING_TERM_INVITE_REQ			= 561;
		int SERVICE_TYPE_NOT_FOUND			= 562;
		int	FAILED_TO_INITIALIZE_SRV		= 563;
		int	INVITE_NOT_FOR_SRV				= 564;

		//Session expirations 601-620
		int	APPSESSION_EXPIRED				= 601;
		int	SESSION_EXPIRED_TERM_IVR		= 602;
		int	SESSION_EXPIRED_ORIG_IVR		= 603;
		int	SESSION_EXPIRED_TERM			= 604;
		int	SESSION_EXPIRED_ORIG			= 605;
		
		//Parsing Failures 621-660
		int IDP_ASN_PARSING_FAIL			= 621;
		int IDP_INVALID_EXTN_TYPE			= 622;
		int IDP_PARAM_OUT_OF_RANGE			= 623;
		int IDP_MISSING_CALLING_PARTY		= 624;
		int IDP_MISSING_CPC					= 625;
		int IDP_MISSING_TMR					= 626;
		int IDP_MISSING_ACPC				= 627;
		int IDP_MISSING_FCI					= 628;	
		int ENC_ASN_PARSING_FAIL			= 631;
		int ENC_PARAM_OUT_OF_RANGE			= 632;
		int UNKNOWN_RESULT					= 635;
		int ER_ASN_PARSING_FAIL			= 636;
		int ERB_PARAM_OUT_OF_RANGE			= 641;
		int ERB_INVALID_LEG_TYPE			= 642;
		int ERB_INVALID_RECEIVING_SIDE		= 643;
		int ERB_INVALID_INPUT				= 644;
		int ERR_MSG_MAND_PARAM_MISSING		= 651;
		int ERR_MSG_PARAM_NOT_SET			= 652;
		int ERR_MSG_INVALID_INVOK_ID		= 653;
		int REJECT_MSG_MAND_PARAM_MISSING	= 654;
		int REJECT_MSG_PARAM_NOT_SET		= 655;
		int REJECT_INVALID_INVOK_ID			= 656;
		int REJECT_INVALID_PROBLEM_TYPE		= 657;
		int UNEXPECTED_SRR = 658;
		int UNEXPECTED_ACR = 659;
		int UNEXPECTED_CONT = 660;
		int ERB_ASN_PARSING_FAIL = 661;
		int ACR_PARAM_OUT_OF_RANGE = 662;
		int ACR_ASN_PARSING_FAIL = 663;
		int SRR_ASN_PARSING_FAIL = 664;
		int SRR_PARAM_OUT_OF_RANGE = 665;
		int ROUTESELECTFAILURE_RCVD_FROM_TERM = 666;
		Object ARI_ASN_PARSING_FAIL = 667;
		int OBGW_NOT_AVAILABLE = 698;

}
