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
package com.agnity.ph.sip;

import com.agnity.ph.common.PhConstants;

import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;

/**
 * This class contains MPH startup configuration in the form of key value pairs.
 * For ex Sip error code for exception cases. Service can change these values.
 */
public class SipProtocolConfig {
	
	private static Logger logger = Logger.getLogger(SipProtocolConfig.class);

	private static final transient Map<String, String> config = new HashMap<String, String>();
	/*
	 * SIP error response codes
	 */

	// SIP error response code that mPH sends in case of any exception in mPH
	public static final String SIP_RES_CODE_IN_EXCEPTION = "SIP_RES_CODE_IN_EXCEPTION";

	/*
	 * SIP error code which is used in error scenarios where mPH has taken the
	 * decision to disconnect the call.
	 * 
	 * Like error response received for Re-Invite (Or)
	 * 
	 * A incoming request/response is received in unexpected service state
	 */
	public static final String SIP_RES_CODE_IN_ERROR = "SIP_RES_CODE_IN_ERROR";

	/*
	 * SIP error code which is used if call is disconnected after SAS switchover
	 */
	public static final String SIP_RES_CODE_AFTER_FT = "SIP_RES_CODE_AFTER_FT";

	// SIP error code which is used if release CAUSE_CODE is not set by service
	// on legData
	public static final String DEFAULT_STATUS_CODE = "DEFAULT_STATUS_CODE";

	// SIP error response code that mPH sends in case of service fails to
	// initialize
	public static final String SIP_RES_CODE_SRV_INIT_FAILED = "SIP_RES_CODE_SRV_INIT_FAILED";

    // SIP error response code that mPH sends in case Correlated INVITE received
	// after sas switchover
	public static final String SIP_RES_CODE_INVITE_RCVD_AFTER_FT = "SIP_RES_CODE_INVITE_RCVD_AFTER_FT";
	/*
	 * SIP Error code for loop-back INVITE which is not intended for current
	 * service
	 */
	public static final String SIP_RES_CODE_INVITE_NOT_FOR_SRV = "SIP_RES_CODE_INVITE_NOT_FOR_SRV";

	// SIP error code which is used if ACK/PRACK timeout happens
	public static final String ACK_PRACK_TIMEOUT = "ACK_PRACK_TIMEOUT";

	/*
	 * Number of calls for which mPH should calculate average response time. If
	 * 0, then mPH doesnot calculate average response time for initial INVITE
	 */
	public static final String AVG_TIME_CALC_COUNT = "AVG_TIME_CALC_COUNT";

	/*
	 * TRUE/FALSE. If TRUE then mPH sends 100 trying for received INVITE
	 */
	public static final String INVITE_100_TRYING_FLAG = "INVITE_100_TRYING_FLAG";

	public static final String REINVITE_100_TRYING_FLAG = "REINVITE_100_TRYING_FLAG";

	/*
	 * SAS FIP
	 */
	public static final String FLOATING_IP = "FLOATING_IP";

	/*
	 * SAS listen port for HTTP requests
	 */
	public static final String HTTP_PORT = "HTTP_PORT";

	/*
	 * If TRUE them mPH replicates the app-session after receiving first
	 * reliable provisional response from term
	 */
	public static final String REL_PROV_RESP_REPLICATION = "REL_PROV_RESP_REPLICATION";

	/*
	 * If TRUE then mPH will support reliable provisional responses
	 */
	public static final String REL_PROVISIONAL_SUPPORTED = "REL_PROVISIONAL_SUPPORTED";

	/*
	 * 
	 * TERM_SESSION_REF_SUPPORT - Flag is to indicate whether session refresh
	 * needs to be supported for term or not. Possible values: 0 - Session
	 * Refresh is not required 1 - Session refresh required unconditionally 2 -
	 * Session refresh required when requested from originating side
	 */
	public static final String TERM_SESSION_REF_SUPPORT = "TERM_SESSION_REF_SUPPORT";

	/*
	 * TERM_SESSION_REFRESH_HEADER - Flag is to indicate whether Min-SE and
	 * Session expires values of orig needs to be used or these should be used
	 * from SipProtocolConfig configuration 1 - Use parameters of originating
	 * request 2 - Use parameters from ProtocolConfig configuration
	 */
	public static final String TERM_SESSION_REFRESH_HEADER = "TERM_SESSION_REFRESH_HEADER";

	/*
	 * TERM_SESSION_EXPIRES_VAL and TERM_MIN_SE_VAL are the values of
	 * session-expires and min-se headers in seconds for term leg.These values
	 * are set in outgoing INVITES to term leg if value of
	 * TERM_SESSION_REFRESH_HEADER is 2
	 */
	public static final String TERM_SESSION_EXPIRES_VAL = "TERM_SESSION_EXPIRES_VAL";
	public static final String TERM_MIN_SE_VAL = "TERM_MIN_SE_VAL";

	/*
	 * Time on ms. Session refreh timer are not replicated. If SAS FT happens
	 * then they are started on new active sas forthe remaining time. While
	 * starting these timers on new active sas this SESSION_REF_GRACE_PERIOD is
	 * added in remaining session_refreh time
	 */
	public static final String SESSION_REF_GRACE_PERIOD = "SESSION_REF_GRACE_PERIOD";

	/*
	 * Duration [in ms] of INVITE pending transaction timer which is started by
	 * mPH on receiving 491 for INVITE
	 */
	public static final String INV_PENDING_TRANS_TIMER_DURATION = "INV_PENDING_TRANS_TIMER_DURATION";
	
	public static final String NO_INVITE_WITHOUT_SDP ="NO_INVITE_WITHOUT_SDP";

	public static final String NOTIFY_180_WITHOUT_SDP = "NOTIFY_180_WITHOUT_SDP";

	public static final String REL_FAILURE_RETRY_TIMER_DURATION = "REL_FAILURE_RETRY_TIMER_DURATION";
	
	public static final String LOOPBACK_VIA_HEADER ="LOOPBACK_VIA_HEADER";
	
	public static final String CPC_PARSING_POLICY="CPC_PARSING_POLICY";//default is PAI, FROM
	
	public static final String SEND_CPC_PARAM="SEND_CPC_PARAM";
	
	public static final String SEND_OLI_PARAM="SEND_OLI_PARAM";
	
	public static final String SEND_ISUB_IN_FROM="SEND_ISUB_IN_FROM";

	public static final String READ_PARAM_FROM_ISUP_CONTENT = "READ_PARAM_FROM_ISUP_CONTENT";

	public static final String SET_OLI_AS_CPC = "SET_OLI_AS_CPC";
	
	public static final String OLI_AS_CPC_PAYPHONE_VALUE = "OLI_AS_CPC_PAYPHONE_VALUE";
	
	public static final String REDIRECT_RESPONSE_CODE="REDIRECT_RESPONSE_CODE";

	public static final String OVERRIDE_PAI_PCI_HDRS_ADDRESS = "OVERRIDE_PAI_PCI_HDRS_ADDRESS";
	public static final String ISUP_ONLY_INITIAL="SIPT_ONLY_INITIAL";
	//#0 - ITU and 1- ANSI
	public static final String	ISUP_PROTOCOL_VARIANT="ISUP_PROTOCOL_VARIANT";
	public static final String	ISUP_ANSI_VERSION="ISUP_ANSI_VERSION";
	public static final String REMOVE_PAI_PRIVACY="REMOVE_PAI_PRIVACY";

	public static final String SEND_DNIS_IN_SIPT = "SEND_DNIS_IN_SIPT";	
	
	public static final String SEND_USER_PHONE_PARAM_TO_TERM="SEND_USER_PHONE_PARAM_TO_TERM";

	public static final String READ_CALLED_NUM_FROM_RURI = "READ_CALLED_NUM_FROM_RURI";

	public static final String DEFAULT_ERB_SET = "DEFAULT_ERB_SET";
    public static final String MAX_BCSM_EVENTS = "MAX_BCSM_EVENTS";
    public static final String DFC_IN_SEPARATE_DIALOGUE = "DFC_IN_SEPARATE_DIALOGUE";
	
	public static final String PUT_MUSIC_ON_HOLD="PUT_MUSIC_ON_HOLD";
	
	public static final String ANN_FILE_MUSIC_ON_HOLD="ANN_FILE_MUSIC_ON_HOLD";

	public static final String SIP_PORT = "SIP_PORT";
	
	public static final String ORIG_ALLOWED_MIN_SE = "ORIG_ALLOWED_MIN_SE";
	
	public static final String MAX_CALL_DURATION = "MAX_CALL_DURATION";
	 
	public static final String SERVICE_CHAINING_ENABLED = "SERVICE_CHAINING_ENABLED";

	public static final String HTTP_OP_RESPONSE_TIME = "HTTP_GW_RESPONSE_TIME";
	
	public static final String PARSE_PAI_TEL_SIP_USER="PARSE_PAI_TEL_SIP_USER";
	
	public static final String SIP_MS_PASS_RTK="SIP_MS_PASS_RTK";
	
	public static final String DISABLE_OUTBOUND_PROXY = "DISABLE_OUTBOUND_PROXY";
	public static final String RTP_TUNNELLING = "RTP_TUNNELLING";
	
	public static final String RTP_TUNNELLING_18X_CODE = "RTP_TUNNELLING_18X_CODE";
	
	public static final String RELAY_2XX_IN_EARLY_MEDIA = "RELAY_2XX_IN_EARLY_MEDIA";

	public static final String SERVER_SITE_NAME = "SERVER_SITE_NAME";
	
	public static final String DUMP_COUNTERS_TIME = "DUMP_COUNTERS_TIME";
	
	public static final String DUMP_COUNTERS = "DUMP_COUNTERS";
	
	public static final String ADD_P_CHARGE_VECTOR = "ADD_P_CHARGE_VECTOR";

	public static final String SEND_INCOMING_RURI_TOTERM = "SEND_INCOMING_RURI_TOTERM";

	public static final String DEPLOY_APP_NAME_WITH_VERSION = "DEPLOY_APP_NAME_WITH_VERSION";

	public static final String PREFIX_RN_NOT_FOUND_VACANT_CODE = "PREFIX_RN_NOT_FOUND_VACANT_CODE";

	public static final String PREFIX_RN_WITH_REDIECT_CONTACT_USER = "PREFIX_RN_WITH_REDIECT_CONTACT_USER";
	
	public static final String DEFAULT_DIAMETER_SH_DEST_REALM="DEFAULT_DIAMETER_SH_DEST_REALM";
	
	static {
		System.out.println("loading mph static properties ");
		setConfigData(SIP_RES_CODE_IN_EXCEPTION, "503");
		setConfigData(SIP_RES_CODE_IN_ERROR, "503");
		setConfigData(SIP_RES_CODE_SRV_INIT_FAILED, "503");
		setConfigData(SIP_RES_CODE_INVITE_RCVD_AFTER_FT, "503");
		setConfigData(SIP_RES_CODE_INVITE_NOT_FOR_SRV, "503");
		setConfigData(DEFAULT_STATUS_CODE, "503");
		setConfigData(ACK_PRACK_TIMEOUT, "503");
		setConfigData(SIP_RES_CODE_AFTER_FT, "503");
		setConfigData(AVG_TIME_CALC_COUNT, "0");
		setConfigData(INVITE_100_TRYING_FLAG, PhConstants.FALSE);
		setConfigData(REINVITE_100_TRYING_FLAG, PhConstants.FALSE);
		setConfigData(FLOATING_IP, "127.0.0.1");
		setConfigData(HTTP_PORT, "8080");
		setConfigData(REL_PROV_RESP_REPLICATION, PhConstants.TRUE);
		setConfigData(REL_PROVISIONAL_SUPPORTED, PhConstants.TRUE);
		setConfigData(TERM_SESSION_REF_SUPPORT, "2");
		setConfigData(TERM_SESSION_REFRESH_HEADER, "2");
		setConfigData(TERM_SESSION_EXPIRES_VAL, "180");
		setConfigData(SESSION_REF_GRACE_PERIOD, "5000");
		setConfigData(TERM_MIN_SE_VAL, "90");
		setConfigData(INV_PENDING_TRANS_TIMER_DURATION, "3000");
		setConfigData(REL_FAILURE_RETRY_TIMER_DURATION, "1000");
		setConfigData(NO_INVITE_WITHOUT_SDP, PhConstants.FALSE);
		setConfigData(NOTIFY_180_WITHOUT_SDP, PhConstants.FALSE);
		setConfigData(LOOPBACK_VIA_HEADER, PhConstants.DEFAULT);
		setConfigData(CPC_PARSING_POLICY, PhConstants.DEFAULT);
		setConfigData(SEND_CPC_PARAM, PhConstants.TRUE);
		setConfigData(SEND_OLI_PARAM, PhConstants.TRUE);
		setConfigData(SET_OLI_AS_CPC, PhConstants.FALSE);
		setConfigData(READ_PARAM_FROM_ISUP_CONTENT, PhConstants.FALSE);
		setConfigData(OLI_AS_CPC_PAYPHONE_VALUE, "27");
		setConfigData(REDIRECT_RESPONSE_CODE,"302");
		setConfigData(OVERRIDE_PAI_PCI_HDRS_ADDRESS,PhConstants.FALSE);
		setConfigData(ISUP_ONLY_INITIAL, PhConstants.TRUE);
		setConfigData(ISUP_PROTOCOL_VARIANT,"0");
		setConfigData(ISUP_ANSI_VERSION,"ansi88");
		setConfigData(REMOVE_PAI_PRIVACY,PhConstants.FALSE);
		setConfigData(SEND_DNIS_IN_SIPT, PhConstants.FALSE);
		setConfigData(SEND_USER_PHONE_PARAM_TO_TERM, PhConstants.FALSE);
		setConfigData(READ_CALLED_NUM_FROM_RURI, PhConstants.FALSE);	
		setConfigData(DEFAULT_ERB_SET, "ERB_ROUTESELECTFAILURE|ERB_BUSY|ERB_NO_ANSWER|ERB_ANSWER|ERB_DISCONNECT|ERB_ABANDON");
        setConfigData(DFC_IN_SEPARATE_DIALOGUE, "false");
		setConfigData(PUT_MUSIC_ON_HOLD,PhConstants.FALSE);
		setConfigData(ANN_FILE_MUSIC_ON_HOLD,"MusicOnHold.wav");
		setConfigData(SIP_PORT,"5060");
		setConfigData(ORIG_ALLOWED_MIN_SE,"600");
		setConfigData(MAX_CALL_DURATION, null);
		setConfigData(SERVICE_CHAINING_ENABLED, PhConstants.FALSE);
		setConfigData(PARSE_PAI_TEL_SIP_USER, PhConstants.FALSE);
		setConfigData(SIP_MS_PASS_RTK, "0");
		setConfigData(DISABLE_OUTBOUND_PROXY,PhConstants.TRUE);
		setConfigData(RTP_TUNNELLING, "0");
		setConfigData(DUMP_COUNTERS, "false");
		setConfigData(DUMP_COUNTERS_TIME, "10");
		setConfigData(ADD_P_CHARGE_VECTOR, "true");
		setConfigData(SEND_INCOMING_RURI_TOTERM, "false");
		setConfigData(DEPLOY_APP_NAME_WITH_VERSION, "false");
		setConfigData(PREFIX_RN_WITH_REDIECT_CONTACT_USER, "true");
		setConfigData(PREFIX_RN_NOT_FOUND_VACANT_CODE, "12682999");
	}

	public static String getConfigData(String key) {
		
		String value =config.get(key);
		
		if (logger.isDebugEnabled()) {
			logger.debug("getConfigData  key: "+ key+" value: "+value);
		}
		return value;
	}

	public static void setConfigData(String key, String value) {
		if (logger.isDebugEnabled()) {
			logger.debug("setConfigData  key: "+ key+" value: "+value);
		}
		config.put(key, value);
	}

}
