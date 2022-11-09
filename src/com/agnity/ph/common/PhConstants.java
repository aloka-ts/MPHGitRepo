/*******************************************************************************
 * Copyright (c) 2011 Agnity, Inc. All rights reserved.
 * <p>
 * This is proprietary source code of Agnity, Inc.
 * <p>
 * Agnity, Inc. retains all intellectual property rights associated
 * with this source code. Use is subject to license terms.
 * <p>
 * This source code contains trade secrets owned by Agnity, Inc.
 * Confidentiality of this computer program must be maintained at
 * all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.ph.common;

import com.baypackets.ase.sbb.MediaServer;

/**
 * This class is used to define constants used by all the Protocol handlers 
 *
 */
public interface PhConstants {

    String ADDITIONAL_HEADERS = "ADDITIONAL_HEADERS";

    /*
     * Session Expiry constants
     */
    String SESSION_EXPIRE_HEADER = "Session-Expires";
    String MIN_SE_HEADER = "Min-SE";
    String SESSION_EXPIRE_MIN_SE = "SESSION_EXPIRE_MIN_SE";

    String REFRESHER_UAS = "uas";
    String REFRESHER_UAC = "uac";

    String TRUE = "TRUE";
    String FALSE = "FALSE";


    /*
     * Attribute with this name is set by SAS on every sip session. Value of
     * this attribute is the first INVITE received or sent on this sip session
     */
    String ORIG_REQUEST = "ORIG_REQUEST";

    String MATCHES_CALL_CRITERIA = "MATCHES_CALL_CRITERIA";

    /*
     * Connection Types
     */
    int ORIG_CONNECTION_TYPE = 0;
    int IVR_CONNECTION_TYPE = 1;
    int TERM_CONNECTION_TYPE = 2;
    int TERM_IVR_CONNECTION_TYPE = 3;

    /*
     * X-ISC-SVC header is used by SAS for inter-service communication on the
     * same SAS node
     */
    String X_ISC_SVC = "X-ISC-SVC";

    /*
     * AppSession timeout in minutes 1. While cleaning all sip-sessions, set the
     * app-session timeout to CLEANUP_APPSESSION_TIMEOUT. So that app-session
     * woule be cleaned automatically after 2 minutes 2. While sending
     * 302response for INVITE set appSession timeout to
     * CLEANUP_APPSESSION_TIMEOUT, so that if ACK for 302 is not received within
     * 2 minutes even then app-Session is cleaned after
     * CLEANUP_APPSESSION_TIMEOUT minutes
     */
    int CLEANUP_APPSESSION_TIMEOUT = 2;

    /*
     * Sbb factory class. SAS create this attribute on app-session and mPh uses
     * this factory class to create MS-Sbb instances
     */
    String SBB_FACTORY = "SBBFactory";

    /*
     * Leg ID. This attribute is set on every sip session and is used to fetch
     * LegData from Calldata for that sip session.
     */
    String LEG_ID = "LEG_ID";
    /*
     * Calling party would always be one
     */
    String ORIG_LEG_ID = "P_LEG1";

    /*
     * SIP Protocol messages
     */
    String INFO_REQUEST = "INFO";
    String PRACK_REQUEST = "PRACK";
    String CANCEL_REQUEST = "CANCEL";
    String BYE_REQUEST = "BYE";
    String INVITE_REQUEST = "INVITE";
    String UPDATE_REQUEST = "UPDATE";
    String HTTP_GET_REQUEST = "GET";

    /*
     * When SN sends INFO to IVR to stop playing announcement, then IVR responds
     * this INFO with another INFO having reason field value = stopped. So
     * following constant represents this stopped reason field.
     */
    String STOP_ANN_INFO_REASON = "stopped";

    /*
     * SDP types
     */
    String APPLICATION_SDP = "application/sdp";

    /*
     * Attributes required by Ms-Sbb if orig sip-session is in confirmed state
     */
    String DIALOG_STATE = "DIALOG_STATE";
    int DIALOG_STATE_CONFIRMED = 2;

    /*
     * FLOATING_IP string in VXML file. While initialing media-server
     * connection, mPH replaces this string in VXML file with SAS FIP
     */
    String FLOATING_IP_STR = "FLOATING_IP";

    /*
     * HTTP_LISTENER_PORT string in VXML file. While initialing media-server
     * connection, mPH replaces this string in VXML file with SAS FIP
     */
    String HTTP_LISTENER_PORT_STR = "HTTP_LISTENER_PORT";

    String REQUIRE_HEADER = "Require";
    String SUPPORTED_HEADER = "Supported";
    String SUPPORTED_TIMER = "timer";
    String SUPPORTED_100REL = "100rel";
    String SUPPORTED_TIMER_100REL = "100rel,timer";
    String REFRESHER_TAG = "refresher";

    String CONTENT_DISPOSITION_HDR = "Content-Disposition";
    String CONTENT_DISPOSITION_VALUE_FOR_ISUP = "signal; handling=required";
    String CONTENT_DISPOSITION_VALUE_FOR_SDP = "session; handling=required";

    String CONTACT_HEADER = "Contact";

    String R_SEQ_HEADER = "RSeq";

    String TO_HEADER = "To";

    String DIVERSION_HEADER = "Diversion";

    /*
     * First SIP-T and INAP call were taking almost 11 seconds to process. To
     * solve this issue SAS will generate an invite and send a warm up call to
     * service.
     */
    String WARMUP_HEADER = "WARMUP";

    String A_INACTIVE = "inactive";
    String A_SENDONLY = "sendonly";
    String A_RECVONLY = "recvonly";
    String A_SENDRECV = "sendrecv";

    String CORRELATION_MAP = "Correlation-Map";
    String CORRELATION_ID = "CORRELATION_ID";

    /*
     * ACK_FOR_RESPONSE is a attribute, set by SAS on sipSession. Value of this
     * attribute is ERROR or SUCCESS depending on whether current ACK request is
     * received for INVITE 2xx response or failure response
     */
    String ACK_FOR_RESPONSE = "ACK_FOR_RESPONSE";
    String SUCCESS = "SUCCESS";

    String FILE_SEPARATOR = "/";

    /*
     * This attribute is set on all IVR sipSessions to distinguish IVR
     * sipSessions from orig and term sip sessions
     */
    String IVR_LEG = "IVR_LEG";

    /*
     * Following constants are the default values while creating Ann PlaySpec
     */
    int DEFAULT_PNC_MIN_DIGIT = 1;
    int DEFAULT_PNC_MAX_DIGIT = 16;
    int DEFAULT_PNC_FIRST_DIG_TIME_MS = 8000; // In ms
    int DEFAULT_PNC_INTER_DIG_TIME_MS = 4000; // In ms

    int CAUSE_CODE_NOANSWER = 408;
    int CAUSE_CODE_BUSY = 486;
    int CAUSE_CODE_NOTAVAIL = 480;
    /*
     * If service does not set LegDataAttributes.P_CAUSE_CODE or value of
     * LegDataAttributes.P_CAUSE_CODEis 0 them mPH uses default error code if
     * call is reqruired to be disconnected
     */
    int INVALID_CAUSE_CODE = 0;

    int INVOKE_CLASS_TYPE = 1;

    /*
     * Attributes used to fetch App Session from Tcap Session
     */
    String APPLICATION_SESSION_ID = "ApplicationSession";

    /*
     * SS7 specific constants
     */
    String TCAP_SESSION_ID = "Tcap-Session";

    /*
     *
     */
    String UNARMED_ERROR_TYPE = "UNARMED_ERROR_TYPE";

    String DFC_SENT = "DFC_SENT";

    String ASSIST_APP_SESSION_ID = "ASSIST_APP_SESSION_ID";

    String CORRELATION_PREFIX = "008863";
    String CORRELATION_ASSIST = "0";
    String CORRELATION_HANDOFF = "1";
    String CORRELATION_NO_FLEX_CHARGING = "0";

	/*
     * FCI_ORIGIN_NUMBER is set by service in CallData. So that PH can use this
	 * while sending FCI
	 */
    //String FCI_ORIGIN_NUMBER = "FCI_ORIGIN_NUMBER";

	/*
	 * FCI_DEST_NUMBER is set by service in CallData. So that PH can use this
	 * while sending FCI
	 */
    //String FCI_DEST_NUMBER = "FCI_DEST_NUMBER";

    String FOR_HANDOFF = "FOR_HANDOFF";

    String LEG1_TRIGGERS_ARMED = "LEG1_TRIGGERS_ARMED";

    String CDR_REF = "CDR_REF";

    String SWITCH_CODE = "SWITCH_CODE";

    String SN_CHARGE_AREA_STR = "SN_CHARGE_AREA_STR";

    String CLUSTER_NUMBER_STR = "CLUSTER_NUMBER_STR";


    String INAP_MSG_TRACE = "INAP_MSG_TRACE";

    String SERVICE_CDR_FLAG = "SERVICE_CDR_FLAG";

    String ASSIST_LEG = "ASSIST_LEG";

    String CDR_PART_SEPARATOR = ",";


    String READY_TO_INVALIDATE = "READY_TO_INVALIDATE";

    String MULTIPART_MIXED = "multipart/mixed";

    String TCAP_CDR_REF = "tcap.com.baypackets.ase.sbb.CDR";

    // constants for Trace call Service
    String TRACE_MESSAGE_END_FOOTER = "------------------END OF TRACE-----------------------";
	String traceDelim = "------------------------------------------------------\n";
	String incomingMsgHdr = "----INCOMING MESSAGE RECEIVED FROM NETWORK----\n";
	String outgoingMsgHdr = "----OUTGOING MESSAGE SENT TO NETWORK----\n";
	String protocolType ="Protocol Type: INAP\n";
	String sccpParams="SCCP Parameters\n";
	String tcapParams="TCAP Parameters\n - ";
	String inapOperation="INAP Operation: ";
	

    String CARRIER_CODE = "CARRIER_CODE";

    String NO_ANSWER_TIMER_IND = "NO_ANSWER_TIMER_IND";

    String REASON_HEADER = "Reason";
    String REASON_HEADER_VALUE_INITIAL = "Q.850;cause=";
    String CONTENT_TYPE = "Content-Type";
    String APPLICATION_ISUP = "application/isup";
    String APPLICATION_ISUP_VERSION = "version";
    String APPLICATION_ISUP_WITH_VERSION = "application/isup; version=ttc93-sn; base=ttc93+";

    int NO_ANS_TIMEOUT_CV_TERM_LEG = 16;
    int BYE_CV_ANM_ALREADY_SENT = 31;

    /**
     * Following fields added for MPH1.0 enhancements
     */
    String P_SIG_INFO_HEADER = "P-Sig-Info";

    String FROM_HEADER = "From";

    String TRUNK_GRP_ID = "TGID";

    String SWITCH_ID = "SWID";

    String ISUP_OLI_PARAM = "isup-oli";

    String OTG_PARAM = "otg";

    String CPC_PARAM = "cpc";

    String JIP_PARAM = "jip";

    String CIC_PARAM = "cic";

    String NOA_PARAM = "NOA";

    String P_CHARGE_INFO = "P-Charge-Info";

    String P_CARRIER_INFO = "P-Carrier-Info";

    String DTG_PARAM = "dtg";

    String TGRP_PARAM = "tgrp";

    String TRUNK_CONTEXT = "trunk-context";

    String NPDI_PARAM = "npdi";

    String TARGET_DIALOG = "Target-Dialog";

    String REFER_REQUEST = "REFER";

    String NOTIFY_REQUEST = "NOTIFY";

    String CSEQ_HEADER = "CSeq";

    String REPLACES_HEADER = "Replaces";

    String NOTIFY_REFER_CONTENT_TYPE = "message/sipfrag";

    String REFER_SUCESS_CONTENT = "SIP/2.0 200 OK";

    String REFER_TRYING_CONTENT = "SIP/2.0 100 Trying";

    String REFER_TO_HEADER = "Refer-To";

    String ANONYMOUS = "anonymous";

    String EVENT_HEADER = "Event";

    String REFER_EVENT = "refer";

    String SUBSCRIPTION_HEADER = "Subscription-State";

    String RESTRICTED = "restricted";

    String PAI_HEADER = "P-Asserted-Identity";

    int SERVER_TRXN_TIMEOUT = 408;

    String ROUTE_HEADER = "Route";

    String VIA_HEADER = "Via";

    String PATH_HEADER = "Path";

    String RACK_HEADER = "RAck";

    String PRIVACY_HEADER = "Privacy";

    String PROXY_REQUIRES_HEADER = "Proxy-Require";

    String CALLID_HEADER = "Call-ID";

    String CONTENT_LENGTH_HEADER = "Content-Length";

    String RECORD_ROUTE = "Record-Route";

    String REFERRED_BY_HEADER = "Referred-By";

    String P_CHARGE_VECTOR = "P-Charging-Vector";

    String RETRY_AFTER = "Retry-After";
    
    String RES_PRIORITY_HEADER = "Resource-Priority";

    String URI_SCHEME_SIP = "sip";
    
    String HISTORY_INFO = "History-Info";

    String URI_SCHEME_TEL = "tel";
    String SEMI_COLON_STR = ";";
    String EQUALS_STR = "=";
    
    String ISUB = "isub";

    String SESSION_READY_TO_INVALIDATE = "S_R_INVALIDATE";

    /**
     * For the first term provisional response containing SDP, this attribute is set true. This helps in determining that
     * some SDP has already been negotiated with orig party. This flag is set to true when MS connection has been made since
     * SDP negotaition happens in that case too.
     */
    String SESSION_ID_WITH_ORIG_SDP_EXCHANGED = "SESS_ID_W_ORIG_EXCHG";

    String DEFAULT = "DEFAULT";
    String CPC_POLICY_FROM_FIRST = "FROM_FIRST";
    String PAY_PHONE = "payphone";
    String MSML_TYPE = "msml";
    String MIN_DIGITS = "min";
    String MAX_DIGITS = "max";
    String RETURN_KEY = "rtk";
    String CANCEL_KEY = "cancel";
    String DOMAIN = "DOMAIN";
    String CASFIP = "CASFIP";

    String STALE = "STALE";
    String MDC_CALL_ID_CONST = "ORIG_CID";
	String SERVICE_ID = "SERVICE_ID";

	String APP_DTMF_CONTENT_TYPE = "application/dtmf";
	
	  String ALERT_INFO_HEADER = "Alert-Info";
	  String ALERT_INFO_CALL_WAITING="urn:alert:service:call-waiting";
	  String WEB_USER = "WEB_USER";
	  String X_APP_HEADER = "X-App";
	  String X_CONF_HEADER = "X-Conference-Id";

	String PEER_REQUEST_LIST = "PEER_REQUEST_LIST";
	
	
	    String X_HOST_CONF_HEADER = "X-Host-Conference-Id";
	    
	    // Conference Related Constants
	    String STRING_VIDEO = "video";
	    String MEDIA_TYPE = "MEDIA_TYPE";
	    String DISPLAY_ID = "DISPLAY_ID";
	    String UPDATE_CONF = "UPDATE_CONF";
	    String UPDATE_CONF_SIZE = "UPDATE_CONF_SIZE";
	    String NO_OF_VIDEO_PARTICIPATS = "NO_OF_VIDEO_PARTICIPATS";
	    int CONFERENCE_TYPE_VIDEO = 1;
	    String CONF_IDENTIFIER = "conference";
	    String ATTRIB_CONF_CONTROLLER = "CONF_CONTROLLER";
	    String ATTRIB_PARTICIPANT_INVITE = "PARTICIPANT_INVITE";
	    String ATTRIB_PARTICIPANT_CONFID = "PARTICIPANT_CONFID";
	    String ATTRIB_PARTICIPANT_COUNT = "PARTICIPANT_COUNT";
	    String ATTRIB_PARTICIPANT_SBB = "PARTICIPANT_SBB";
	    String ATTRIB_HOST_SESSION = "HOST_SESSION";
	    String ATTRIB_CONF_HOST_UNJOINED = "CONF_HOST_UNJOINED";
	    String ATTRIB_HOST_SBB = "HOST_SBB";
	    String WAITING_PARTY = "WAITING_PARTY";
	    int NINE_PARTY = 9;
	    int FOUR_PARTY = 4;
	    String ATTRIB_SBB_FACTORY = "SBBFactory";
	    String CONF_B2BUA_MODE = "CONF_B2B_MODE"; //This mode will be used to create conference after 2 parties connected.
	    String ATTRIB_PEER_JOINED = "ATTRIB_PEER_JOINED";
	    
		int MS_CAPABILITIES = MediaServer.CAPABILITY_DIGIT_COLLECTION
				| MediaServer.CAPABILITY_VAR_ANNOUNCEMENT
				| MediaServer.CAPABILITY_AUDIO_RECORDING;

		CharSequence REPLACES = "Replaces";
		
		String CALLPICKUP_LINKED_REQUEST="CALLPICKUP_LINKED_REQUEST";

		String P_PREFFERED_IDENTITY = "P-Preferred-Identity";
		
		String USER_AGENT_HEADER="User-Agent";
	
    //*****************Timers****************

    // While creating session refresh timer, name of Session refresh timer is SESSION_REFRESH_TIMER concatanated with session ID ofthat session
    String SESSION_REFRESH_TIMER = "SESSION_REFRESH_TIMER";

    // This timer is started by mPH if it receives 491 response for Re-INVITE
    String INV_PNDING_TRANS_TIMER = "INV_PNDING_TRANS_TIMER";
    String CORRELATION_TIMER = "CORRELATION_TIMER";
    String CDR_TIMER = "CDR_TIMER";

    // Name of no-answer timer
    String NO_ANSWER_TIMER = "NO_ANSWER_TIMER";
    String AT_ACK_TIMER = "AT_ACK_TIMER";
    String CORRELATED_ORIG_CLEANUP_TIMER = "CORRELATED_ORIG_CLEANUP_TIMER";
    String APP_TIMER = "APP_TIMER";
    String NEXT_CALL_TIMER = "NEXT_CALL_TIMER";
    String CALL_TRANSFER_TIMER = "P_CALL_TRANSFER_TIMER";
    String REL_FAILURE_RETRY_TIMER = "REL_FAILURE_RETRY_TIMER";

	String MAX_CALL_DURATION_TIMER = "MAX_CALL_DURATION_TIMER";

	String MS_BYE_TRXN_INCREMENTED = "MS_BYE_TRXN_INCREMENTED";

	String HTTP_OPERATION_RESP_TIMER = "ACCESS_GATEWAY_TIMER";

	String HTTP_HEADER_TAG = "Authorization";
	
	String	HTTP_FACTORY  = "HttpFactory";
	
	String	INVITE_SUCCESS_RESPONSE  = "INVITE_SUCCESS_RESPONSE";
	
	String HTTP_CONTENT_TYPE_JSON="application/json";
	String HTTP_CONTENT_TYPE_JSON_CHARSET="application/json;charset=UTF-8";
	
	String HTTP_JSON_CALLID="callId";
	
	String HTTP_JSON_SESSID="sessionId";
	
	String HTTP_JSON_MSGTYPE="msgType";
	
	
	String HTTP_JSON_MSGTYPE_CUSTOMER="customer";

	String HTTP_RESP_SENT_TIMER = "HTTP_RESP_SENT_TIMER";
	

    //***************Timer Ends**************
	
	/**
	 * PSX releated constant
	 */
	String PSX_RN ="rn";
	
	String PSX_SSN = "ssn";
	
	String PSX_DPC = "dpc";
	
    String RN_PARAM = "rn";
    
    String SPID_PARAM = "spid";
    
    String DPC_PARAM = "dpc";
    
    String SSN_PARAM = "ssn";

	String ENUM_CLIENT = "EnumClient";

	String RF_FACTORY = "RfFactory";
	String RO_FACTORY = "RoFactory";
	String SMPP_FACTORY = "SmppFactory";
	String DIAMETER_SH_FACTORY = "ShFactory";
	String ENUM_FACTORY = "EnumFactory";

	String ENUM_ADAPTOR = "EnumAdaptor";

	String MRS_RELAY = "MRS_RELAY";
	String MRS="MRS";
	
	String PANI_HEADER="P-Access-Network-Info";
	
	String PANI_CGI_3GPP="cgi-3gpp";
	String PANI_CELL_ID="utran-cell-id-3gpp";
	String PANI_SAI_3GPP="utran-sai-3gpp";
	String PANI_XXXXXXX_CELL_ID="cell-id-3gpp";
	
	String PANI_3GPP_GERAN="3GPP-GERAN";
	String PANI_3GPP_UTRAN_TDD="3GPP-UTRAN-TDD";
	String PANI_3GPP_UTRAN_FDD="3GPP-UTRAN-FDD";
	String PANI_3GPP_E_UTRAN_FDD="3GPP-E-UTRAN-FDD";
	String PANI_3GPP_E_UTRAN_TDD="3GPP-E-UTRAN-TDD";
	String PANI_3GPP_NR_FDD="3GPP-NR-FDD";
	String PANI_3GPP_NR_TDD="3GPP-NR-TDD";
	
	int FETCH_PANI_CGI_3GPP = 0;
	int FETCH_PANI_CELL_ID = 1;
	int FETCH_PANI_SAI_3GPP = 2;
	int FETCH_PANI_XXXXXXX_CELL_ID = 3;

	String CLASS = "CLASS";
	String ISVM="ISVM";

	//
	int PLAY_OPERATION = 1;
	int PROMPT_AND_COLLECT_OPERATION = 2;

	String P_SERVED_USER = "P-Served-User";
	String HDR_PRIORITY = "Priority";

	String TRANSPORT_TCP = "tcp";
	String TRANSPORT_UDP="udp";

	String UREP_PARAM = "urep";

	String UCAT_PARAM = "ucat";
	
}
