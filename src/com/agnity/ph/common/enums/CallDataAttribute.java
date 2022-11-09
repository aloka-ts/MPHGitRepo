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
package com.agnity.ph.common.enums;
import static com.agnity.ph.common.enums.PersistanceType.NON_PERSISTABLE;
import static com.agnity.ph.common.enums.PersistanceType.PERSISTABLE;
/**
 * Created by ankitsinghal on 12/02/16.
 * <p>
 * This is a interface used for defining names for data attributes used for a
 * call .All the persistable properties are marked as P_ and non-persistable
 * properties are marked as NP_
 */
public enum CallDataAttribute {
    NP_REASON_FOR_RELEASE(PERSISTABLE),
    /*
     * This property refers session between SN and calling party
     */
    P_LEG1(PERSISTABLE),
    /*
     * This property refers session between SN and called party
     */
    P_LEG2(PERSISTABLE),
    
    /*
     * this is used to identify tranfer leg
     */
    TRANSFER_LEG(PERSISTABLE),
    /*
     * Application Session ID
     */
    P_APP_SESSION_ID(PERSISTABLE),
    /*
     * IP of call originator server. Taken from recent via header
     */
    P_ORIGIN_IP(PERSISTABLE),
    /*
     * Port of call originator server. Taken from recent via header
     */
    P_ORIGIN_PORT(PERSISTABLE),
    /*
     * IP of call termination server. Taken from leg data if present otherwise
     * origin ip
     */
    P_TERM_IP(PERSISTABLE),
    /*
     * Port of call termination server. Taken from leg data if present otherwise
     * origin ip
     */
    P_TERM_PORT(PERSISTABLE),
    /*
     * If TRUE then SAS switchover has been performed during this call. If false
     * or this property is not present in CallData then it means SASswitchover
     * has not been performed during call
     */
    NP_FT_CALL(PERSISTABLE),
    /*
     * If TRUE then tracing is enabled. If false or this property is not present
     * in CallData then it means Trace is disabled
     */
    P_TRACE_FLAG(PERSISTABLE),
    /*
     * Call Trace constraint list to be used for inap call tracing
     */
    P_TRACE_CONSTRAINT_ID(PERSISTABLE),
    /*
     * Trace Message Holder
     */
    P_TRACE_MESSAGE(PERSISTABLE),
    /*
     * Calling Leg Call ID. Used for logging purpose.
     */
    P_ORIG_LEG_CALL_ID(PERSISTABLE),
    /*
     * Called Leg Call ID. Used for logging purpose.
     */
    P_TERM_LEG_CALL_ID(PERSISTABLE),
    /*
     * Call Start, connect and disconnect date and time
     */
    P_CALL_START_TIME(PERSISTABLE),
    P_CALL_CONNECT_TIME(PERSISTABLE),
    P_CALL_DISCONNECT_TIME(PERSISTABLE),
    
    P_ALERTING_DURATION(PERSISTABLE),
    P_TOTAL_CALL_DURATION(PERSISTABLE),
    /**
     * Call Recording Start and end date.
     */
    P_RECORD_START_TIME(PERSISTABLE),
    
    P_RECORD_END_TIME(PERSISTABLE),
	/*
     * Instance of the service. ProtocolHandlerServler passes this instance to
	 * SIP Ph. And Sip PH stores this instance in Calldata so that sip PH can
	 * give callBacks to service
	 */
    //  P_SERVICE_HANDLER ,
    /*
     * SAS CDR utility reference.
     */
    P_CDR_REF(PERSISTABLE),
    /*
     * TRUE if final CDR has been written
     */
    P_FINAL_CDR_WRITTEN(PERSISTABLE),
    /*
     * It might be possible that FT happens before receiving 200 OK from IVR
     * while connecting IVR. In that case, IVR_SESSION_ID would not be available
     * in legData. To handle this case, LEG_ID_CONNECTED_TO_IVR to which service
     * is connecting to MS, is set in callData while initiating media server
     * connection.
     */
    P_LEG_ID_CONNECTED_TO_IVR(PERSISTABLE),
    P_INVITE_ROUTE_TIME(PERSISTABLE),
    P_ACK_ROUTE_TIME(PERSISTABLE),
    /*
     * Current action that mPH is performing
     */
    P_CURRENT_ACTION(PERSISTABLE),
    /*
     * If TRUE then mPH has received this call through
     * inter-service-communication
     */
    P_X_ISC_SVC(PERSISTABLE),
    /*
     * Correlation ID. USed for SIP REDIRECT+B2BUA flows
     */
    P_CORRELATION_ID(PERSISTABLE),
    /**
     * Redirect contact address received in 302 response
     */
    NP_REDIRECT_ADDRESS_LIST(PERSISTABLE),
    /*
     * SS7 Specific call data attributes
     */
    P_DIALOG_PRIMITIVE_TYPE(PERSISTABLE),
    /*
     * dialog id of the ss7 call
     */
    P_DIALOG_ID(PERSISTABLE),
    
    /**
    * Outgoing dialog ID  ofnew SS7 Call. 
    */
    P_OUTGOING_DIALOG_ID(PERSISTABLE),
    /*
     * Protocol used by a call used mainly for ss7 call to identify protocol
     */
    P_PROTOCOL(PERSISTABLE),
    /*
     * Destination number on which call is being routed
     */
    P_DESTINATION_NUMBER(PERSISTABLE),
    /*
     * the  dialled number  modified by the application
     */
    P_MODIFIED_CALL_NUMBER(PERSISTABLE),
    /*
     *flag is used internally by mph to check if dfc should be sent or not
     */
    P_DFC_REQUIRED_FLAG(PERSISTABLE),
    /*
     * The indicator set by mph to tell applictaion that call attempt has got failed
     */
    NP_FAILED_CALL_IND(NON_PERSISTABLE),
    /*
     * This attribute is used to tell that if cause value has been received for reelased call or not
     */
    P_CAUSE_VALUE_FROM_MSG(PERSISTABLE),
    /*
     * call release ( by mph and network )reason code for ss7 call
     */
    NP_RELEASE_REASON_CODE(PERSISTABLE),	//FIXME: Remove NP from EnumName
    /*
     * call release ( by mph and network )reason value for ss7 call
     */
    NP_RELEASE_REASON_VALUE(PERSISTABLE), //FIXME: Remove NP from EnumName
    /*
     * Service key for ITUT ss7 protocols e.g. INAPCSCF and CAPv2
     */
    P_SERVICE_KEY(PERSISTABLE),
    /*
     * invoke id used for sending ss7/tcap message
     */
    P_INVOKE_ID(PERSISTABLE),
    
    /**
    * invoke id for outgoing ss7 message
    */
    P_OUTGOING_INVOKE_ID(PERSISTABLE),
    /*
     *  ivr connection count used in FCI message
     */
    P_IVR_CONNECTION_COUNT(PERSISTABLE),
    P_SCCP_LOCAL_USER_ADDRESS(PERSISTABLE),
    P_INTERM_CDR_COUNT(PERSISTABLE),
    P_INTERM_CDR_DATE_TIME(PERSISTABLE),
    P_EXTERNAL_CDR_REQUIRED(PERSISTABLE),
    P_LAST_CALL_ACTION(PERSISTABLE),
    P_ASSIST_IND(PERSISTABLE),
    P_ATTEMPTED_IND(PERSISTABLE),
    P_WRITE_SERVICE_CDR(PERSISTABLE),
    P_AT_INVOKE_ID(PERSISTABLE),
    P_LAST_INVOKE_ID_RANGE_START(PERSISTABLE),
    P_LAST_INVOKE_ID_RANGE_END(PERSISTABLE),
    P_CLUSTER_NUMBER(PERSISTABLE),
    P_CALLED_PARTY_TYPE(PERSISTABLE),
    P_CHARGE_NUMBER(PERSISTABLE),
    P_USER_ID(PERSISTABLE),
    P_CARRIER(PERSISTABLE),
    P_COLLECTED_ADDRESS_INFO(PERSISTABLE),
    NP_ORIG_CALLED_PARTY_NUMBER(PERSISTABLE), //FIXME: Remove NP from EnumName
    P_CHARGE_PARTY_STATION_TYPE(PERSISTABLE),
    P_REDIRECTING_PARTY_ID(PERSISTABLE),
    P_ACG_ENCOUNTERED(PERSISTABLE),
    P_SS7_LEG_ID(PERSISTABLE),
    P_COLLECTED_DIGITS(PERSISTABLE),
    NP_HTTP_REQ(NON_PERSISTABLE),
    NP_HTTP_RES(NON_PERSISTABLE),
    NP_HTTP_REQ_CONTENT(NON_PERSISTABLE),
    NP_HTTP_REQ_CONTENT_TYPE(NON_PERSISTABLE),
    
    NP_HTTP_RES_CONTENT(NON_PERSISTABLE),
    NP_HTTP_RES_CONTENT_TYPE(NON_PERSISTABLE),
    
    NP_HTTP_RESP_CODE(NON_PERSISTABLE),
    
    NP_HTTP_RESP_MSG(NON_PERSISTABLE),
    
    NP_HTTP_REQ_HDRS(NON_PERSISTABLE),
    
    NP_HTTP_RES_HDRS(NON_PERSISTABLE),
    
    NP_HTTP_REQ_METHOD(NON_PERSISTABLE),
    
    NP_HTTP_REQ_PARAMS(NON_PERSISTABLE),
	/*
     * Below fields are added for ISUP
	 */
    NP_RELEASE_LOCATION(PERSISTABLE),  //FIXME: Remove NP from EnumName
    P_FWD_CALL_IND(PERSISTABLE),
    P_DPC_INFO(PERSISTABLE),
    P_ADD_CPC_1(PERSISTABLE),
    P_ADD_CPC_2(PERSISTABLE),
    P_ADD_PSTNC(PERSISTABLE),
    P_TMR(PERSISTABLE),
    P_GENERIC_NUM(PERSISTABLE),
    P_TTC_CALLED_IN_NUMBER(PERSISTABLE),
    P_IAM_CALLING_NUM(PERSISTABLE),
    P_IAM_CALLED_NUM(PERSISTABLE),
    P_CALLED_LS_SUB_IND(PERSISTABLE),
    P_TERM_BYPASS_IND(PERSISTABLE),
    P_TERM_IGS_IND(PERSISTABLE),
    P_CALLED_MEM_STS_IFO(PERSISTABLE),
    P_CHRG_INTERVAL(PERSISTABLE),
    P_CHARGE_AREA(PERSISTABLE),
    P_CALLED_SUB_ADDRESS(PERSISTABLE),
    P_FLEXI_CHRG_APPLIED(PERSISTABLE),
    P_SERVICE_IND(PERSISTABLE),
    P_ISUP_CONTENT_TYPE(PERSISTABLE),
    /*
     * Following enhancements has been done for MPH1.0
     */
    P_ORIG_TRUNK_GROUP(PERSISTABLE),
    P_MC_CONTACTS_LIST(PERSISTABLE),
    P_DEST_TRUNK_GROUP(PERSISTABLE),
    NP_SIG_INFO(NON_PERSISTABLE),
    P_ORIG_CONTACT_URI(PERSISTABLE),
    P_ORIG_LINE_INFO(PERSISTABLE),
    P_DEST_TGRP(PERSISTABLE),
    P_DEST_TRUNK_CONTEXT(PERSISTABLE),
    P_CALL_RINGING_TIME(PERSISTABLE),
    P_CALL_MS_CONNECT_TIME(PERSISTABLE),
    P_TRANSFERED_CALL_ID(PERSISTABLE),
    P_TRANSFERED_CALL_IND(PERSISTABLE),
    P_MC_CONTACT(PERSISTABLE),
    // Added for Call Queueing
    P_DQ_CALLS_APP_ID(PERSISTABLE),
    NP_MS_OP_STOP_INPROG(NON_PERSISTABLE),
    NP_IS_CONSULTED_TRANSFER(NON_PERSISTABLE),
    NP_TRANSFERED_PENDING_INVITE_REQ(NON_PERSISTABLE),
    NP_TRANSFERED_SESSION(NON_PERSISTABLE),
    NP_TRANSIENT_CALL(NON_PERSISTABLE),
    /*
     * the  dialled number  modified by the application
     */
    P_MODIFIED_CALLING_NUMBER(PERSISTABLE),
    P_MODIFIED_CALLING_DISPLAY_NAME(PERSISTABLE),
    /*
     * DPs armed by service.
     */
    P_ERB_SET(PERSISTABLE),
    @Deprecated
    FCI_DEST_NUMBER(PERSISTABLE),
    @Deprecated
    FCI_ORIGIN_NUMBER(PERSISTABLE),
    P_CALLING_PARTY(PERSISTABLE),
    P_CONTRACTOR_NUMBER(PERSISTABLE),
    SSD_KEY(PERSISTABLE),
    P_APP_SPECIFIC_DATA(PERSISTABLE),
    NP_APP_SPECIFIC_DATA(NON_PERSISTABLE),
    P_CHARGE_VECTOR(PERSISTABLE),
    P_CHARGE_VECTOR_SUCCESS(PERSISTABLE),
    P_PROV_WO_SDP_NOTIFIED(PERSISTABLE),
    P_MS_ID(PERSISTABLE), 
    P_ISUP_CPC(PERSISTABLE),
    P_REINVITE_SENT_TO_TERM(PERSISTABLE),
    NP_ORIG_BYE_ON_IVR(NON_PERSISTABLE),
    SERVICE_REQUESTED_ACTION_ARRAY(NON_PERSISTABLE),
    RESYNCH_LEG_ID(PERSISTABLE),
    FROM_URI(NON_PERSISTABLE),
    TO_URI(PERSISTABLE), 
    CALL_WAITING_ENABLED(PERSISTABLE), 
    AUTH_PASSWORD(PERSISTABLE),
    CALL_PICKUP_DATA(PERSISTABLE), 
    APP_SESSION_ID(PERSISTABLE),
    PROMPT_COLLECT_INVOKE_ID(PERSISTABLE),
    IS_FCI_SENT(PERSISTABLE),
    FCI(PERSISTABLE),
	SSD_KEY_ORIGAPP(NON_PERSISTABLE),
    SSD_KEY_VPN(PERSISTABLE),
    SSD_KEY_LNP(PERSISTABLE),
    SSD_KEY_ATF(PERSISTABLE),
    SSD_KEY_TC(NON_PERSISTABLE),
    SSD_KEY_AXVPN(PERSISTABLE),
    ADDRESS_MAP(PERSISTABLE), 
	
	SERVICE_ID(PERSISTABLE), 
	PREV_SERVICE_ID(PERSISTABLE),
	MAX_CALL_DURATION(PERSISTABLE),
	CORRELATION_KEY(PERSISTABLE), 
	P_SCCP_REMOTE_USER_ADDRESS(PERSISTABLE), 
	P_CORRELATION_TOKEN(PERSISTABLE),
	P_DISCONNECT_LEG_INVOKE_ID(PERSISTABLE),
	
	P_NETWORK_TRANSACTION(PERSISTABLE), // To calculate TPS per call
    
    P_SVC_CHAINING_MAP(PERSISTABLE),
    
    NP_ETC_SEND_FAILURE(NON_PERSISTABLE),
    
    P_DIALOUT(PERSISTABLE),
    
    P_LAST_OPERATION_RX(PERSISTABLE),
    
    NP_CM_CALL_ALLOWED(NON_PERSISTABLE),
   // There could be multiple event rxed in Begin. 
   // Depending on last Operation Received, the response should be framed
    
	P_HTTP_REMOTE_HOST(PERSISTABLE), 
	P_HTTP_REMOTE_PORT(PERSISTABLE), 
	P_HTTP_REQ_SENT_TIME(PERSISTABLE), 
	P_HTTP_RES_RECIEVED_TIME(PERSISTABLE),
	P_HTTP_REQ_RECEIVED_TIME(PERSISTABLE), 
	P_HTTP_RES_SENT_TIME(PERSISTABLE), 
	NP_HTTP_REQ_URL(NON_PERSISTABLE),
	NP_INCOMING_HTTP_REQ(NON_PERSISTABLE), 
	NP_OUTGOING_HTTP_RES(NON_PERSISTABLE), 
	NP_OUTGOING_HTTP_REQ(NON_PERSISTABLE),
    NP_INCOMING_HTTP_RES(NON_PERSISTABLE),
    
    // This flag is used in AIN in order to send Pre-arranged NULL
    // so that stack can clean up dialogue locally. Required in case of
    // call  getting cleaned up through UNI/TerminationNotification
    P_SEND_PREARRANGED_END(PERSISTABLE),
	P_AIN_CONNECT_TIME(PERSISTABLE),                  // value rx in termination notification of AIN
	P_AIN_TERMINATION_IND(PERSISTABLE),               // value rx in termination notification of AIN;
	P_AIN_ECHO_DATA(PERSISTABLE),					  // Echo data rxed in termination notification
    //BNS_APPLICATION_ERROR
    P_BNS_APP_ERROR(PERSISTABLE),
    P_BNS_PROTOCOL_ERROR(PERSISTABLE),
    
 // for Triggering Criteria Rule
    P_TC_POINT_CODE(PERSISTABLE),
	P_TC_CALLING_PARTY(PERSISTABLE),
	P_TC_CALLED_PARTY(PERSISTABLE),
	P_TC_SSN(PERSISTABLE), 
	P_TC_CIC(PERSISTABLE), 
	P_TC_TRUNK_GROUP(PERSISTABLE),
	P_TC_SERVICE_KEY(PERSISTABLE),
	P_TC_OP_CODE(PERSISTABLE),
	P_TC_PROTOCOL(PERSISTABLE),
	P_TC_TRIGGERING_CRITERIA(PERSISTABLE), 
	
	// Ain Termination Notification params
	P_AIN_TN_CONNECT_TIME_MIN(PERSISTABLE), 
	P_AIN_TN_CONNECT_TIME_SEC(PERSISTABLE), 
	P_AIN_TN_CONNECT_TIME_TENTH_SEC(PERSISTABLE), 
	P_AIN_TN_BUSY_CAUSE(PERSISTABLE), 
	NP_CONTACT_302_USER(NON_PERSISTABLE),  
    NP_DIALOUT_PARTY(NON_PERSISTABLE), 
    
    P_OUTGOING_LAST_INVOKE_ID_RANGE_END(PERSISTABLE),
	P_OUTGOING_LAST_INVOKE_ID_RANGE_START(PERSISTABLE),
	P_CHARGING_VECTOR_ORIG_IOI(PERSISTABLE),
	P_CHARGING_VECTOR_TERM_IOI(PERSISTABLE),
    P_ISUP_CAUSE_CODE(PERSISTABLE),
    P_ISUP_LEG1_IAM_MESSAGE(PERSISTABLE),
    P_MULTIPLE_COMPONENT_COUNT(PERSISTABLE),
    P_MULTIPLE_COMPONENT_CUR_COUNT(PERSISTABLE),
    P_AIN_TRIGGERING_MESSAGE(PERSISTABLE),
    // ATI
    ATI_INVOKE_ID(PERSISTABLE), 
    P_SCCP_CALLED_USER_GTI(NON_PERSISTABLE), P_CALL_DROPPED(NON_PERSISTABLE);
    private PersistanceType persistanceType;
    CallDataAttribute(PersistanceType persistanceType) {
        this.persistanceType = persistanceType;
    }
    public PersistanceType getPersistanceType() {
        return this.persistanceType;
    }
}