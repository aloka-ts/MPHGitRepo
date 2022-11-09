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
package com.agnity.mphdata.common;
import java.io.Serializable;
import com.agnity.ph.http.HttpEvent;
/**
 * This Class defines the primitive that is used by the PH and Application to
 * pass the relevant action to be executed.
 */
public class Action implements Serializable {
	private static final long serialVersionUID = -8137299546758193620L;
	boolean addCustomHeader;
	private ActionType actionType;
	private String leg;
	private Protocol protocol;
	private DROP_CALL_MODE dropCallMode = DROP_CALL_MODE.NONE;
	private CONNECTIONMODE connectionMode = CONNECTIONMODE.NONE;
	private CONTINUE_MODE continueMode = CONTINUE_MODE.NONE;
	private SEND_MODE inapSendMode = SEND_MODE.NONE;
	private int releaseCauseValue; // releaseCauseValue: INAP/ISUP Release Cause Value returned by services to
									// protocol handler
	private long timerTimeInMills;
	private String timerName;
	private boolean forcedFciFlag; // This flag is used by an application
									// to notify mPH to send FCI forcefully if set in call data
	private boolean sendDialogue;
	private int sendDfcInSeparateDlg = -1; // Application uses this flag to decide whether to send DFC
											// in separate Dialogue or not.
	private boolean invokeServiceChaining; // Application uses this flag to decide whether to invoke
											// ServiceChaining or not.
	private boolean dropCallOnNoNextSvc; // Application uses this flag to decide whether to drop call on No next service
											// or not
	private boolean errorInSendingEtc; // This flag is used to make sure that mPH doesn't send DFC in case there
										// was an error in sending ETC - due to no free PRI Number available etc.
	private boolean isServiceComplete; // This flag specify whether action is received from service
										// ans it has completed call processing from event perspective.
										// mPH sets it true in END_CALL. Based on this flag mPH decides
										// not to generate any further events.
	private boolean sendCallingParty = true; // This flag specify whether to send callingParty in CONNECT msg or not.
												// By Default it is true, applications need to set it false, if not
												// required.
	private boolean isMsDiconnectedForAinCall; // This flag is specific to AIN protocol. In case of AIN
												// MS_DISCONNECTED should be notified to application on Resource clean
												// an not at MS_DISCONNECTED in MediaEventListener
	private boolean isNewHttpSession;
	
	
	private String nextAppId;
	
	
	public String getNextAppId() {
		return nextAppId;
	}
	public void setNextAppId(String nextAppId) {
		this.nextAppId = nextAppId;
	}
	private HttpEvent httpReqSendEvent;
	private HttpEvent httpResSendEvent;
	public HttpEvent getHttpResSendEvent() {
		return httpResSendEvent;
	}
	public void setHttpResSendEvent(HttpEvent httpResSendEvent) {
		this.httpResSendEvent = httpResSendEvent;
	}
	public HttpEvent getHttpReqSendEvent() {
		return httpReqSendEvent;
	}
	public void setHttpReqSendEvent(HttpEvent httpReqSendEvent) {
		this.httpReqSendEvent = httpReqSendEvent;
	}
	public boolean isNewHttpSession() {
		return isNewHttpSession;
	}
	public void setNewHttpSession(boolean isNewHttpSession) {
		this.isNewHttpSession = isNewHttpSession;
	}
	public boolean isSendCallingParty() {
		return sendCallingParty;
	}
	public void setSendCallingParty(boolean sendCallingParty) {
		this.sendCallingParty = sendCallingParty;
	}
	public boolean isDropCallOnNoNextSvc() {
		return dropCallOnNoNextSvc;
	}
	public void setDropCallOnNoNextSvc(boolean dropCallOnNoNextSvc) {
		this.dropCallOnNoNextSvc = dropCallOnNoNextSvc;
	}
	public Action(ActionType actionType) {
		this.actionType = actionType;
	}
	public static long getSerialversionuid() {
		return serialVersionUID;
	}
	/*
	 * Event for which action is performed
	 */
	private Event event;
	/**
	 * when service calls serviceComplete and this flag is set by service to tell if
	 * next service should be invoked
	 * 
	 */
	private boolean notifyPrevService;
	/**
	 * This is same name as returned by ServiceInterface.getApplicationName(). this
	 * should be specified while calling serviceComplete or writeCDR in case of
	 * service chaining
	 */
	private String applicationName;
	private String httpUrl;
	public int getSendDfcInSeparateDlg() {
		return sendDfcInSeparateDlg;
	}
	public void setHttpUrl(String httpUrl) {
		this.httpUrl = httpUrl;
	}
	public String getHttpUrl() {
		return this.httpUrl;
	}
	public String getApplicationName() {
		return applicationName;
	}
	public void setApplicationName(String applicationName) {
		this.applicationName = applicationName;
	}
	public boolean isNotifyPrevService() {
		return notifyPrevService;
	}
	public void setNotifyPrevService(boolean invokeNextService) {
		this.notifyPrevService = invokeNextService;
	}
	public boolean isRemainInPath() {
		return remainInPath;
	}
	public void setRemainInPath(boolean remainInPath) {
		this.remainInPath = remainInPath;
	}
	private boolean remainInPath = true;
	private String httpReqMethod = "POST";
	public String getHttpReqMethod() {
		return httpReqMethod;
	}
	public void setHttpReqMethod(String httpReqMethod) {
		this.httpReqMethod = httpReqMethod;
	}
	public Event getEvent() {
		return event;
	}
	public void setEvent(Event event) {
		this.event = event;
	}
	public boolean isAddCustomHeader() {
		return addCustomHeader;
	}
	public void setAddCustomHeader(boolean addCustomHeader) {
		this.addCustomHeader = addCustomHeader;
	}
	public SEND_MODE getSendMode() {
		return inapSendMode;
	}
	public void setSendMode(SEND_MODE sendMode) {
		this.inapSendMode = sendMode;
	}
	public SEND_MODE getInapSendMode() {
		return inapSendMode;
	}
	public String getTimerName() {
		return timerName;
	}
	public void setTimerName(String timerName) {
		this.timerName = timerName;
	}
	public long getTimerTimeInMills() {
		return timerTimeInMills;
	}
	public void setTimerTimeInMills(long timerTimeInMills) {
		this.timerTimeInMills = timerTimeInMills;
	}
	public ActionType getActionType() {
		return actionType;
	}
	public void setActionType(ActionType actionType) {
		this.actionType = actionType;
	}
	public String getLeg() {
		return leg;
	}
	public void setLeg(String leg) {
		this.leg = leg;
	}
	public Protocol getProtocol() {
		return protocol;
	}
	public void setProtocol(Protocol protocol) {
		this.protocol = protocol;
	}
	public DROP_CALL_MODE getDropCallMode() {
		return dropCallMode;
	}
	public void setDropCallMode(DROP_CALL_MODE value) {
		dropCallMode = value;
	}
	public CONNECTIONMODE getConnectionMode() {
		return connectionMode;
	}
	public void setConnectionMode(CONNECTIONMODE connectionMode) {
		this.connectionMode = connectionMode;
	}
	public CONTINUE_MODE getContinueMode() {
		return continueMode;
	}
	public void setContinueMode(CONTINUE_MODE continueMode) {
		this.continueMode = continueMode;
	}
	public boolean isForcedFciFlag() {
		return forcedFciFlag;
	}
	public void setForcedFciFlag(boolean forcedFciFlag) {
		this.forcedFciFlag = forcedFciFlag;
	}
	public int dfcToBeSentInSeperateDialogue() {
		return sendDfcInSeparateDlg;
	}
	public void setDfcToBeSentInSeperateDialogue(int dfcFlag) {
		this.sendDfcInSeparateDlg = dfcFlag;
	}
	public void setInvokeServiceChaining(boolean invokeServiceChaining) {
		this.invokeServiceChaining = invokeServiceChaining;
	}
	public boolean isInvokeServiceChaining() {
		return invokeServiceChaining;
	}
	@Override
	public String toString() {
		return "Action{" + "addCustomHeader=" + addCustomHeader + ", actionType=" + actionType + ", leg='" + leg + '\''
				+ ", protocol=" + protocol + ", dropCallMode=" + dropCallMode + ", connectionMode=" + connectionMode
				+ ", continueMode=" + continueMode + ", inapSendMode=" + inapSendMode + ", releaseCauseValue="
				+ releaseCauseValue +
				// ", releaseCauseText='" + releaseCauseText + '\'' +
				", timerTimeInMills=" + timerTimeInMills + ", timerName='" + timerName + '\'' + ", forcedFciFlag="
				+ forcedFciFlag + ", sendDfcInSeparateDlg=" + sendDfcInSeparateDlg + ", sendDialogue=" + sendDialogue
				+ ", invokeServiceChaining=" + invokeServiceChaining + ", errorInSendingEtc=" + errorInSendingEtc
				+ ", httpResSendEvent=" + httpResSendEvent + ", httpReqSendEvent=" + httpReqSendEvent + ", httpUrl="
				+ httpUrl + '}';
	}
	/**
	 * @return
	 */
	public int getReleaseCauseValue() {
		return releaseCauseValue;
	}
	/**
	 * @param releaseCauseValue
	 */
	public void setReleaseCauseValue(int releaseCauseValue) {
		this.releaseCauseValue = releaseCauseValue;
	}
	public boolean isSendDialogue() {
		return sendDialogue;
	}
	public void setSendDialogue(boolean sendDialogue) {
		this.sendDialogue = sendDialogue;
	}
	public boolean isErrorInSendingEtc() {
		return errorInSendingEtc;
	}
	public void setErrorInSendingEtc(boolean errorInSendingEtc) {
		this.errorInSendingEtc = errorInSendingEtc;
	}
	public boolean isServiceComplete() {
		return isServiceComplete;
	}
	public void setServiceComplete(boolean isServiceComplete) {
		this.isServiceComplete = isServiceComplete;
	}
	public boolean isMsDiconnectedForAinCall() {
		return isMsDiconnectedForAinCall;
	}
	public void setMsDiconnectedForAinCall(boolean value) {
		this.isMsDiconnectedForAinCall = value;
	}
	/**
	 * Each action represents a single action to be executed by PH /application
	 */
	/**
	 * Each action represents a single action to be executed by PH /application
	 */
	public enum ActionType {
		ACTION_CONNECT_MS, ACTION_DISCONNECT_MS, ACTION_PLAY, ACTION_PLAY_COLLECT, ACTION_PLAY_RECORD, ACTION_CONNECT,
		ACTION_REDIRECT, ACTION_DISCONNECT, ACTION_END_CALL, ACTION_PROCESS_CALLS, ACTION_RESYNC_CALL, ACTION_HOLD_CALL,
		ACTION_LS_CMD, ACTION_HTTP_REQ, ACTION_SEND_HTTP_RES, ACTION_CONTINUE, ACTION_CHRG, ACTION_CALL_HB,
		ACTION_RESET_TMR, ACTION_INTERROGATE, ACTION_INTERROGATE_SUBS, ACTION_MODIFY_INFO, ACTION_SEND_ROUTING_INFO,
		ACTION_REJECT_REQUEST, ACTION_SEND_REQ_RESULT, ACTION_TRY_REDIRECT_CONTACTS, ACTION_ALLOW_TRANSFER,
		ACTION_REJECT_TRANSFER, ACTION_TRANSFER_CONNECT, ACTION_START_TIMER, ACTION_STOP_TIMER, ACTION_PROCESS_NEXT,
		ACTION_NONE, ACTION_STOP_MS_OPERATION, ACTION_DTMF_TRANSFER_CONNECT, ACTION_RESYNCH_LEGS,
		ACTION_CONNECT_PARALLEL, ACTION_CONNECT_SERIAL, ACTION_PICKUP_CALL, ACTION_SERVICE_COMPLETE,
		ACTION_INVOKE_SVC_CHAINING, ACTION_WRITE_CDR, ACTION_CREATE_CONF, ACTION_RECORD, ACTION_SEND_HTTP_ERROR,
		ACTION_SEND_HTTP_REQ, ACTION_SEND_CCR_EVENT, ACTION_SEND_CCR_INITIAL, ACTION_SEND_CCR_UPDATE,
		ACTION_SEND_CCR_TERMINATE, ACTION_SEND_AR_EVENT_BASED_REQ, ACTION_SEND_AR_SESSION_BASED_REQ,
		ACTION_SEND_ENUM_QUERY, ACTION_SEND_ENUM_RESPONSE,ACTION_PSX_ROUTING, ACTION_SEND_CCR_ANSWER,
		APP_FOR_TC_NOT_FOUND,APP_FOR_TC_FOUND, ACTION_SEND_SHORT_MESSAGE, ACTION_DO_REPLICATION ,ACTION_SH_SEND_USER_DATA_REQUEST,
		ACTION_GDI_REQUEST,	ACTION_FORCE_CALL_CLEANUP
	}
	public static enum CONNECTIONMODE {
		NONE, ASSIST, B2BUA, EQSROUTING, REDIRECTION, REROUTING, PORTROUTING, CONTINUE, DIRECT_MS, PROXY_STATELESS,MRS_RELAY,
		PROXY_STATEFULL
	}
	/**
	 * This enum is used to identify the event report BCSM events
	 */
	public static enum ERB_TYPE {
		ERB_ROUTESELECTFAILURE, ERB_BUSY, ERB_NO_ANSWER, ERB_ANSWER, ERB_DISCONNECT, ERB_ABANDON, ERB_TERMSEIZED,
		ERB_MIDCALL, ERB_NETWORK_BUSY
	}
	/**
	 * External query service type this is used for redirect server scenario in
	 * which 300 MC is received with multiple contacts so in case of routing failure
	 * from any of 300Mc contacts following actions can be taken as per response
	 * codes
	 *
	 * @author reeta
	 */
	public static enum EQS_TYPE {
		TRY_NEXT_CONTACT, TRY_NEXT_ROUTE, TERMINATE
	}
	/*
	 * Send mode is added to check if outgoing inap/cap message will go in END or
	 * continue dialog
	 */
	public static enum SEND_MODE {
		END, CONTINUE, NONE
	}
	/**
	 * DROP Call Mode represents how the call was disconnected
	 */
	public static enum DROP_CALL_MODE {
		NONE, RELEASE_CALL, INITIAL_ERROR, USER_ABORT, USER_REJECT, NULL_END, NULL_END_PREARRANGED, DISCONNECT_TERM_LEG,
		RELEASE_CALL_WITH_STR, BNS_APPLICATION_ERROR, BNS_PROTOCOL_ERROR, GN_APPLICATION_ERROR, GN_PROTOCOL_ERROR,
		OLNS_APPLICATION_ERROR, OLNS_PROTOCOL_ERROR, LIDB_PROTOCOL_ERROR, CANCEL_NULL_END_PREARRANGED, CONTINUE
	}
	/**
	 * This is used in conjuction with TC_CONTINUE
	 */
	public static enum CONTINUE_MODE {
		NONE, INITIAL_ERROR, USER_REJECT, ENTITY_RELEASE
	}
}