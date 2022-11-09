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
import com.agnity.ph.diameter.AVPAttributes_NotUsed;
import com.agnity.ph.diameter.CCRAVPAttributes_NotUsed;
import com.agnity.ph.http.HttpEvent;
/**
 * 
 * This class defines the various events that are possible at PH and/or at
 * application . For E.g EVENT_INITIAL is used whenever PH receives SIP INVITE
 * or INAP IDP or CAP IDP
 *
 */
public class Event implements Serializable {
	public enum EventType {
		EVENT_INITIAL, EVENT_SUCCESS, EVENT_FAILURE, 
		EVENT_DISCONNECT, EVENT_RESYNC_SUCCESS, EVENT_SESSION_EXPIRED,
		EVENT_DTMF, EVENT_MS_SUCCESS, EVENT_MS_FAILURE, EVENT_PLAY_SUCCESS, 
		EVENT_PLAY_FAILURE, EVENT_PNC_SUCCESS, EVENT_PNC_FAILURE, EVENT_PNR_SUCCESS,
		EVENT_PNR_FAILURE, EVENT_MS_DISCONNECT, EVENT_CALL_DROPPED, EVENT_FAILOVER,
		EVENT_OPR_FAIL, EVENT_OPR_SEND_FAIL, EVENT_CHRG, EVENT_CALL_HB_SUCCESS,
		EVENT_CALL_HB_FAIL, EVENT_REDIRECT, EVENT_HTTP_REQ_RECEIVED, EVENT_HTTP_RESP_RECEIVED ,EVENT_TRANSFER,
		EVENT_CALL_TRANSFER_DISCONNECT, EVENT_TRANSFER_SUCCESS, EVENT_TRANSFER_FAILURE, 
		EVENT_TRANSFER_REJECTED, EVENT_ROUTE_NEXT, EVENT_ROUTE_TERMINATE, 
		EVENT_GW_NOT_FOUND, EVENT_APP_TIMER_TIMEOUT,EVENT_NEXT_CALL_TIMER_TIMEOUT, EVENT_SDP_RECEIVED, 
		EVENT_PROVISIONAL_WO_SDP, EVENT_REDIRECTION_COMPLETED,EVENT_NO_NEXT_SERVICE,EVENT_SERVICE_CHAIN_INVOKED,EVENT_PROVISIONAL_RECEIVED,EVENT_TRANSFER_INITIATED,
        EVENT_HOLD_SUCCESS, EVENT_CONF_SUCCESS, EVENT_CONF_FAILURE ,EVENT_RECORD_SUCCESS, EVENT_RECORD_FAILURE, EVENT_MS_OPERATION_STOPPED, EVENT_HTTP_REQ_FAILED,
		EVENT_HTTP_RESP_TIMER_TIMEOUT,EVENT_APP_TIMER_STOPPED,EVENT_PLAY_IN_PROGRESS, EVENT_APP_TIMER_STARTED, EVENT_HTTP_RESP_SENT, EVENT_HTTP_REQ_SENT, 
		EVENT_HTTP_ERR_RES_SENT, EVENT_ENUM_QUERY_FAILURE, EVENT_ENUM_QUERY_SUCCESS, EVENT_CCR_EVENT, EVENT_CCR_INITIAL, EVENT_CCR_INTERIM, EVENT_CCR_TERMINATION,
		EVENT_APPLY_CHG_REPORT,
		EVENT_AR_EVENT, EVENT_AR_START_RECORD, EVENT_AT_INTERIM_RECORD, EVENT_ENUM_RESPONSE_SENT, EVENT_CCA_SENT, EVENT_CCA_RECEIVED, EVENT_ENUM_RESPONSE_RECEIVED,
		EVENT_TERM_NOTIFICATION, EVENT_SEND_MESSAGE_FAILURE, EVENT_SHORT_MESSGE_SENT, EVENT_SHORT_MESSAGE_RESPONSE_RECEIVED,EVENT_SHORT_MESSAGE_DELIVERED,EVENT_SHORT_MESSAGE_DELIVER_WAIT_TIMEOUT,
		EVENT_SHORT_MESSAGE_SUBMIT_WAIT_TIMEOUT,EVENT_SHORT_MESSAGE_RETRY_LOGIC_FAILURE,
		EVENT_SH_SEND_USER_REQUEST_FAILURE, EVENT_SH_USER_DATA_RESPONSE_RECEIVED, EVENT_SHORT_MESSAGE_RECEIVED, EVENT_ATI_RESULT_RECEIVED, EVENT_ATI_FAILURE, 
		EVENT_SERVICE_CHAINING_INVOKE_SUCCESS, EVENT_SERVICE_CHAINING_INVOKE_FAILURE,EVENT_PROCESS_NEXT_COMPONENT,
		EVENT_GDI_SEND_REQUEST_FAILURE, EVENT_GDI_RESPONSE_RECEIVED
	}
	public Event(EventType eventType, Protocol protocol, String leg) {
		this.eventType = eventType;
		this.protocol = protocol;
		this.leg = leg;
	}
	private EventType eventType;
	private String leg;
	private Protocol protocol;
	
	private String timerName;
	
	private HttpEvent httpEvent;
	
	private AVPAttributes_NotUsed avps;
	public HttpEvent getHttpEvent() {
		return httpEvent;
	}
	public void setHttpEvent(HttpEvent httpEvent) {
		this.httpEvent = httpEvent;
	}
	public String getTimerName() {
		return timerName;
	}
	public void setTimerName(String timerName) {
		this.timerName = timerName;
	}
	public EventType getEventType() {
		return eventType;
	}
	public void setEventType(EventType eventType) {
		this.eventType = eventType;
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
	@Override
	public String toString() {
		return "Event [eventType=" + eventType + ", leg=" + leg + ", protocol="
				+ protocol + "]";
	}
	public void setAVPAttributes(AVPAttributes_NotUsed avps) {
		this.avps=avps;	
	}
	
	public AVPAttributes_NotUsed getAVPAttributes(){
		return avps;
	}
}