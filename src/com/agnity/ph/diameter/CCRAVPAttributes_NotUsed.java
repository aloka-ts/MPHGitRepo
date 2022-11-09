/*****
Copyright (c) 2013 Agnity, Inc. All rights reserved.  

This is proprietary source code of Agnity, Inc.  

Agnity, Inc. retains all intellectual property rights associated  with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.

Confidentiality of this computer program must be maintained at all times, unless explicitly authorized by Agnity, Inc.
*****/

package com.agnity.ph.diameter;

import java.util.HashMap;
import java.util.Map;
//import com.traffix.openblox.diameter.ro.generated.enums.EnumSubscriptionIdType;

/**
 * @author Reeta
 *
 */
public class CCRAVPAttributes_NotUsed implements AVPAttributes_NotUsed {

	private Map<String, Object> persitableData = new HashMap<String, Object>();
	private static final long serialVersionUID = 491012216192015185L;

	public static final String sessionId_str="sessionId_str";
	public static final String originHost_str="originHost_str";
	public static final String originRealm_str="originRealm_str";
	public static final String destnRealm_str="destnRealm_str";
	public static final String destnHost_str="destnHost_str";
	public static final String requestNo_long="requestNo_long";
	public static final String authAppId_long="authAppId_long";
	public static final String userName_str="userName_str";
	public static final String originStateId_long="originStateId_long";
	public static final String eventTimestamp_date="eventTimestamp_date";
	public static final String serviceCtxtId_str="serviceCtxtId_str";
	public static final String subsIdMap="subsIdMap";// HashMap<SubscriptionIdTypeEnum, String>
	public static final String  terminationCause_int="terminationCause_int";
	public static final String  requestedAction_int="requestedAction_int";
	public static final String  multipleServicesIndicator_int="multipleServicesIndicator_int";
	public static final String  serviceIdentifier_long="serviceIdentifier_long";
	public static final String  ratingGroup_long="ratingGroup_long";
	public static final String  ccTimeRequested_long="ccTimeRequested_long";
	public static final String  ccTotalOctets_long="ccTotalOctets_long";
	public static final String  ccTimeUsed_long="ccTimeUsed_long";
	public static final String  reportingReasonUsed_int="reportingReasonUsed_int";
	public static final String  reportingReason_int="reportingReason_int";
	public static final String  triggerType_int="triggerType_int";
	public static final String  roleOfNode_int="roleOfNode_int";
	public static final String userSessionId_str="userSessionId_str";
	public static final String cgpn_str="cgpn_str";
	public static final String cdpn_str="cdpn_str";
	public static final String calledAssertedId_str="calledAssertedId_str";
	public static final String chargingId_str="chargingId_str";
	public static final String origIOI_str="origIOI_str";
	public static final String termIOI_str="termIOI_str";
	public static final String subscriptionId_str="subscriptionId_str";
	public static final String sipRequestTimestamp_date="sipRequestTimestamp_date";
	public static final String sipResponseTimestamp_date="sipResponseTimestamp_date";
	public static final String aocRequestType_int="aocRequestType_int";
	
	
	public void setAttribute(String avpAttributeName,Object value){
		persitableData.put(avpAttributeName, value);
		
	}
	
	public Object getAttribute(String avpAttributeName){
		return persitableData.get(avpAttributeName);
		
	}
} // end-of-class
