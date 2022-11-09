/*******************************************************************************
 *   Copyright (c) 2020 Agnity, Inc. All rights reserved.
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
package com.agnity.ph.Enum;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolHandler;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.ra.enumserver.message.EnumRequest;
/**
 * This Class is being used for sending/receiving Enum request/res
 * @author reeta
 *
 */
public class EnumProtocolHandler implements ProtocolHandler {

	private static final EnumProtocolHandler INSTANCE = new EnumProtocolHandler();
	private static Logger logger = Logger
			.getLogger(EnumProtocolHandler.class);

	private EnumProtocolHandler() {
	}

	/**
	 * This method is used to process/send diameter RO/RF protocol requests
	 */
	@Override
	public void executeAction(CallData callData, Action action)
			throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("::executeAction:" + action.getActionType().name());
		}
		String serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);

		SipApplicationSession appSession = SipProtocolUtil.getAppSession(
				(String) callData.get(CallDataAttribute.P_APP_SESSION_ID),
				serviceId);
		action.setProtocol(Protocol.ENUM);
		switch (action.getActionType()) {

		case ACTION_SEND_ENUM_QUERY : {
			EnumProtocolHelper.sendEnumQuery(appSession, callData, action);
			break;
		}
		case ACTION_SEND_ENUM_RESPONSE : {
			  EnumProtocolHelper.sendResponse(appSession, callData, action);
			   break;
		     }
		case ACTION_END_CALL : {
			  EnumProtocolHelper.dropCall(appSession, callData, action);
			   break;
		     }
		default:{
			if (logger.isDebugEnabled()) {
				logger.debug("::executeAction: unexpected action");
			}
		}
		break;

		}

	}

	@Override
	public void timeout(ServletTimer timer) {
		// TODO Auto-generated method stub
	}

	public static ProtocolHandler getInstance() {
		return INSTANCE;
	}

	/**
	 * This method is used to handle incoming enum request
	 * @param callData
	 * @param enumRequest
	 * @param serviceHandler
	 * @throws Exception
	 */
	public void handleRequest(CallData callData, EnumRequest enumRequest,
			ServiceInterface serviceHandler) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("::handleRequest  EnumRequest:");
		}
		SipApplicationSession appSession = enumRequest.getApplicationSession() ;
		//		SipProtocolUtil.getAppSession(
		//				(String) callData.get(CallDataAttribute.P_APP_SESSION_ID),
		//				(String) callData.get(CallDataAttribute.SERVICE_ID));
		callData.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(0));

		appSession.setAttribute(CallData.CALL_DATA, callData);

		LegData legData = new LegData();
		legData.set(LegDataAttributes.ENUM_REQUEST_RECEIVED, enumRequest);
		legData.set(LegDataAttributes.ENUM_KEY ,enumRequest.getkey());
		legData.set(LegDataAttributes.ENUM_AUS ,enumRequest.getAUS());
		callData.set(CallDataAttribute.P_PROTOCOL ,Protocol.ENUM);

		callData.set(CallDataAttribute.P_LEG1, legData);

		if (logger.isDebugEnabled()) {
			logger.debug("::Notify Service:");
		}
		Event event = new Event(EventType.EVENT_INITIAL, Protocol.ENUM,
				CallDataAttribute.P_LEG1.name());
		ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

	}

	/**
	 * This method is used to handle incoming enum response
	 * @param calldata
	 * @param list
	 * @throws Exception
	 */
	public void handleResponse(CallData calldata, List list) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("::handleResponse  EnumResponse:");
		}
		List<String> listofAddress = new ArrayList<String>();
		String number= null;
		for (Object object : list) {
			String str =(String) object;
			number=	str.substring(str.lastIndexOf(".")+1);
			if(logger.isDebugEnabled()){
				logger.debug("Adding value in Number in List::"+number);
			}
			if(number !=null)
				listofAddress.add(number);

		}
		//		SipApplicationSession appSession = enumRequest.getApplicationSession() ;
		////		SipProtocolUtil.getAppSession(
		////				(String) callData.get(CallDataAttribute.P_APP_SESSION_ID),
		////				(String) callData.get(CallDataAttribute.SERVICE_ID));
		//		callData.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(0));
		//
		//		appSession.setAttribute(CallData.CALL_DATA, callData);
		//
		//		LegData legData = new LegData();
		//		legData.set(LegDataAttributes.ENUM_REQUEST_RECEIVED, enumRequest);
		//		legData.set(LegDataAttributes.ENUM_KEY ,enumRequest.getkey());
		//		legData.set(LegDataAttributes.ENUM_AUS ,enumRequest.getAUS());
		//		
		//		callData.set(CallDataAttribute.P_LEG1, legData);

		if (logger.isDebugEnabled()) {
			logger.debug("::Notify Service:");
		}
		LegData leg1 = (LegData) calldata
				.get(CallDataAttribute.P_LEG1);

		if(calldata.get(CallDataAttribute.P_PROTOCOL) == null){
			calldata.set(CallDataAttribute.P_PROTOCOL ,Protocol.ENUM);
		}
		leg1.set(LegDataAttributes.ENUM_RESULT_LIST,listofAddress);
		ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)calldata.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		Event event = new Event(EventType.EVENT_ENUM_RESPONSE_RECEIVED, Protocol.ENUM,
				CallDataAttribute.P_LEG1.name());
		ProtocolRouter.getInstance().execute(event, calldata, serviceHandler);

	}


}
