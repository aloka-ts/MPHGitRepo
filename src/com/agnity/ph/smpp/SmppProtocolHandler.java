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
package com.agnity.ph.smpp;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
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
import com.baypackets.ase.ra.smpp.SmppRequest;
import com.baypackets.ase.ra.smpp.SmppResourceException;
import com.baypackets.ase.ra.smpp.stackif.DeliverSM;
import com.baypackets.ase.ra.smpp.stackif.SubmitSM;
import com.baypackets.ase.ra.smpp.stackif.SubmitSMResp;
import com.baypackets.ase.resource.Message;
import com.baypackets.ase.resource.ResourceException;

/**
 * This Class is being used for sending/receiving Enum request/res
 * 
 * @author reeta
 *
 */
public class SmppProtocolHandler implements ProtocolHandler {

	private static final SmppProtocolHandler INSTANCE = new SmppProtocolHandler();
	private static Logger logger = Logger.getLogger(SmppProtocolHandler.class);

	private SmppProtocolHandler() {
	}

	/**
	 * This method is used to send short message to smsc
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

		switch (action.getActionType()) {

		case ACTION_SEND_SHORT_MESSAGE: {
			SmppProtocolHelper.sendShortMessage(appSession, callData, action);
			break;
		}

		default: {
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
	 * This method id used to handle incoming response
	 * @param message
	 * @param calldata
	 * @param serviceInstance
	 */
	public void handleResponse(Message message, CallData calldata,
			ServiceInterface serviceInstance) {
		// TODO Auto-generated method stub
		if (logger.isDebugEnabled()) {
			logger.debug("::handleResponse  SmppResponse:");
		}

		if (logger.isDebugEnabled()) {
			logger.debug("::Notify Service:");
		}
		LegData leg1 = (LegData) calldata.get(CallDataAttribute.P_LEG1);
		if(message instanceof SubmitSMResp) {
			try {
				leg1.set(LegDataAttributes.SMPP_SUBMIT_SM_ID,((SubmitSMResp)message).getMessageId());
			} catch (SmppResourceException e) {
			logger.error("No submitSm response id present");
			}
		}
		ServiceInterface serviceHandler = PhUtilityServices.getInstance(
				(String) calldata.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();
		Event event = new Event(EventType.EVENT_SHORT_MESSAGE_RESPONSE_RECEIVED,
				Protocol.SMPP, CallDataAttribute.P_LEG1.name());
		try {
			ProtocolRouter.getInstance().execute(event, calldata, serviceHandler);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		SmppProtocolHelper.writeServiceCdr(calldata, null);
	}

	/**
	 * This method id used to handle incoming request
	 * @param request
	 * @param callData
	 * @param serviceInstance
	 */
	public void handleRequest(SmppRequest request, CallData callData,
			ServiceInterface serviceInstance) {
				if (logger.isDebugEnabled()) {
					logger.debug("::handleRequest  SmppRequest:");
				}

				if (logger.isDebugEnabled()) {
					logger.debug("::Notify Service:");
				}
				if(request instanceof SubmitSM) {
					LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
					ServiceInterface serviceHandler = PhUtilityServices.getInstance(
							(String) callData.get(CallDataAttribute.SERVICE_ID))
							.getServiceHandler();
					
					try {
						leg1.set(LegDataAttributes.SMPP_SRC_PHONE_NUMBER, request.getSourceAddr().getRange());
						leg1.set(LegDataAttributes.SMPP_DEST_PHONE_NUMBER,request.getDestinationAddr().getRange());
						leg1.set(LegDataAttributes.SMPP_SHORT_MESSAGE,request.getShortMessage());
						callData.set(CallDataAttribute.P_LEG1, leg1);
						callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID, request.getMessageId());
						callData.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(1));
					} catch (SmppResourceException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					Event event = new Event(EventType.EVENT_SHORT_MESSAGE_RECEIVED,
							Protocol.SMPP, CallDataAttribute.P_LEG1.name());
					try {
						ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
//					
//					try {
//						request.createResponse(200).send();
//					} catch (IOException e) {
//						// TODO Auto-generated catch block
//						e.printStackTrace();
//					} catch (ResourceException e) {
//						// TODO Auto-generated catch block
//						e.printStackTrace();
//					}
					SmppProtocolHelper.writeServiceCdr(callData, null);
				}else if (request instanceof DeliverSM) {
					DeliverSM deliverSMRequest= ((DeliverSM)request);
					if (logger.isDebugEnabled()) {
						logger.debug("DeliverSm Recieved in SmppProcolHandler with values:- " + ((DeliverSM)request).toString());
					}
				LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
				try {
					String shortmsg= deliverSMRequest.getShortMessage();
					if(shortmsg!=null){
						if(logger.isDebugEnabled()) {
							logger.debug("Short message is "+shortmsg);
							logger.debug("Setting SMPP_DELIVERY_MSISDN "+ deliverSMRequest.getDestinationAddr().getRange());
						}
						leg1.set(LegDataAttributes.SMPP_DELIVERY_MSISDN, deliverSMRequest.getDestinationAddr().getRange());
						
						// getting value
						String[] array=shortmsg.split(" ");
						for(int i=0;i<array.length;i++){
							String token=array[i];
							token=token.trim();
							String[] innerArray=token.split(":");
							String key=innerArray[0];
							if(key.equalsIgnoreCase("err")){
								logger.debug("Setting SMPP_DELIVERY_RESPONSE_CODE "+ innerArray[1]);
							leg1.set(LegDataAttributes.SMPP_DELIVERY_RESPONSE_CODE, innerArray[1]);	
							}else if(key.equalsIgnoreCase("stat")) {
								logger.debug("Setting SMPP_DELIVERY_RESPONSE_STATUS "+ innerArray[1]);
								leg1.set(LegDataAttributes.SMPP_DELIVERY_RESPONSE_STATUS, innerArray[1]);	
							}else if(key.equalsIgnoreCase("date") && (array[i-1].trim().equalsIgnoreCase("submit"))) {
								logger.debug("Setting SMPP_SUBMIT_DATE_TIME "+ innerArray[1]);
								formatDate(innerArray[1]);
								leg1.set(LegDataAttributes.SMPP_SUBMIT_DATE_TIME,formatDate(innerArray[1]));	
							}else if(key.equalsIgnoreCase("date") && (array[i-1].trim().equalsIgnoreCase("done"))) {
								logger.debug("Setting SMPP_DELVERY_DATE_TIME "+ innerArray[1]);
								leg1.set(LegDataAttributes.SMPP_DELVERY_DATE_TIME,formatDate(innerArray[1]));	
							} else if(key.equalsIgnoreCase("id")) {
								leg1.set(LegDataAttributes.SMPP_DELIVER_SM_ID,innerArray[1]);
							}
						}
					
						callData.set(CallDataAttribute.P_LEG1, leg1);
					}
				}catch(Exception e) {
				logger.error("Error in getting values from short message:- "+e);
				}
				     
	               
					ServiceInterface serviceHandler = PhUtilityServices.getInstance(
							(String) callData.get(CallDataAttribute.SERVICE_ID))
							.getServiceHandler();
					Event event = new Event(EventType.EVENT_SHORT_MESSAGE_DELIVERED,
							Protocol.SMPP, CallDataAttribute.P_LEG1.name());
					
					try {
						ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
				}
				
				
			
	}
	
 private String formatDate(String dateTime) {
	 SimpleDateFormat format1 = new SimpleDateFormat("yyMMddhhmm");
	 SimpleDateFormat format2 = new SimpleDateFormat("dd/MM/yy HH:mm");
	 Date date;
	 String finalDate= null;
	try {
		date = format1.parse(dateTime);
		finalDate= format2.format(date);
		logger.debug("Date formed:- "+ finalDate);

	} catch (ParseException e) {
		logger.error("Cannot parse date due to:- "+ e);
	}
	
	 return finalDate;
 }

}
