/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.
 

Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
****/
package com.agnity.ph.ainscf.lidb;

import java.util.ArrayList;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.ph.ainscf.AinScfProtocolHelper;
import com.agnity.ph.ainscf.AinScfProtocolUtil;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ASNParsingException.FAILTYPE;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.tcap.component.InvokeIndEvent;

/**
 * @author rarya
 *@author stiwari
 */
public class LidbQueryHandler {
	
	private static Logger logger = Logger.getLogger(LidbQueryHandler.class);

	/**
	 * method that process for Application BNS,GN,CC,TLNS and OLNS
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 * @throws Exception
	 */
	public static Action[] processLIDBQuery(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		Action[] action = null;
		
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
		if(logger.isDebugEnabled()){
			logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler ");
		}
		
		if(legData.get(LegDataAttributes.P_MRS_COMP_LIST) == null) {
			ArrayList<InvokeIndEvent> invokeIndEventList = new ArrayList<InvokeIndEvent>();
			legData.set(LegDataAttributes.P_MRS_COMP_LIST, invokeIndEventList);
			callData.set(CallDataAttribute.P_MULTIPLE_COMPONENT_COUNT, null);
			callData.set(CallDataAttribute.P_MULTIPLE_COMPONENT_CUR_COUNT, null);
		}
		if (!invokeIndEvent.isLastComponent()) {
			//set component count to P_MULTIPLE_COMPONENT_COUNT
			setComponentCount(callData);
			//NOT LAST COMPONENT
			@SuppressWarnings("unchecked")
			ArrayList<InvokeIndEvent> list = (ArrayList<InvokeIndEvent>) legData.get(LegDataAttributes.P_MRS_COMP_LIST) == null?
					new ArrayList<InvokeIndEvent>():(ArrayList<InvokeIndEvent>) legData.get(LegDataAttributes.P_MRS_COMP_LIST);
					list.add(invokeIndEvent);
					legData.set(LegDataAttributes.P_MRS_COMP_LIST, list);
					if (logger.isInfoEnabled()) {
						logger.info("AC Query component list (NOT LAST):"+list);
					}

		} else {
			//set component count to P_MULTIPLE_COMPONENT_COUNT
			setComponentCount(callData);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.AC_QUERY);
			@SuppressWarnings("unchecked")
			ArrayList<InvokeIndEvent> list = (ArrayList<InvokeIndEvent>) legData.get(LegDataAttributes.P_MRS_COMP_LIST);
			list.add(invokeIndEvent);

			legData.set(LegDataAttributes.P_MRS_COMP_LIST, list);
			if (logger.isInfoEnabled()) {
				logger.info("AC Query component list (LAST):"+list);
			}
			//set curr count to 1
			int index = 1;
			callData.set(CallDataAttribute.P_MULTIPLE_COMPONENT_CUR_COUNT, index);
			//call parseLIDBQuery for 1st component
			InvokeIndEvent nextInvokeIndEvent = list.get(0);
			action = parseLIDBQuery(nextInvokeIndEvent,tcapSession);
			
		}
		return action;
	}
	
	public static Action[] parseLIDBQuery(InvokeIndEvent invokeIndEvent,TcapSession tcapSession) throws Exception {
		if(logger.isDebugEnabled()) {
			logger.debug("inside parseLIDBQuery");
		}
		Action[] action = null;
		byte[] input = invokeIndEvent.getParameters().getParameter();
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		//for setting linked id
		legData.set(LegDataAttributes.P_LAST_RX_INVOKE_ID, invokeIndEvent.getInvokeId());
		
		int dialogId = tcapSession.getDialogueId();
		if(input[2] == (byte) 0xDF){
			//BNS and OLNS
			if(input[3] == (byte) 0x71){
				if(logger.isDebugEnabled()){
					logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for BNS ");
				}
				action= AinScfProtocolHelper.processBNSQuery(invokeIndEvent, tcapSession,
							PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
									.getCallTraceService());
			}else if(input[3] == (byte) 0x6f){
				if(logger.isDebugEnabled()){
					logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for OLNS ");
					action= AinScfProtocolHelper.processOLNSQuery(invokeIndEvent, tcapSession,
							PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
									.getCallTraceService());
				}
			}else if(input[3] == (byte) 0x75){
				if(logger.isDebugEnabled()){
					logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for GetDataQuery ");
					action= AinScfProtocolHelper.processLidbGetData(invokeIndEvent, tcapSession,
							PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.getCallTraceService());
				}
				
			}else if(input[3] == (byte) 0x70){
				if(logger.isDebugEnabled()){
					logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for CC1Query ");
					action= AinScfProtocolHelper.processLidbCC1Query(invokeIndEvent, tcapSession,
							PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
									.getCallTraceService());
				}			
			}else if(input[3] == (byte) 0x5F){
				if(logger.isDebugEnabled()){
					logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for CC2Query ");
					action= AinScfProtocolHelper.processLidbCC2Query(invokeIndEvent, tcapSession,
							PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
									.getCallTraceService());
				}			
			}else if(input[3] == (byte) 0x69){
				if(logger.isDebugEnabled()){
					logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for TLNSQuery ");
					action= AinScfProtocolHelper.processLidbTLNSQuery(invokeIndEvent, tcapSession,
							PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
									.getCallTraceService());
				}			
			}else if(input[3] == (byte) 0x6E){
				if(logger.isDebugEnabled()){
					logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for Intercpet Query ");
					action= AinScfProtocolHelper.processLidbInterceptQuery(invokeIndEvent, tcapSession,
							PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
									.getCallTraceService());
				}			
			}else if(input[3] == (byte) 0x56){
				if(logger.isDebugEnabled()){
					logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for ICDC Query ");
					action= AinScfProtocolHelper.processLidbICDCQuery(invokeIndEvent, tcapSession,
							PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
									.getCallTraceService());
				}			
			}else if(input[3] == (byte) 0x49){
				if(logger.isDebugEnabled()){
					logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for AccountCode ");
				}
					action= AinScfProtocolHelper.processAccountCodeQuery(invokeIndEvent, tcapSession,
							PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
									.getCallTraceService());			
			}else{
				ASNParsingException ape = new ASNParsingException();
						ape.setMessage(MESSAGE.LIDB_PROTOCOL_ERR);
						ape.setParseFailType(FAILTYPE.PROTOCOL_ERROR);
				logger.error(dialogId + " Unsupported LIDB Service Request: 0xDF " + input[3]);
				return AinScfProtocolHelper.getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
			}
			
		}else if(input[2] == (byte) 0x97){
			if(logger.isDebugEnabled()){
				logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for GN ");
			}
			action= AinScfProtocolHelper.processGNQuery(invokeIndEvent, tcapSession,
					PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.getCallTraceService());
		}
		else if(input[2] == (byte) 0xAA) {
			if(logger.isDebugEnabled()){
				logger.debug(dialogId+"[PH] :: Inside LidbQueryHandler recived operation for AC ");
			}
			action= AinScfProtocolHelper.processAcQuery(invokeIndEvent, tcapSession,
					PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.getCallTraceService());
		}
		else{
			logger.error(dialogId + "Unsupported LIDB Request " + input[2] + " " + input[3]);
			ASNParsingException ape = new ASNParsingException();
			ape.setMessage(MESSAGE.LIDB_PROTOCOL_ERR);
			ape.setParseFailType(FAILTYPE.PROTOCOL_ERROR);
	      return AinScfProtocolHelper.getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		}
		if(logger.isDebugEnabled()) {
			logger.debug("exit parseLIDBQuery");
		}
		return action;
	}
	
	/**
	 * method used to set Multiple component count 
	 * @param callData
	 */
	public static void setComponentCount(CallData callData) {
		if(logger.isDebugEnabled()) {
			logger.debug("inside setComponentCount");
		}
		int index = callData.get(CallDataAttribute.P_MULTIPLE_COMPONENT_COUNT) != null? (int) callData.get(CallDataAttribute.P_MULTIPLE_COMPONENT_COUNT) : 0;
		if(logger.isDebugEnabled()) {
			logger.debug("current component count :: "+ index);
		}
		callData.set(CallDataAttribute.P_MULTIPLE_COMPONENT_COUNT, index+1);
		
		if(logger.isDebugEnabled()) {
			logger.debug("exit setComponentCount");
		}
	}
}
