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
package com.agnity.ph.mapscf;

import com.agnity.mphdata.common.*;

import jain.MandatoryParameterNotSetException;
import jain.ParameterNotSetException;
import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;
import jain.protocol.ss7.tcap.ComponentIndEvent;
import jain.protocol.ss7.tcap.ComponentReqEvent;
import jain.protocol.ss7.tcap.DialogueIndEvent;
import jain.protocol.ss7.tcap.DialogueReqEvent;
import jain.protocol.ss7.tcap.GTIndicator0001;
import jain.protocol.ss7.tcap.GTIndicator0010;
import jain.protocol.ss7.tcap.GTIndicator0011;
import jain.protocol.ss7.tcap.GTIndicator0100;
import jain.protocol.ss7.tcap.GlobalTitle;
import jain.protocol.ss7.tcap.TcapConstants;
import jain.protocol.ss7.tcap.component.ComponentConstants;
import jain.protocol.ss7.tcap.component.ErrorIndEvent;
import jain.protocol.ss7.tcap.component.ErrorReqEvent;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.InvokeReqEvent;
import jain.protocol.ss7.tcap.component.Operation;
import jain.protocol.ss7.tcap.component.Parameters;
import jain.protocol.ss7.tcap.component.RejectIndEvent;
import jain.protocol.ss7.tcap.component.RejectReqEvent;
import jain.protocol.ss7.tcap.component.ResultIndEvent;
import jain.protocol.ss7.tcap.component.ResultReqEvent;
import jain.protocol.ss7.tcap.dialogue.BeginIndEvent;
import jain.protocol.ss7.tcap.dialogue.BeginReqEvent;
import jain.protocol.ss7.tcap.dialogue.DialogueConstants;
import jain.protocol.ss7.tcap.dialogue.DialoguePortion;
import jain.protocol.ss7.tcap.dialogue.EndReqEvent;
import jain.protocol.ss7.tcap.dialogue.ProviderAbortIndEvent;
import jain.protocol.ss7.tcap.dialogue.UserAbortIndEvent;
import jain.protocol.ss7.tcap.dialogue.UserAbortReqEvent;

import java.rmi.server.RemoteRef;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.inapitutcs2.enumdata.CauseValEnum;
import com.agnity.inapitutcs2.util.Util;
import com.agnity.map.exceptions.InvalidInputException;
import com.agnity.map.operations.MapOpCodes;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.Action.DROP_CALL_MODE;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ASNParsingException.FAILTYPE;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;
import com.agnity.ph.common.exception.CriticalityTypeException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.common.measurement.MeasurementCounter;
import com.agnity.ph.common.measurement.PhMeasurementService;
import com.agnity.ph.common.measurement.enums.SS7Message;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil;
import com.agnity.ph.inapcs1scf.InapCS1ScfRelReasonCode;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapSession;

/**
 * 
 * This class is a helper class which is used by MAP protocol handler to delegate
 * the message processing of incoming messages and and creation of outgoing MAP messages
 *
 */
public class MapScfProtocolHelper {

	private static Logger logger = Logger
			.getLogger(MapScfProtocolHelper.class);
	private static Object src = "source".intern();


	/**
	 * This method is used to send any time interrogate request to HLR
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @param cCallTraceService
	 * @throws Exception
	 */
	/**
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @param cCallTraceService
	 * @throws Exception
	 */
	public static void sendAnyTimeInterrogation(TcapSession tcapSession,
			CallData callData, Action action,CallTraceService cCallTraceService) throws Exception {

		int dialogId = tcapSession.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(action.getLeg()));
		legData.set(LegDataAttributes.P_DIALOG_ID, dialogId);
		callData.set(CallDataAttribute.P_DIALOG_ID, dialogId);
//		callData.set(
//				CallDataAttribute.P_ORIG_LEG_CALL_ID,
//				String.valueOf(dialogId));
		
		SipApplicationSession appSession=MapScfProtocolUtil.getAppSession(tcapSession);
		

		//logger.error(dialogId + " [PH]:: Inside sendAnyTimeInterrogation P_DIALOG_ID :" +dialogId);
		
		
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendAnyTimeInterrogation, legType:" + action.getLeg());
		}

		byte[] ati = MapScfProtocolParser.createAnyTimeInterrogate(callData, action);
		byte[] erOpCode = { MapOpCodes.MAP_ANY_TIME_INTERROGATION_BYTE};

		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				erOpCode);

		List<SccpUserAddress> mapScfSua = MapScfProtocolConfig.getInstance().getSccpLocalAddressList((String)callData.get(CallDataAttribute.SERVICE_ID));
		SccpUserAddress remoteSua = null;//(SccpUserAddress) legData.get(LegDataAttributes.P_HLR_SUA);

		String destPc = (String) legData.get(LegDataAttributes.NP_DIALOUT_DEST_PC);
		String destSsn = (String) legData.get(LegDataAttributes.NP_DIALOUT_DEST_SSN);
		String destGTDigits = (String) legData.get(LegDataAttributes.NP_DIALOUT_DEST_GT_DIGITS);
		String destTT = (String) legData.get(LegDataAttributes.NP_DIALOUT_DEST_TRANS_TYPE);
		String destRInd = (String) legData.get(LegDataAttributes.NP_DIALOUT_DEST_ROUTING_IND);
		String destGTInd = (String) legData.get(LegDataAttributes.NP_DIALOUT_DEST_GT_IND);

		if(logger.isDebugEnabled()){
			logger.debug("ATI params from APplication: destPC:" + destPc +", destSsn:" + destSsn + ", destGTDigits:"
					+ destGTDigits + ",destTT:" + destTT + ", destRInd:" + destRInd +
					", destGTInd:" + destGTInd);
		}

		if(!StringUtils.isNotBlank(destPc)){
			destPc = MapScfProtocolConfig.getConfigData(MapScfProtocolConfig.DESTINATION_PC);
		}

		if(!StringUtils.isNotBlank(destSsn)){
			destSsn = MapScfProtocolConfig.getConfigData(MapScfProtocolConfig.DESTINATION_SSN);
		}

		if(!StringUtils.isNotBlank(destGTDigits)){
			destGTDigits = MapScfProtocolConfig.getConfigData(MapScfProtocolConfig.DESTINATION_GT_DIGITS);
		}

		if(!StringUtils.isNotBlank(destTT)){
			destTT = MapScfProtocolConfig.getConfigData(MapScfProtocolConfig.DESTINATION_TT);
		}

		if(!StringUtils.isNotBlank(destRInd)){
			destRInd = MapScfProtocolConfig.getConfigData(MapScfProtocolConfig.DESTINATION_ROUTING_IND);
		}

		if(!StringUtils.isNotBlank(destGTInd)){
			destGTInd = MapScfProtocolConfig.getConfigData(MapScfProtocolConfig.DESTINATION_GT_IND);
		}

		if(logger.isDebugEnabled()){
			logger.debug("ATI params Normalized: destPC:" + destPc +", destSsn:" + destSsn + ", destGTDigits:"
					+ destGTDigits + ",destTT:" + destTT + ", destRInd:" + destRInd +
					", destGTInd:" + destGTInd);
		}

		GlobalTitle gtRemote=null;

		// Check for routing indicator. Application MUST set routing indicator
		// if not defined then error will be thrown. Possible values are 1 or 2 
		// - Routing based on subsystem    - 1
		// - Routing based on Global Title - 2
		//ROUTING_GLOBALTITLE   1
		// Next - create Remote SUA possible combination 
		//  - Both PC and SSN may be present - application to set both values 
		//  - Either of PC - SSN could be present 
		//  - Neither of PC - SSN could be present (Need to check)
		remoteSua = createProvidedDestSUA(destSsn, destPc);

		// in case remote Sua is null (which it will not be as in any case 
		// SUA will be created)
		if(remoteSua == null){
			logger.error(dialogId + " Unable to create SUA for pc:" + destPc + " and ssn:" + destSsn);
			throw new InvalidInputException("Unable to create SUA for pc:" + destPc + " and ssn:" + destSsn);
		}

		if(StringUtils.isNotBlank(destRInd) && StringUtils.equalsIgnoreCase(destRInd, "2")){

			//GT digits in called party SCCP address shall be same as MSISDN set by application
			String msisdn=(String)legData.get(LegDataAttributes.MAP_ATI_MSISDN);

			// perform GT handling
			gtRemote = createGlobalTitle(destTT, destGTInd, msisdn);

			if(gtRemote != null && remoteSua != null){
				remoteSua.setGlobalTitle(gtRemote);
				remoteSua.setRoutingIndicator(jain.protocol.ss7.AddressConstants.ROUTING_GLOBALTITLE);
				remoteSua.setNationalUse(true);

				if(logger.isDebugEnabled()){
					logger.debug(dialogId + " setting GT in remote SUA [" + remoteSua + "]");
				}
			}
		}else if(StringUtils.isNotBlank(destRInd) && StringUtils.equalsIgnoreCase(destRInd, "1")){
			if(remoteSua != null){
				remoteSua.setRoutingIndicator(jain.protocol.ss7.AddressConstants.ROUTING_SUBSYSTEM);
				remoteSua.setNationalUse(true);
			}
		}else{
			logger.error(dialogId + " Invalid routing indicator 1-pc-ssn, 2-GT, configured:" + destRInd);
			throw new InvalidInputException("Invalid routing indicator 1-pc-ssn, 2-GT, configured:" + destRInd);
		}

		// Configuring Local SUA. Reading from configured value (Service_Profile)
		SccpUserAddress localSua = null;
		GlobalTitle gtiLocal = null;
		if(mapScfSua.isEmpty()) {
			logger.error("Local MAP SCF not configured");
			throw new InvalidInputException("Local MAP SCF not configured, failed to create ATI request");
		}else{
			// TODO: device a way to support multiple map scf local mapping
			localSua = MapScfProtocolConfig.getLocalSua();
			if(localSua == null){
				if(logger.isDebugEnabled()){
					logger.debug("Using local SUA as MapScfConfig is not configured");
				}
			localSua = mapScfSua.get(0); 
			}

			if(localSua!=null){
				tcapSession.setAttribute("SccpUserAddressAttr",localSua);
			}
			LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
			
			localSua=MapScfProtocolUtil.updateCallingAddress(callData, leg2Data, localSua);
			// local GT indicator created with following value 
			// Translation Type = 10
			// GTIndicator = 2 ; GTIndicator0010
			// GT Digits - same as ATI GSM SCF address set by application 
			String gsmScfAddr= (String)legData.get(LegDataAttributes.MAP_ATI_GSM_SCF_ADDRESS);

			gtiLocal = createGlobalTitle("10", "2", gsmScfAddr);
			localSua.setRoutingIndicator(jain.protocol.ss7.AddressConstants.ROUTING_GLOBALTITLE);
			localSua.setGlobalTitle(gtiLocal);
			localSua.setNationalUse(true);
			localSua.setProtocolVariant(2); // 2 is ANSI for INC
		}

		if(logger.isDebugEnabled()){
			logger.debug("Local SUA = "+localSua+", Remote SUA = "+remoteSua);
		}

		BeginReqEvent begin = new BeginReqEvent(src, dialogId, localSua, remoteSua);

		InvokeReqEvent erInvokeReqEvent = new InvokeReqEvent(src,dialogId, rcOperation);
		erInvokeReqEvent.setInvokeId(MapScfProtocolUtil.getNextInvokeId(callData));
		erInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, ati));
		erInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		erInvokeReqEvent.setLastInvokeEvent(true);

		legData.set(LegDataAttributes.P_LEG_MAP_STATE, MapCallStates.OPERATION_IN_PROGRESS);

		// modify this
		callData.set(CallDataAttribute.P_DIALOG_ID, dialogId);
		callData.set(CallDataAttribute.P_PROTOCOL, Protocol.MAP_SCF);
		callData.set(CallDataAttribute.ATI_INVOKE_ID, erInvokeReqEvent.getInvokeId());

		// check if dialogue Portion is set or not
		if(MapScfProtocolConfig.getDialoguePortion() != null){
			begin.setDialoguePortion(MapScfProtocolConfig.getDialoguePortion());
		}

		sendComponentReq(erInvokeReqEvent, callData, cCallTraceService);

		sendDialogueReq(begin, callData, cCallTraceService);

		MapScfProtocolUtil.setLastInvokeIdStartRange(
				MapScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1,
				callData);
		MapScfProtocolUtil.setLastInvokeIdEndRange(
				MapScfProtocolUtil.getLastInvokeId(callData), callData);

		 SimpleDateFormat sdfDate = new SimpleDateFormat("yyyyMMddHHmmssSSS");//dd/MM/yyyy
		    Date now = new Date();
		    String strDate = sdfDate.format(now);
		legData.set(LegDataAttributes.MAP_ATI_REQ_SENT_TIMESTAMP, strDate);
		
		HttpServletRequest req=(HttpServletRequest) callData.get(CallDataAttribute.NP_HTTP_REQ);
	//	logger.error("ATI_REQ:"+ req.getSession().getId()+","+tcapSession.getDialogueId()+","+legData.get(LegDataAttributes.MAP_ATI_MSISDN)+","+strDate);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendAnyTimeInterrogation");
		}
	}



	/**
	 * Method is used to create global title based on fields provided by 
	 * application. Type of global title shall be decided based on GT Indicator. 
	 * In case application has not set value it shall be read from static 
	 * values defined in mph.profperties. 
	 * @param callData
	 * @param legData - LegData as defined by action.
	 * @return
	 */
	@SuppressWarnings("deprecation")
	private static GlobalTitle createGlobalTitle(String destTT, String destGTInd, 
			String destGTDigits) {

		if(logger.isDebugEnabled()){
			logger.debug("Inside createGlobalTitle for GTDigits:" + destGTDigits);
		}

		int translationType = 10; // dfault for MAP is 10
		if(StringUtils.isNotBlank(destTT)){
			translationType = Integer.parseInt(destTT);
		}

		int gtInd = 2; // default is 2
		if(StringUtils.isNotBlank(destGTInd)){
			gtInd = Integer.parseInt(destGTInd);
		}

		byte[] encodedGtDigits = null;
		int encodingScheme = 0;
		if(StringUtils.isNotBlank(destGTDigits)){
			encodedGtDigits = MapScfProtocolUtil.encodeDigitsInBcdFormat(destGTDigits);

			if(destGTDigits.length()%2 == 0){
				encodingScheme = 2; // even
			}else {
				encodingScheme = 1; // odd
			}

			if (logger.isDebugEnabled()) {
				logger.debug("MAP encoded GT Digits "+ CommonUtils.formatBytes(encodedGtDigits));
			}
		}

		// Create global title based on GTIndicator set by application 
		// i fnot set then by default create GT indicator of type 2
		GlobalTitle gt = null;
		// specific GT params

		switch(gtInd){
		case 1:{
			gt = new GTIndicator0001();
			((GTIndicator0001) gt).setTranslationType((byte)translationType);
		}
		break;
		case 2:{
			// set translation type and encoded digits 
			gt = new GTIndicator0010((byte)translationType, encodedGtDigits);
		}
		break;
		case 3:{
			//
			gt = new GTIndicator0011((byte)translationType, 
					jain.protocol.ss7.AddressConstants.NP_ISDN_TEL,
					((encodingScheme == 1)?jain.protocol.ss7.AddressConstants.ES_ODD:
						jain.protocol.ss7.AddressConstants.ES_EVEN), encodedGtDigits);
		}
		break;
		case 4:{
			gt = new GTIndicator0100((byte)translationType, 
					jain.protocol.ss7.AddressConstants.NP_ISDN_TEL,
					((encodingScheme == 1)?jain.protocol.ss7.AddressConstants.ES_ODD:
						jain.protocol.ss7.AddressConstants.ES_EVEN), 
					jain.protocol.ss7.AddressConstants.NA_SUBSCRIBER,
					encodedGtDigits);
		}
		break;
		default:{
			// GT address 2by default
			gt = new GTIndicator0010((byte)translationType, encodedGtDigits);
		}
		break;
		};

		//		if (gtInd == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0001) {
		//			GTIndicator0001 gt1 = new GTIndicator0001();		
		//			gt1.setTranslationType(translationType);
		//			gt = gt1;
		//
		//		} else if (gtInd == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0010) {
		//			gt = new GTIndicator0010();
		//			((GTIndicator0001) gt).setTranslationType(translationType);
		//			//gt = gt2;
		//		} else if (gtInd == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0011) {
		//			GTIndicator0011 gt3 = new GTIndicator0011();
		//
		//			gt3.setTranslationType(translationType);
		//			gt = gt3;
		//			// adding Translation Type
		//		} else if (gtInd == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0100) {
		//			GTIndicator0100 gt4 = new GTIndicator0100();
		//			gt4.setTranslationType(translationType);
		//			gt = gt4;
		//		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: updateGlobalTitle exit  "+gt);
		}
		return gt;
	}

	private static SccpUserAddress getSccpUserAddress() {
		SccpUserAddress hlrSccpAddr=null;

		String	destPc = MapScfProtocolConfig.getConfigData(MapScfProtocolConfig.DESTINATION_PC);

		String	destSsn = MapScfProtocolConfig.getConfigData(MapScfProtocolConfig.DESTINATION_SSN);

		if (logger.isDebugEnabled()) {
			logger.debug("getSccpUserAddress " + destPc +" hlsSsn "+ destSsn);
		}


		if (destPc!=null &&!destPc.isEmpty()  && destSsn!=null &&! destSsn.isEmpty()) {
			String[] tmp = destPc.split("-");
			SignalingPointCode spc = null;

			if (tmp.length == 3) {
				spc = new SignalingPointCode(Integer.parseInt(tmp[2]),
						Integer.parseInt(tmp[1]), Integer.parseInt(tmp[0]));
			}

			if (spc != null && !destSsn.isEmpty()) {
				hlrSccpAddr = new SccpUserAddress(new SubSystemAddress(spc,
						(short) Integer.parseInt(destSsn)));
				hlrSccpAddr.setProtocolVariant(1);


			}
		}
		return hlrSccpAddr;
	}

	/**
	 * This method id used to create sua from provided dialout destination SUA details
	 * Possible combination 
	 *  - PC is  present and  SSN is not present 
	 *  - both are not present 
	 *  - SSN may be present but PC is not present
	 * @param destSsn
	 * @param destPC
	 * @return
	 */
	private static SccpUserAddress createProvidedDestSUA(String destSsn,
			String destPC) {

		SccpUserAddress destSccpAddr = null;
		String[] pc = null;
		SignalingPointCode spc = null;
		short ssn = 0;

		if(StringUtils.isNotBlank(destPC)){
			pc = destPC.split("-");
			if (pc.length == 3) {
				spc = new SignalingPointCode(Integer.parseInt(pc[2]),
						Integer.parseInt(pc[1]), Integer.parseInt(pc[0]));
			}else{
				logger.error("createProvidedDestSUA: destPC is wrongly formatter:" + destPC);
			}
		}

		if(StringUtils.isNoneBlank(destSsn)){
			ssn = (short) Integer.parseInt(destSsn);
		}

		destSccpAddr = new SccpUserAddress(new SubSystemAddress(spc, ssn));
		destSccpAddr.setProtocolVariant(2); // setting 2 for ANSI in case of co-existence 
		// 2 is ANSI for INC where as 1 is ANSI for CAS

		if(logger.isDebugEnabled()){
			logger.debug("createProvidedDestSUA: destSccpAddress:" + destSccpAddr);
		}

		return destSccpAddr;
	}

	/**
	 * This method is called by protocol handler for sending MAP component
	 * indication event.
	 * 
	 * @param cre
	 *            represents the instance of ComponentReqEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @throws Exception
	 */
	static void sendComponentReq(ComponentReqEvent cre, CallData callData,
			CallTraceService cCallTraceService) throws Exception {

		//int dialogId = (Integer) callData
		//		.getPersistableData(CallDataAttribute.P_DIALOG_ID);

		int dialogId = cre.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendComponentReq");
			logger.debug("Calldata for dialog ["+dialogId+"] = "+callData);
		}

		PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider()
		.sendComponentReqEvent(cre);

		traceComponent(cre, callData);

	}

	/**
	 * This method will trace outgoing MAP component; We catch inside try catch
	 * throwable to avoid any impact on call in case of error;
	 * 
	 * @param cre
	 *            represents the instance of ComponentReqEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */

	static void traceComponent(ComponentReqEvent cre, CallData callData) {
		try {
			CallTraceService cCallTraceService = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
			String isTraceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);

			if (PhConstants.TRUE.equals(isTraceFlag)) {
				int primitive = cre.getPrimitiveType();

				StringBuilder traceMsg = new StringBuilder();

				traceMsg.append(">>>>Sending>>>>");
				traceMsg.append("\n");

				traceMsg.append("INVOKE ID::");
				traceMsg.append(cre.getInvokeId());
				traceMsg.append("\n");

				traceMsg.append("PRIMITIVE::");

				int callState = CallTraceService.CALL_IN_PROGRESS;

				switch (primitive) {
				case TcapConstants.PRIMITIVE_INVOKE: {
					traceMsg.append("INVOKE");
					traceMsg.append("\n");

					byte[] operCode = ((InvokeReqEvent) cre).getOperation()
							.getOperationCode();
					byte operCodeByte = operCode[0];
					traceMsg.append("MESSAGE::");
					switch (operCodeByte) {
					case MapOpCodes.MAP_ANY_TIME_INTERROGATION_BYTE: {
						traceMsg.append("ATI");
						traceMsg.append("\n");
						break;
					}
					case MapOpCodes.MAP_ANY_TIME_MODIFICATION_BYTE: {
						traceMsg.append("ATM");
						traceMsg.append("\n");
						break;
					}
					case MapOpCodes.MAP_ANY_TIME_SUBSCRIPTION_INTERROGATION_BYTE: {
						traceMsg.append("ATSI");
						traceMsg.append("\n");
						break;
					}
					case MapOpCodes.MAP_NOTE_SUBSCRIBER_DATA_MODIFIED_BYTE: {
						traceMsg.append("NSDM");
						traceMsg.append("\n");
						break;
					}

					default: {
						traceMsg.append("OPCODE BYTE:" + operCodeByte);
						traceMsg.append("\n");
						break;
					}
					}

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((InvokeReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((InvokeReqEvent) cre)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				}// @end switch message type
				case TcapConstants.PRIMITIVE_RESULT: {
					traceMsg.append("RESULT");
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ResultReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((ResultReqEvent) cre)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
					break;
				}
				case TcapConstants.PRIMITIVE_ERROR: {
					traceMsg.append("ERROR");
					traceMsg.append("\n");

					traceMsg.append("Type::");
					traceMsg.append(((ErrorReqEvent) cre).getErrorType());
					traceMsg.append("\n");

					traceMsg.append("Code::");
					traceMsg.append(Util.formatBytes(((ErrorReqEvent) cre)
							.getErrorCode()));
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ErrorReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((ErrorReqEvent) cre)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				}
				case TcapConstants.PRIMITIVE_REJECT: {
					traceMsg.append("REJECT");
					traceMsg.append("\n");

					traceMsg.append("Type::");
					traceMsg.append(((RejectReqEvent) cre).getProblemType());
					traceMsg.append("\n");

					traceMsg.append("Problem::");
					traceMsg.append(((RejectReqEvent) cre).getProblem());
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((RejectReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((RejectReqEvent) cre)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
				}
				}// @switch primitive

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								String.valueOf(callData
										.get(CallDataAttribute.P_DIALOG_ID)),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing component req event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error in traceComponent", t);
			}
		}
	}


	/**
	 * This method is used to send any time subscriber interrogate request to HLR
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @param cCallTraceService
	 * @throws Exception
	 */
	public static void sendAnyTimeSubsInterrogation(TcapSession tcapSession,
			CallData callData, Action action,CallTraceService cCallTraceService) throws Exception {

		int dialogId = tcapSession.getDialogueId();

		LegData legData = (LegData) callData
				.get(CallDataAttribute.valueOf(action.getLeg()));

		legData.set(LegDataAttributes.P_DIALOG_ID, dialogId);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leg = "+action.getLeg());
			logger.debug(dialogId + " [PH]:: Inside sendAnyTimeSubsInterrogation");
			logger.debug("calldata before sending = "+callData);
			logger.debug("leg data before sending = "+legData);
		}

		byte[] atsi = MapScfProtocolParser.createAnyTimeSubsInterrogate(callData, action);
		byte[] erOpCode = { MapOpCodes.MAP_ANY_TIME_SUBSCRIPTION_INTERROGATION_BYTE};

		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				erOpCode);

		/*		InvokeReqEvent erInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), rcOperation);
		erInvokeReqEvent.setInvokeId(MapScfProtocolUtil
				.getNextInvokeId(callData));
		erInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, atsi));
		erInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		legData.setPersistableData(LegDataAttributes.P_LEG_MAP_STATE, MapCallStates.OPERATION_IN_PROGRESS);

		sendComponentReq(erInvokeReqEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendAnyTimeSubsInterrogation");
		}*/

		List<SccpUserAddress> mapScfSua = MapScfProtocolConfig.getInstance().getSccpLocalAddressList((String)callData.get(CallDataAttribute.SERVICE_ID));
		SccpUserAddress remoteSua = (SccpUserAddress) legData.get(LegDataAttributes.P_HLR_SUA);
		SccpUserAddress localSua = null;
		if(mapScfSua.isEmpty()) {
			logger.error("Local MAP SCF not configured");
			throw new InvalidInputException("Local MAP SCF not configured, failed to create ATI request");
		}else{
			// TODO: device a way to support multiple map scf local mapping
			localSua = MapScfProtocolConfig.getLocalSua();
			if(localSua == null){
				if(logger.isDebugEnabled()){
					logger.debug("Using local SUA as MapScfConfig is not configured");
				}
			localSua = mapScfSua.get(0); 
			}
		}

		if(logger.isDebugEnabled()){
			logger.debug("Local SUA = "+localSua+", Remote SUA = "+remoteSua);
		}

		BeginReqEvent begin = new BeginReqEvent(src, dialogId,localSua, remoteSua);


		InvokeReqEvent erInvokeReqEvent = new InvokeReqEvent(src,
				dialogId, rcOperation);
		erInvokeReqEvent.setInvokeId(MapScfProtocolUtil
				.getNextInvokeId(callData));
		erInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, atsi));
		erInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		legData.set(LegDataAttributes.P_LEG_MAP_STATE, MapCallStates.OPERATION_IN_PROGRESS);

		// Change this 

		callData.set(CallDataAttribute.P_DIALOG_ID, dialogId);
		callData.set(CallDataAttribute.P_PROTOCOL, Protocol.MAP_SCF);


		sendComponentReq(erInvokeReqEvent, callData, cCallTraceService);

		sendDialogueReq(begin, callData, cCallTraceService);


		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendAnyTimeInterrogation");
		}


	}


	/**
	 * This method is used to send any time modification request to HLR
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @param cCallTraceService
	 * @throws Exception
	 */
	public static void sendAnyTimeModification(TcapSession tcapSession,
			CallData callData, Action action,CallTraceService cCallTraceService) throws Exception {

		int dialogId = tcapSession.getDialogueId();

		LegData legData = (LegData) callData
				.get(CallDataAttribute.valueOf(action.getLeg()));

		legData.set(LegDataAttributes.P_DIALOG_ID, dialogId);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leg = "+action.getLeg());
			logger.debug(dialogId + " [PH]:: Inside sendAnyTimeModification");
			logger.debug("calldata before sending = "+callData);
			logger.debug("leg data before sending = "+legData);
		}

		byte[] atm = MapScfProtocolParser.createAnyTimeModification(callData, action);
		byte[] erOpCode = { MapOpCodes.MAP_ANY_TIME_MODIFICATION_BYTE};

		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				erOpCode);

		/*

		InvokeReqEvent erInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), rcOperation);
		erInvokeReqEvent.setInvokeId(MapScfProtocolUtil
				.getNextInvokeId(callData));
		erInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, atm));
		erInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		legData.setPersistableData(LegDataAttributes.P_LEG_MAP_STATE, MapCallStates.OPERATION_IN_PROGRESS);

		sendComponentReq(erInvokeReqEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendAnyTimeModification");
		}
		 */
		List<SccpUserAddress> mapScfSua = MapScfProtocolConfig.getInstance().getSccpLocalAddressList((String)callData.get(CallDataAttribute.SERVICE_ID));
		SccpUserAddress remoteSua = (SccpUserAddress) legData.get(LegDataAttributes.P_HLR_SUA);
		SccpUserAddress localSua = null;
		if(mapScfSua.isEmpty()) {
			logger.error("Local MAP SCF not configured");
			throw new InvalidInputException("Local MAP SCF not configured, failed to create ATI request");
		}else{
			// TODO: device a way to support multiple map scf local mapping
			localSua = mapScfSua.get(0); 
		}

		if(logger.isDebugEnabled()){
			logger.debug("Local SUA = "+localSua+", Remote SUA = "+remoteSua);
		}

		BeginReqEvent begin = new BeginReqEvent(src, dialogId,localSua, remoteSua);


		InvokeReqEvent erInvokeReqEvent = new InvokeReqEvent(src,
				dialogId, rcOperation);
		erInvokeReqEvent.setInvokeId(MapScfProtocolUtil
				.getNextInvokeId(callData));
		erInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, atm));
		erInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		legData.set(LegDataAttributes.P_LEG_MAP_STATE, MapCallStates.OPERATION_IN_PROGRESS);

		callData.set(CallDataAttribute.P_DIALOG_ID, dialogId);
		callData.set(CallDataAttribute.P_PROTOCOL, Protocol.MAP_SCF);

		sendComponentReq(erInvokeReqEvent, callData, cCallTraceService);

		sendDialogueReq(begin, callData, cCallTraceService);


		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendAnyTimeModification");
		}
	}


	/**
	 * This method is used to send routing information request to HLR
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @param cCallTraceService
	 * @throws Exception
	 */
	public static void sendRoutingInformation(TcapSession tcapSession,
			CallData callData, Action action,CallTraceService cCallTraceService) throws Exception {

		int dialogId = tcapSession.getDialogueId();

		LegData legData = (LegData) callData
				.get(CallDataAttribute.valueOf(action.getLeg()));

		legData.set(LegDataAttributes.P_DIALOG_ID, dialogId);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leg = "+action.getLeg());
			logger.debug(dialogId + " [PH]:: Inside sendRoutingInformation");
			logger.debug("calldata before sending = "+callData);
			logger.debug("leg data before sending = "+legData);
		}


		byte[] sri = MapScfProtocolParser.createSendRoutingInformation(callData, action);
		byte[] erOpCode = { MapOpCodes.MAP_SEND_ROUTING_INFO_BYTE};

		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				erOpCode);

		/*	InvokeReqEvent erInvokeReqEvent = new InvokeReqEvent(src,
					tcapSession.getDialogueId(), rcOperation);
			erInvokeReqEvent.setInvokeId(MapScfProtocolUtil
					.getNextInvokeId(callData));
			erInvokeReqEvent.setParameters(new Parameters(
					Parameters.PARAMETERTYPE_SEQUENCE, sri));
			erInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

			legData.setPersistableData(LegDataAttributes.P_LEG_MAP_STATE, MapCallStates.OPERATION_IN_PROGRESS);

			sendComponentReq(erInvokeReqEvent, callData, cCallTraceService);
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: leave sendRoutingInformation");
			}	*/

		List<SccpUserAddress> mapScfSua = MapScfProtocolConfig.getInstance().getSccpLocalAddressList((String)callData.get(CallDataAttribute.SERVICE_ID));
		SccpUserAddress remoteSua = (SccpUserAddress) legData.get(LegDataAttributes.P_HLR_SUA);
		SccpUserAddress localSua = null;
		if(mapScfSua.isEmpty()) {
			logger.error("Local MAP SCF not configured");
			throw new InvalidInputException("Local MAP SCF not configured, failed to create ATI request");
		}else{
			// TODO: device a way to support multiple map scf local mapping
			localSua = mapScfSua.get(0); 
		}

		if(logger.isDebugEnabled()){
			logger.debug("Local SUA = "+localSua+", Remote SUA = "+remoteSua);
		}

		BeginReqEvent begin = new BeginReqEvent(src, dialogId,localSua, remoteSua);


		InvokeReqEvent erInvokeReqEvent = new InvokeReqEvent(src,
				dialogId, rcOperation);
		erInvokeReqEvent.setInvokeId(MapScfProtocolUtil
				.getNextInvokeId(callData));
		erInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, sri));
		erInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		legData.set(LegDataAttributes.P_LEG_MAP_STATE, MapCallStates.OPERATION_IN_PROGRESS);


		callData.set(CallDataAttribute.P_DIALOG_ID, dialogId);
		callData.set(CallDataAttribute.P_PROTOCOL, Protocol.MAP_SCF);

		sendComponentReq(erInvokeReqEvent, callData, cCallTraceService);
		sendDialogueReq(begin, callData, cCallTraceService);


		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendAnyTimeModification");
		}
	}

	/**
	 * This method performs the processing of continue dialogue indication
	 * event.
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processContinue(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) {
		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: processContinue Enter ");
		}

		if (dialogueIndEvent.isDialoguePortionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Set Dialogue Portion in call data");
			}

			try {
				legData.set(LegDataAttributes.P_DIALOG_PORTION,
						dialogueIndEvent.getDialoguePortion());
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId + ":: Error getting dialogue portion. "
						+ e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId + ":: Error getting dialog portion.",
							e);
					logger.info(dialogId
							+ "::IGNORE ParameterNotSetException fetching "
							+ "dialoguePortion from CONTINUE dialogue event");
				}
			}
		}
		return null;

	}

	/**
	 * This method performs the processing of TC_END dialogue indication event.
	 * In ANSI, Response is received which is mapped to END. 
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processEnd(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) {
		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processEnd Enter ");
			

		}

		return null;
	}

	/**
	 * This method performs the processing of Abort dialogue indication event.
	 * Marks the cause value as 41 for failed calls.
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processAbort(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) {
		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processAbort Enter ");
		}

		return null;
	}

	/**
	 * This method performs the processing of UAbort dialogue indication event.
	 * Marks the cause value as 31 because user hung up the call and related
	 * events are not armed..
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processUAbort(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) {
		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processUAbort Enter ");
		}

		return null;
	}



	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of PRIMITIVE_RESULT type is received.
	 * 
	 * @param resultIndEvent
	 *            represents an instance of ResultIndEvent
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action Object
	 * @throws Exception 
	 * @throws ParameterOutOfRangeException 
	 * @throws CriticalityTypeException 
	 * @throws ASNParsingException 
	 */
	static Action[] processResult(ResultIndEvent resultIndEvent,
			TcapSession tcapSession) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);


		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processResult");
		}
		

		byte[] operCode = null;
		Action[] action = null;
		if (resultIndEvent.isLastResultEvent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: It is last result event");
			}
			/*
			 * check if opcode present in result if yes get opercode directly
			 */
			if (resultIndEvent.isOperationPresent()) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Operation Present in event");
				}
				try {
					operCode = resultIndEvent.getOperation().getOperationCode();
				} catch (MandatoryParameterNotSetException e) {
					logger.error(dialogId + ":: Error getting operation code "
							+ e.getMessage());
					throw e;
				} catch (ParameterNotSetException e) {
					logger.warn(dialogId + ":: Error getting operation code "
							+ e.getMessage());
					if (logger.isInfoEnabled()) {
						logger.warn(dialogId
								+ ":: Error getting operation code", e);
						logger.info(dialogId
								+ ":: IGNORE ParameterNotSetException in getOperation.");
					}
				}
			}
			/*
			 * opcode not present get invoke id and fetch opcode form there
			 */
			else if (resultIndEvent.isInvokeIdPresent()) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Invoke Id present in event");
				}

			}
			/*
			 * if opcode is still null
			 */
			if (operCode != null) {
				String operCodeStr = CommonUtils.formatBytes(operCode);
				byte operCodeByte = operCode[0];
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Operation Code is "
							+ operCodeStr);
				}

				switch (operCodeByte) {
				case MapOpCodes.MAP_ANY_TIME_INTERROGATION_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: any time interrogation byte result");
					}

					MapScfProtocolParser.parseAnytimeInterrogateResult(tcapSession,
							resultIndEvent, callData);

					break;
				}
				case MapOpCodes.MAP_ANY_TIME_SUBSCRIPTION_INTERROGATION_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: any time subs interrogation byte result");
					}

					MapScfProtocolParser.parseAnytimeSubsInterrogateResult(
							resultIndEvent, callData);
					break;
				}
				case MapOpCodes.MAP_ANY_TIME_MODIFICATION_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: any time modification byte result");
					}
					MapScfProtocolParser.parseAnytimeModificationResult(
							resultIndEvent, callData);

					break;
				}
				case MapOpCodes.MAP_SEND_ROUTING_INFO_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: send routing information byte result");
					}
					MapScfProtocolParser.parseSendRoutingInfoRes(
							resultIndEvent, callData);

					break;
				}
				default: {
					logger.warn(dialogId
							+ ":: Received Result for unknown Component Indication Event "
							+ operCodeStr);
					callData.set(
							CallDataAttribute.NP_RELEASE_REASON_CODE,
							MapScfRelReasonCode.UNKNOWN_RESULT);
					action = getUnknownResultAction(tcapSession);
					break;
				}// end default
				}// end switch
				callData
				.set(CallDataAttribute.P_CALL_DISCONNECT_TIME,new Date());
				legData.set(LegDataAttributes.P_LEG_MAP_STATE, MapCallStates.OPERATION_COMPLETED);

			} else {
				logger.warn(dialogId + "::Result Operation code is unknown.");
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						MapScfRelReasonCode.UNKNOWN_RESULT);
				action = getUnknownResultAction(tcapSession);
			}
		}
		//logger.error("Not supported processResult() .....");
		return null;
	}

	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of PRIMITIVE_ERROR type is received.
	 * 
	 * @param errorIndEvent
	 *            represents an instance of ErrorIndEvent
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action Objects.
	 */
	static Action[] processError(ErrorIndEvent errorIndEvent,
			TcapSession tcapSession) {
		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processError");
		}

		int invokeId = -1;
		byte[] errorCode = null;


		try {
			invokeId = errorIndEvent.getInvokeId();
			errorCode = errorIndEvent.getErrorCode();
		} catch (MandatoryParameterNotSetException mpne) {
			logger.error(dialogId
					+ ":: MandatoryParameterNotSetException in RE", mpne);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					MapScfRelReasonCode.ERR_MSG_MAND_PARAM_MISSING);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UERROR,
					FAILTYPE.DEFAULT);
		} catch (ParameterNotSetException pne) {
			logger.error(dialogId + ":: ParameterNotSetException in RE", pne);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					MapScfRelReasonCode.ERR_MSG_PARAM_NOT_SET);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UERROR,
					FAILTYPE.DEFAULT);
		}

		// in case error is returned for ATI then send to application as ATI Failure.
		boolean found = false;
		if(callData.get(CallDataAttribute.ATI_INVOKE_ID) != null){
			int atiInvokeId = (int) callData.get(CallDataAttribute.ATI_INVOKE_ID);

			if(atiInvokeId == invokeId){
				found = true;
				String errorCodeStr = null;
				if(errorCode != null){
					int errCode = (int)errorCode[0];
					errorCodeStr = Integer.toString(errCode);
				}

				logger.debug(dialogId + ":: Process Error , invokeId matched, Error code is " + errorCodeStr);

				
				LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);	
				if(StringUtils.isNotBlank(errorCodeStr)){
					legData.set(LegDataAttributes.NP_ERROR_CODE, errorCodeStr);
				}

				Event event = new Event(EventType.EVENT_ATI_FAILURE, Protocol.MAP_SCF, CallDataAttribute.P_LEG1.name());
				try {
					ProtocolRouter.getInstance().execute(event, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
				} catch (Exception e) {
					logger.error("[PH]:: Exception thrown while parsing PAC result");
				}
			}
		}
		Action action = null;
		if(found == false){

			int lastInvokeIdStart = MapScfProtocolUtil
					.getLastInvokeIdStartRange(callData);
			int lastInvokeIdEnd = MapScfProtocolUtil
					.getLastInvokeIdEndRange(callData);
			/*
			 * validateInvokeId
			 */
			if ((invokeId < lastInvokeIdStart) || (invokeId > lastInvokeIdEnd)) {
				logger.warn(dialogId + "::Invoke id invalid in U error; recived:"
						+ invokeId + " valid range for current message:: "
						+ lastInvokeIdStart + " to " + lastInvokeIdEnd);
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						MapScfRelReasonCode.ERR_MSG_INVALID_INVOK_ID);
				return getInvalidInvokeIdAction(tcapSession, MESSAGE.UERROR);
			}
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Return action to Drop the call");
			}
			action = new Action(Action.ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			/*
			 * set cause as temporary failure
			 */
			action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());			

		}
		if (errorCode != null && logger.isDebugEnabled()) {
			String errorCodeStr = CommonUtils.formatBytes(errorCode);
			logger.debug(dialogId + ":: Error code is " + errorCodeStr);

		}

		if(action != null){
			return (new Action[] { action });
		}else{
			return null;
		}
	}

	/**
	 * This method is called by the Protocol handler whenever
	 * ParameterOutOfRangeException is thrown while parsing MAP signal
	 * received.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param message
	 *            represents an instance of MESSAGE
	 * @return an array of Action objects.
	 */
	static Action[] getOutOfRangeParamterAction(TcapSession tcapSession,
			MESSAGE message) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getOutOfRangeParamterAction::" + message);
		}

		/*
		 * handle as ASN parsing
		 */
		return getASNParsingFailureAction(tcapSession, message,
				FAILTYPE.DEFAULT);

	}



	/**
	 * This method will trace incoming MAP component; inside try catch
	 * throwable to avoid any impact on call in case of error;
	 * 
	 * @param cie
	 *            represents the instance of ComponentIndEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */
	static void traceComponent(ComponentIndEvent cie, CallData callData) {
		try {
			CallTraceService cCallTraceService = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
			String isTraceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);

			if (PhConstants.TRUE.equals(isTraceFlag)) {
				int primitive = cie.getPrimitiveType();

				StringBuilder traceMsg = new StringBuilder();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				traceMsg.append("<<<<Recieved<<<<");
				traceMsg.append("\n");

				traceMsg.append("INVOKE ID::");
				traceMsg.append(cie.getInvokeId());
				traceMsg.append("\n");

				traceMsg.append("PRIMITIVE::");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_INVOKE: {
					traceMsg.append("INVOKE");
					traceMsg.append("\n");

					byte[] operCode = ((InvokeIndEvent) cie).getOperation()
							.getOperationCode();
					int operCodeByte = CommonUtils.formatBytesToInt(operCode);

					traceMsg.append("MESSAGE::");
					switch (operCodeByte) {
					case MapOpCodes.MAP_ANY_TIME_INTERROGATION_BYTE: {
						traceMsg.append("ATI");
						traceMsg.append("\n");
						break;
					}
					case MapOpCodes.MAP_ANY_TIME_MODIFICATION_BYTE: {
						traceMsg.append("ATM");
						traceMsg.append("\n");
						break;
					}
					case MapOpCodes.MAP_ANY_TIME_SUBSCRIPTION_INTERROGATION_BYTE: {
						traceMsg.append("ATSI");
						traceMsg.append("\n");
						break;
					}
					case MapOpCodes.MAP_SEND_ROUTING_INFO_BYTE: {
						traceMsg.append("SRI");
						traceMsg.append("\n");
						break;
					}
					case MapOpCodes.MAP_NOTE_SUBSCRIBER_DATA_MODIFIED_BYTE: {
						traceMsg.append("NSDM");
						traceMsg.append("\n");
						break;
					}
					default: {
						traceMsg.append("OPCODE BYTE:" + operCodeByte);
						traceMsg.append("\n");
						break;
					}
					}

					Object traceCall=callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {

						if (((InvokeIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((InvokeIndEvent) cie)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				}// @end switch message type
				case TcapConstants.PRIMITIVE_RESULT: {
					traceMsg.append("RESULT");
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ResultIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((ResultIndEvent) cie)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
					break;
				}
				case TcapConstants.PRIMITIVE_ERROR: {
					traceMsg.append("ERROR");
					traceMsg.append("\n");

					traceMsg.append("Type::");
					traceMsg.append(((ErrorIndEvent) cie).getErrorType());
					traceMsg.append("\n");

					traceMsg.append("Code::");
					traceMsg.append(Util.formatBytes(((ErrorIndEvent) cie)
							.getErrorCode()));
					traceMsg.append("\n");

					Object traceCall=callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ErrorIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((ErrorIndEvent) cie)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				}
				case TcapConstants.PRIMITIVE_REJECT: {
					traceMsg.append("REJECT");
					traceMsg.append("\n");

					traceMsg.append("Type::");
					traceMsg.append(((RejectIndEvent) cie).getProblemType());
					traceMsg.append("\n");

					traceMsg.append("Problem::");
					traceMsg.append(((RejectIndEvent) cie).getProblem());
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((RejectIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((RejectIndEvent) cie)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
				}
				}// @switch primitive

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {

					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								String.valueOf(callData
										.get(CallDataAttribute.P_DIALOG_ID)),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing component ind event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error in tracing component ind event", t);
			}
		}
	}

	/**
	 * This method is used by protocol handler for sending dialogue request.
	 * 
	 * @param dre
	 *            represents the instance of DialogueReqEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @throws Exception
	 */
	static void sendDialogueReq(DialogueReqEvent dre, CallData callData,
			CallTraceService cCallTraceService) throws Exception {

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId;

		try {
			dialogId = (Integer) legData.get(LegDataAttributes.P_DIALOG_ID);

		} catch (Exception ex) {
			dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		}

		/*
		 * LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1); int
		 * dialogId = (Integer) legData.get(LegDataAttributes.P_DIALOG_ID);
		 */

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: Inside sendComponentReq");
		}

		incrementTcapCounters(dre);
		PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider()
		.sendDialogueReqEvent(dre);

		traceDialog(dre, callData, cCallTraceService);

	}

	/**
	 * This method will trace outgoing MAP Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error;
	 * 
	 * @param dre
	 *            represents the instance of DialogueReqEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */
	static void traceDialog(DialogueReqEvent dre, CallData callData,
			CallTraceService cCallTraceService) {
		try {
			String traceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);
			if (PhConstants.TRUE.equals(traceFlag)) {
				int primitive = dre.getPrimitiveType();

				StringBuilder traceMsg = new StringBuilder();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				traceMsg.append(">>>>Sending>>>>");
				traceMsg.append("\n");

				traceMsg.append("DIALOGUE::");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_BEGIN: {
					traceMsg.append("BEGIN");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_CONTINUE: {
					traceMsg.append("CONTINUE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_END: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					if (((EndReqEvent) dre).getTermination() == DialogueConstants.TC_PRE_ARRANGED_END) {
						traceMsg.append("true");
					} else {
						traceMsg.append("false");
					}
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_PROVIDER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("P-ABORT");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_USER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("U-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(Util.formatBytes(((UserAbortReqEvent) dre)
							.getUserAbortInformation()));
					traceMsg.append("\n");

					traceMsg.append("Generic Tcap Reason::");
					traceMsg.append(((UserAbortReqEvent) dre).getAbortReason());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_NOTICE: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("NOTICE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_UNIDIRECTIONAL: {
					traceMsg.append("UNIDIRECTIONAL");
					traceMsg.append("\n");
					break;
				}
				default: {
					traceMsg.append("CODE:" + primitive);
					traceMsg.append("\n");
					break;
				}
				}// @switch primitive

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								String.valueOf(callData
										.get(CallDataAttribute.P_DIALOG_ID)),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing Dialog req event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing dialogue event", t);
			}
		}

	}

	/**
	 * This method will trace incoming MAP Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error;
	 * 
	 * @param die
	 *            represents the instance of DialogueIndEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */
	static void traceDialog(DialogueIndEvent die, CallData callData,
			CallTraceService cCallTraceService) {

		try {

			String traceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);
			if (traceFlag.equals(PhConstants.TRUE)) {
				int primitive = die.getPrimitiveType();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				StringBuilder traceMsg = new StringBuilder();

				traceMsg.append("<<<<Recieved<<<<");
				traceMsg.append("\n");

				traceMsg.append("DIALOGUE::");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_BEGIN: {
					traceMsg.append("BEGIN");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_CONTINUE: {
					traceMsg.append("CONTINUE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_END: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					traceMsg.append("false");
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_END_PRE_ARRANGED: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					traceMsg.append("true");
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_PROVIDER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("P-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(((ProviderAbortIndEvent) die).getPAbort());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_USER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("U-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(Util.formatBytes(((UserAbortIndEvent) die)
							.getUserAbortInformation()));
					traceMsg.append("\n");

					traceMsg.append("Generic Tcap Reason::");
					traceMsg.append(((UserAbortIndEvent) die).getAbortReason());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_NOTICE: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("NOTICE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_UNIDIRECTIONAL: {
					traceMsg.append("UNIDIRECTIONAL");
					traceMsg.append("\n");
					break;
				}
				default: {
					traceMsg.append("CODE:" + primitive);
					traceMsg.append("\n");
					break;
				}
				}// @switch primitive

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								String.valueOf(callData
										.get(CallDataAttribute.P_DIALOG_ID)),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing Dialog ind event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing dialogue ind event", t);
			}
		}

	}

	/**
	 * This method will trace incoming MAP Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error;
	 * 
	 * @param die
	 *            represents the instance of DialogueIndEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */
	static void traceDialog(DialogueIndEvent die, CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside traceDialog");
		}
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();
		try {
			String traceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);

			if (traceFlag != null && traceFlag.equals(PhConstants.TRUE)) {
				int primitive = die.getPrimitiveType();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				StringBuilder traceMsg = new StringBuilder();

				traceMsg.append("<<<<Recieved<<<<");
				traceMsg.append("\n");

				traceMsg.append("DIALOGUE::");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_BEGIN: {
					traceMsg.append("BEGIN");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_CONTINUE: {
					traceMsg.append("CONTINUE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_END: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					traceMsg.append("false");
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_END_PRE_ARRANGED: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					traceMsg.append("true");
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_PROVIDER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("P-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(((ProviderAbortIndEvent) die).getPAbort());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_USER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("U-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(Util.formatBytes(((UserAbortIndEvent) die)
							.getUserAbortInformation()));
					traceMsg.append("\n");

					traceMsg.append("Generic Tcap Reason::");
					traceMsg.append(((UserAbortIndEvent) die).getAbortReason());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_NOTICE: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("NOTICE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_UNIDIRECTIONAL: {
					traceMsg.append("UNIDIRECTIONAL");
					traceMsg.append("\n");
					break;
				}
				default: {
					traceMsg.append("CODE:" + primitive);
					traceMsg.append("\n");
					break;
				}
				}

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					@SuppressWarnings("unchecked")
					List<Integer> constraintList = (List<Integer>) callData
					.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								String.valueOf(callData
										.get(CallDataAttribute.P_DIALOG_ID)),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing Dialog ind event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing dialogue ind event", t);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit traceDialog");
		}
	}

	/**
	 * This method performs the processing of Begin dialogue indication event.
	 * In case ANSI this method will be called in case of Query With Permission or
	 * Query without permission. 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @throws MandatoryParameterNotSetException
	 */
	static void processBegin(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) throws MandatoryParameterNotSetException {

		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] processBegin Enter ");
		}

		/*
		 * call data can't be null if it has passed validation method so moving
		 * ahead without null check store dialog
		 */

		if (dialogueIndEvent.isDialoguePortionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ ":: [PH] Set Dialogue Portion in call data");
			}

			try {
				/*
				 * Store Dialogue Portion in Leg Data.
				 */
				legData.set(LegDataAttributes.P_DIALOG_PORTION,
						dialogueIndEvent.getDialoguePortion());
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId
						+ ":: [PH] Error getting dialogue portion "
						+ e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId
							+ ":: [PH] Error getting dialog portion.", e);
					logger.info(dialogId
							+ "::[PH] IGNORE ParameterNotSetException fetching "
							+ "dialoguePortion from BEGIN dialogue event");
				}
			}
		}

		/*
		 * perform ACN check
		 */
		BeginIndEvent beginIndEvent = (BeginIndEvent) dialogueIndEvent;
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " ::[PH] isAppContextNamePresent = "
					+ beginIndEvent.isAppContextNamePresent());
		}
		if (beginIndEvent.isAppContextNamePresent()) {
			try {
				byte[] appContextName = beginIndEvent.getAppContextName();
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ " ::[PH] Application Context version from IDP = "
							+ Util.formatBytes(appContextName));
				}

				/*
				 * As isAppContextNamePresent always returns true even if
				 * appCOntextName is null making null check
				 */
				if (appContextName != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ " :: [PH] Application Context not null");
					}
					legData.set(
							LegDataAttributes.P_ACTIVITY_CONTEXT_VERSION,
							appContextName[appContextName.length - 1]);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ " :: [PH] Application Context is null");
					}

					/*
					 * valid acn handling is odne at service in test call only
					 * return handleInavlidAcn(tcapSession);
					 */
				}
			} catch (Exception e) {
				logger.warn(dialogId + ":: [PH] Error fetching AC version "
						+ e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId
							+ " :: [PH] Exception fetching AC Version", e);
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " :: [PH] Set Application Context version "
						+ legData
						.get(LegDataAttributes.P_ACTIVITY_CONTEXT_VERSION)
						+ " in call data");
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: isAppContextNamePresent is false");
			}
		}

		try {
			SccpUserAddress sccpUserAddress = beginIndEvent.getOriginatingAddress();
			SignalingPointCode signalingPointCode = sccpUserAddress
					.getSubSystemAddress().getSignalingPointCode();
			int zone = signalingPointCode.getZone();
			int cluster = signalingPointCode.getCluster();
			int member = signalingPointCode.getMember();
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] Origin zone= " + zone
						+ " cluster=" + cluster + " member=" + member);
			}

			if(zone==0 && cluster==0 && member==0) {
				logger.debug("0 found for zone , cluster and member replacing with mtp3Opc");
				SignalingPointCode spc =beginIndEvent.getMtp3Opc();
				zone= spc.getZone();
				cluster=spc.getCluster();
				member=spc.getMember();
			}
			String zcmFormat = zone + "-" + cluster + "-" + member;

			String pcBitStr = lPad(Integer.toBinaryString(zone), 3)
					+ lPad(Integer.toBinaryString(cluster), 8)
					+ lPad(Integer.toBinaryString(member), 3);

			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] pcBitStr =" + pcBitStr);
			}
			int pc = Integer.parseInt(pcBitStr, 2);
			legData.set(LegDataAttributes.P_OPC, pc);
			legData.set(LegDataAttributes.P_SPC, zcmFormat);
			
			String CD_PTY_NO_PC =(String) legData.get(LegDataAttributes.CD_PTY_NO_PC);
			String CG_PTY_NO_PC = (String) legData.get(LegDataAttributes.CG_PTY_NO_PC);
		SccpUserAddress destAddr=	beginIndEvent.getDestinationAddress();
		SccpUserAddress origAddr= beginIndEvent.getOriginatingAddress();
			if(StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"1")) {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to true in in orig addr endReqEvent");
				}
				destAddr.setPtyNoPC(true);
			}else {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to false in orig addr endReqEvent");
				}
				destAddr.setPtyNoPC(false);
			}
			if(StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"1")) {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to true in dest addr endReqEvent");
				}
				origAddr.setPtyNoPC(true);
			}else {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to false in dest addr endReqEvent");
				}
				origAddr.setPtyNoPC(false);
			}
			callData.set(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS,destAddr);
			callData.set(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS, origAddr);

			if (logger.isInfoEnabled()) {
				logger.info(dialogId
						+ " :: [PH] Calculated Origin Point Code form IDP-Begin ="
						+ legData.get(LegDataAttributes.P_OPC));
			}
		} catch (ParameterNotSetException e1) {
			logger.error(dialogId
					+ " :: [PH] Failed to get origin point code from Dialog Indication event. "
					+ e1.getMessage());
		}

		/*
		 * Use the sccp address received in IDP to support multiple pc-ssn
		 */
		legData.set(LegDataAttributes.P_SUA,
				beginIndEvent.getDestinationAddress());
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]:: processBegin Exit ");
		}
	}


	/**
	 * This method appends 0 as leading digits to input.  
	 * @param input 
	 * @param resultSize
	 * @return
	 */
	private static String lPad(String input, int resultSize) {
		if (input == null) {
			return input;
		}
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < (resultSize - input.length()); i++) {
			result.append("0");
		}
		result.append(input);
		return result.toString();
	}

	/**
	 * This method is for handling any unknown dialogue events. It just ignores
	 * unknown dialog events returning null.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action objects.
	 */
	static Action[] getUnknownDialogAction(TcapSession tcapSession) {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] Inside getUnknownDialogAction:: "
					+ "ignore unknown dialog events");
		}
		/*
		 * ignore unknown dialog events returning null.
		 */
		return null;

	}

	/**
	 * This method is for handling any unknown result events. It just ignores
	 * unknown Result opcode.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action objects.
	 */
	static Action[] getUnknownResultAction(TcapSession tcapSession) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getUnknownResultAction:: "
					+ "Drop call with U-Reject");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
		action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_RETURN_RESULT_UNEXPECTED);

		return (new Action[] { action });

	}


	/**
	 * This method returns action to be taken when an out of sequence invoke
	 * indication event is received.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param message
	 *            represents an instance of MESSAGE
	 * @return an array of Action Objects.
	 */
	static Action[] getInvalidInvokeIdAction(TcapSession tcapSession,
			MESSAGE message) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getInvalidInvokeIdAction with message::"
					+ message);
		}

		Action[] actionArr = null;

		switch (message) {
		case UERROR: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_UNRECOGNIZED_INVOKE_ID);
			actionArr = new Action[] { action };
			break;
		}
		case UREJECT: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_COMPONENT);
			actionArr = (new Action[] { action });
			break;
		}

		}// @end switch

		return actionArr;
	}

	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of type PRIMITIVE_REJECT is received and problem received is
	 * invalid.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param message
	 *            represents an instance of MESSAGE
	 * @return an array of Action Objects
	 */
	static Action[] getInvalidProblemAction(TcapSession tcapSession,
			MESSAGE message) {
		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]:: Inside getInvalidProblemAction with message::"
					+ message);
		}
		Action[] actionArr = null;

		switch (message) {
		case UREJECT: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_COMPONENT);
			actionArr = (new Action[] { action });
			break;
		}

		}// @end switch

		return actionArr;
	}

	/**
	 * Method to retrieve actions to be performed on encountering ASNParsing
	 * Exception.
	 * 
	 * @param invokeIndEvent
	 *            represents an instance of InvokeIndEvent
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param ape
	 *            represents an instance of ASNParsingException
	 * @return an array of Action objects.
	 */
	static Action[] getASNParsingFailureAction(InvokeIndEvent invokeIndEvent,
			TcapSession tcapSession, ASNParsingException ape) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: Inside handleASNParsingFailure with exception::"
					+ ape);
		}
		MESSAGE message = ape.getInapMessage();
		return getASNParsingFailureAction(tcapSession, message,
				ape.getParseFailType());

	}

	/**
	 * Method to retrieve actions to be performed on encountering ASNParsing
	 * Exception.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param message
	 *            represents an instance of MESSAGE
	 * @param failtype
	 *            represents an instance of FAILTYPE
	 * @return an array of Action objects.
	 */
	static Action[] getASNParsingFailureAction(TcapSession tcapSession,
			MESSAGE message, FAILTYPE failtype) {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH] :: Inside getASNParsingFailureAction with message::"
					+ message + "   failtype::" + failtype);
		}
		return (new Action[] { new Action(ActionType.ACTION_REJECT_REQUEST) });
	}




	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of type PRIMITIVE_REJECT is received.
	 * 
	 * @param rejectIndEvent
	 *            represents the instance of RejectIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	public static Action[] processReject(RejectIndEvent rejectIndEvent,
			TcapSession tcapSession) {

		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processReject");
		}
		int rejectType = -1;
		int problemType = -1;
		int problem = -1;


		/*
		 * get reject type
		 */
		if (rejectIndEvent.isRejectTypePresent()) {
			try {
				rejectType = rejectIndEvent.getRejectType();
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId + ":: Error in getting reject type "
						+ e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId + ":: Error getting reject type", e);
					logger.info(dialogId
							+ ":: IGNORE ParameterNotSetException in getting reject type.");
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Reject Type is [" + rejectType
						+ "]");
			}
		}

		int invokeId = -1;
		try {
			invokeId = rejectIndEvent.getInvokeId();
			/*
			 * get problem type and problem
			 */
			problemType = rejectIndEvent.getProblemType();
			problem = rejectIndEvent.getProblem();

		} catch (MandatoryParameterNotSetException mpne) {
			logger.error(dialogId
					+ ":: MandatoryParameterNotSetException in UREJECT", mpne);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					MapScfRelReasonCode.REJECT_MSG_MAND_PARAM_MISSING);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UREJECT,
					FAILTYPE.DEFAULT);
		} catch (ParameterNotSetException pne) {
			logger.error(dialogId + ":: ParameterNotSetException in UREJECT",
					pne);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					MapScfRelReasonCode.REJECT_MSG_PARAM_NOT_SET);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UREJECT,
					FAILTYPE.DEFAULT);
		}

		/*
		 * validateInvokeId
		 */

		int lastInvokeIdStart = MapScfProtocolUtil
				.getLastInvokeIdStartRange(callData);
		int lastInvokeIdEnd = MapScfProtocolUtil
				.getLastInvokeIdEndRange(callData);
		if ((invokeId < lastInvokeIdStart) || (invokeId > lastInvokeIdEnd)) {
			logger.error(dialogId + "::Invoke id invalid in U Reject; recived:"
					+ invokeId + " valid range for current message:: "
					+ lastInvokeIdStart + " to " + lastInvokeIdEnd);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					MapScfRelReasonCode.REJECT_INVALID_INVOK_ID);
			return getInvalidInvokeIdAction(tcapSession, MESSAGE.UREJECT);
		}

		/*
		 * validate problem type
		 */
		if (problemType != ComponentConstants.PROBLEM_TYPE_INVOKE) {
			logger.error(dialogId
					+ "::Problem type invalid in U reject; recived:" + invokeId
					+ " problemType:" + problemType + " Expected problem type:"
					+ ComponentConstants.PROBLEM_TYPE_INVOKE);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					MapScfRelReasonCode.REJECT_INVALID_PROBLEM_TYPE);
			return getInvalidProblemAction(tcapSession, MESSAGE.UREJECT);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Problem Type is [" + problemType
					+ "]  problem is  [" + problem + "]");
		}


		return null;
	}


	/**
	 * This method is used to parse SSIN message
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 * @throws Exception
	 */
	public static Action[] processSSInvocationNotification(
			InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {

		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processSSInvocationNotification");
		}

		try {
			MapScfProtocolParser.parseSSInvocationNotificationReq(invokeIndEvent, callData);

		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in NSDM.", ape);

			//to be uncommented KAVITA
			/*callData.set(
       					CallDataAttribute.NP_RELEASE_REASON_CODE,
       					InapCS1ScfRelReasonCode.SRR_ASN_PARSING_FAIL);
			 */return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in SS-Invoke.",
					pore);
			//to be uncommented KAVITA
			/*
       			callData.set(
       					CallDataAttribute.NP_RELEASE_REASON_CODE,
       					InapCS1ScfRelReasonCode.SRR_PARAM_OUT_OF_RANGE);
			 */     			return getOutOfRangeParamterAction(tcapSession, MESSAGE.SS_INVOKE);

		}
		if (logger.isDebugEnabled()) {
			logger.debug("Exiting processSSInvocationNotification() .....");
		}
		return null;

	}

	/**
	 * This method is used to parse NSDM message
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 * @throws Exception
	 */
	public static Action[] processNotifySubsDataModified(
			InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {

		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processNotifySubsDataModified");
		}

		try {
			MapScfProtocolParser.parseNSDM(invokeIndEvent, callData);

		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in NSDM.", ape);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					InapCS1ScfRelReasonCode.SRR_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in SRR.",
					pore);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					InapCS1ScfRelReasonCode.SRR_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.NSDM);

		}
		if (logger.isDebugEnabled()) {
			logger.debug("Exiting processNotifySubsDataModified() .....");
		}
		return null;

	}

	/**
	 * This method is called by protocol handler for sending error request
	 * event.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	static void sendErrorReqEvent(TcapSession tcapSession, Action action,
			CallTraceService cCallTraceService) throws Exception {
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendErrorReqEvent");
		}

		byte[] reason = new byte[] { 0x07 };
		if (action.getReleaseCauseValue() > 0) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " [PH]:: Got custom reason for u error::"
						+ action.getReleaseCauseValue());
			}
			reason = new byte[] { (byte) action.getReleaseCauseValue() };
		}

		ErrorReqEvent errorReqEvent = new ErrorReqEvent(src,
				tcapSession.getDialogueId(), ComponentConstants.ERROR_LOCAL,
				reason);

		errorReqEvent.setInvokeId(InapCS1ScfProtocolUtil
				.getLastRxInvokeId(callData));

		sendComponentReq(errorReqEvent, callData, cCallTraceService);
	}

	/**
	 * This method is called by protocol handler for sending Reject request
	 * event.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @throws Exception
	 */
	@SuppressWarnings("deprecation")
	private static void sendRejectReqEvent(TcapSession tcapSession,
			Action action, CallTraceService cCallTraceService) throws Exception {
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendRejectReqEvent");
		}

		RejectReqEvent rejectReqEvent = new RejectReqEvent(src);
		rejectReqEvent.setInvokeId(InapCS1ScfProtocolUtil
				.getLastRxInvokeId(callData));
		rejectReqEvent.setDialogueId(tcapSession.getDialogueId());

		/*
		 * reject type from service will always be user
		 */
		rejectReqEvent.setRejectType(ComponentConstants.REJECT_TYPE_USER);

		int problem = action.getReleaseCauseValue();

		rejectReqEvent.setProblem(problem);

		sendComponentReq(rejectReqEvent, callData, cCallTraceService);

	}

	public static void sendRejectRequest(TcapSession tcapSession,
			CallData callData, Action action, CallTraceService callTraceService) throws Exception {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " ::[PH] Send RETURN_ERROR with TC_END");
		}

		sendErrorReqEvent(tcapSession, action, callTraceService);
		sendEndRequestEvent(tcapSession, false, callTraceService);

	}

	/**
	 * This method is called by protocol handler for sending END request event.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param preArrangedEnd
	 *            represents the instance of boolean
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @throws Exception
	 */
	static void sendEndRequestEvent(TcapSession tcapSession,
			boolean preArrangedEnd, CallTraceService cCallTraceService)
					throws Exception {
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendEndRequestEvent");
		}

		EndReqEvent endReqEvent = new EndReqEvent(src,
				tcapSession.getDialogueId());

		if (preArrangedEnd) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Send Pre-arranged END");
			}

			endReqEvent.setTermination(DialogueConstants.TC_PRE_ARRANGED_END);
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Send Basic END");
			}

			endReqEvent.setTermination(DialogueConstants.TC_BASIC_END);
		}

		//added for Protcol Version changes
		HashMap<String,Integer> hashMap =  InapCS1ScfProtocolUtil.getProtocolVersionMap();
		String serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		if(hashMap.containsKey(serviceId)) {
			Integer value = hashMap.get(serviceId);
			endReqEvent.setProtocolVersion(value);
		}
		
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		DialoguePortion dialoguePortion = (DialoguePortion) legData
				.get(LegDataAttributes.P_DIALOG_PORTION);
		if (dialoguePortion != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Set Dialogue portion");
			}
			endReqEvent.setDialoguePortion(dialoguePortion);
		}

		String CD_PTY_NO_PC =(String) legData.get(LegDataAttributes.CD_PTY_NO_PC);
		String CG_PTY_NO_PC = (String) legData.get(LegDataAttributes.CG_PTY_NO_PC);
		SccpUserAddress destAddr = MapScfProtocolUtil.updateCalledAddress(callData, legData);
		SccpUserAddress origAddr=	MapScfProtocolUtil.updateCallingAddress(callData, legData);
		if(StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"1")) {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to true in in orig addr endReqEvent1");
			}
			destAddr.setPtyNoPC(true);
		}else {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to false in orig addr endReqEvent1");
			}
			destAddr.setPtyNoPC(false);
		}
		if(StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"1")) {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to true in dest addr endReqEvent1");
			}
			origAddr.setPtyNoPC(true);
		}else {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to false in dest addr endReqEvent1");
			}
			origAddr.setPtyNoPC(false);
		}
		endReqEvent.setOriginatingAddress(origAddr);
		endReqEvent.setDestinationAddress(destAddr);
	
		sendDialogueReq(endReqEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendEndRequestEvent");
		}

		MapScfProtocolUtil.setLastInvokeIdStartRange(
				MapScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1,
				callData);
		MapScfProtocolUtil.setLastInvokeIdEndRange(
				MapScfProtocolUtil.getLastInvokeId(callData), callData);
	}


	/**
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @param callTraceService
	 * @throws Exception
	 */
	public static void sendRequestResult(TcapSession tcapSession,
			CallData callData, Action action, CallTraceService callTraceService)
					throws Exception {

		if(logger.isDebugEnabled()){
			logger.debug("Inside sendRequestResult");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		byte opCode = MapOpCodes.MAP_SS_INVOCATION_NOTIFICATION_BYTE;
		if (MapOpCodes.MAP_SS_INVOCATION_NOTIFICATION.equals(legData
				.get(LegDataAttributes.NP_MAP_QUERY_TYPE))) {
			opCode = MapOpCodes.MAP_SS_INVOCATION_NOTIFICATION_BYTE;
		}
		sendResultRequestEvent(tcapSession, action, callTraceService, opCode);

		// call pre andpost processing before sending End Request 
		preProcessDroppedCall(tcapSession);
		sendEndRequestEvent(tcapSession, false, callTraceService);
		postProcessDroppedCall(tcapSession, true);

		if(logger.isDebugEnabled()){
			logger.debug("Exit sendRequestResult, sent End Dialogue");
		}

	}

	/**
	 * This method is used to send result of icoming map request
	 * @param tcapSession
	 * @param action
	 * @param cCallTraceService
	 * @throws Exception
	 */
	private static void sendResultRequestEvent(TcapSession tcapSession,Action action,
			CallTraceService cCallTraceService ,byte operation) throws Exception {
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendResultRequestEvent for opcode:" + operation);
		}

		//ResultReqEvent resultIndEvent = new ResultReqEvent(src);

		byte[] erOpCode = {operation};

		Operation nsdmOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, erOpCode);
		//		resultIndEvent.setOperation(nsdmOperation);
		//
		//		resultIndEvent.setInvokeId(InapCS1ScfProtocolUtil
		//				.getLastRxInvokeId(callData));

		ResultReqEvent resultRequestEvent = new ResultReqEvent(src, dialogId, true);
		resultRequestEvent.setOperation(nsdmOperation);
		resultRequestEvent.setInvokeId(InapCS1ScfProtocolUtil.getLastRxInvokeId(callData));
		resultRequestEvent.setLinkId(InapCS1ScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(resultRequestEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Exit sendResultRequestEvent");
		}
	}

	private static void incrementTcapCounters(EventObject eventObject) {
		MeasurementCounter measurementCounter = PhMeasurementService.getInstance().getMeasurementCounter(Protocol.MAP_SCF);
		if(measurementCounter == null) {
			return;
		}

		int primitiveType = -1;
		if(eventObject instanceof DialogueReqEvent) {
			primitiveType = ((DialogueReqEvent) eventObject).getPrimitiveType();
		}else if(eventObject instanceof ComponentReqEvent) {
			primitiveType = ((ComponentReqEvent) eventObject).getPrimitiveType();
		}

		SS7Message ss7Message = SS7Message.valueOf(primitiveType);

		switch(ss7Message) {

		case PRIMITIVE_BEGIN : {
			measurementCounter.incrementSS7MessageCount(SS7Message.PRIMITIVE_BEGIN, false);
			break;
		}

		case PRIMITIVE_END : {
			measurementCounter.incrementSS7MessageCount(SS7Message.PRIMITIVE_END, false);
			break;
		}

		case PRIMITIVE_USER_ABORT : {
			measurementCounter.incrementSS7MessageCount(SS7Message.PRIMITIVE_USER_ABORT, false);
			break;
		}
		}
	}

	/**
	 * This method marks call state, set disconnection time. Also cleans correlation
	 * timers. Invoke this method before sending termination message
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void preProcessDroppedCall(TcapSession tcapSession) {
		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside preProcessDroppedCall");
		}
		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);

		// Need to enable code once MAP and SIP based media server interaction is enabled. 
		//		if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
		//			MapScfProtocolUtil.cleanupCorrelationResources(tcapSession);
		//		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, MapCallStates.TERMINATED);
		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Exit preProcessDroppedCall");
		}
	}

	/**
	 * This method notifies service that call is dropped and executers actions
	 * reurned from service Also writes CDR after service notification. invoke thsi
	 * method afters ending termination mesage
	 * 
	 * @param tcapSession          represents the instance of TcapSession
	 * @param executeServiceAction represents the instance of boolean
	 */
	public static void postProcessDroppedCall(TcapSession tcapSession, boolean executeServiceAction) {

		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside postProcessDroppedCall");
		}
		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		/*
		 * Set the call state again just to handle the case where
		 * preProcessDroppedCall() is not called due to some exception during execution.
		 */
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERMINATED);

		/*
		 * Notify service that call is dropped
		 */
		notifyCallDropped(tcapSession, executeServiceAction);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Write CDR");
		}

		/*
		 * Set the call disconnect time again just to make sure that CDR is written
		 * properly using call disconnect time. This is to handle the case where
		 * preProcessDroppedCall() is not called due to some exception during execution.
		 */
		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());

		/*
		 * write CDR, CDR should be written after notifying service
		 */
		MapScfProtocolUtil.writeServiceCdr(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Exit postProcessDroppedCall");
		}
	}

	/**
	 * This method notify service that call is dropped.
	 * 
	 * @param tcapSession          represents the instance of TcapSession
	 * @param executeServiceAction represents the instance of boolean
	 */
	public static void notifyCallDropped(TcapSession tcapSession, boolean executeServiceAction) {
		int dialogueId = 0;
		try {
			dialogueId = tcapSession.getDialogueId();
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "[PH]:: Inside notifyCallDropped");
			}
			CallData callData = MapScfProtocolUtil.getCallData(tcapSession);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + " :: [PH] Notify service that call is dropped");
			}
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event event = new Event(EventType.EVENT_CALL_DROPPED, Protocol.AIN_SCF, null);

			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

		} catch (Exception ex) {
			logger.error(dialogueId + "::[PH] Error in notifyCallDropped " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(dialogueId + "::[PH] Error in processing notifyCallDropped", ex);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Exit notifyCallDropped");
		}
	}

}
