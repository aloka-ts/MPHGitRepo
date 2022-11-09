/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.


Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.ph.inapcs1scf.outgoingdialogue;

import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.src;

import java.util.LinkedList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.bn.exceptions.EnumParamOutOfRangeException;

import com.agnity.inapitutcs2.asngenerated.BearerCapability;
import com.agnity.inapitutcs2.asngenerated.CalledPartyNumber;
import com.agnity.inapitutcs2.asngenerated.ConnectArg;
import com.agnity.inapitutcs2.asngenerated.DestinationRoutingAddress;
import com.agnity.inapitutcs2.asngenerated.InitialDPArg;
import com.agnity.inapitutcs2.asngenerated.Integer4;
import com.agnity.inapitutcs2.asngenerated.ServiceKey;
import com.agnity.inapitutcs2.datatypes.CalledPartyNum;
import com.agnity.inapitutcs2.datatypes.CallingPartyNum;
import com.agnity.inapitutcs2.enumdata.CalgPartyCatgEnum;
import com.agnity.inapitutcs2.enumdata.IntNwNumEnum;
import com.agnity.inapitutcs2.enumdata.NatureOfAddEnum;
import com.agnity.inapitutcs2.enumdata.NumPlanEnum;
import com.agnity.inapitutcs2.enumdata.TransmissionMedReqEnum;
import com.agnity.inapitutcs2.exceptions.InvalidInputException;
import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.inapitutcs2.operations.InapOperationsCoding;
import com.agnity.inapitutcs2.util.NonAsnArg;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.InapCallStates;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.common.exception.ASNParsingException.FAILTYPE;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;
import com.agnity.ph.inapcs1scf.InapCS1ScfCallTraceHelper;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolConfig;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolHelper;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolParser;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil;
import com.agnity.ph.mapscf.MapScfProtocolConfig;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;
import jain.protocol.ss7.tcap.ComponentReqEvent;
import jain.protocol.ss7.tcap.DialogueReqEvent;
import jain.protocol.ss7.tcap.GTIndicator0010;
import jain.protocol.ss7.tcap.GlobalTitle;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.InvokeReqEvent;
import jain.protocol.ss7.tcap.component.Operation;
import jain.protocol.ss7.tcap.component.Parameters;
import jain.protocol.ss7.tcap.dialogue.BeginReqEvent;
import jain.protocol.ss7.tcap.dialogue.ContinueReqEvent;
import jain.protocol.ss7.tcap.dialogue.DialogueConstants;
import jain.protocol.ss7.tcap.dialogue.DialoguePortion;
import jain.protocol.ss7.tcap.dialogue.EndReqEvent;


/**
 * @author rarya
 *
 */
public class InapCS1ScfOutgoingDialogue {

	private static Logger logger = Logger.getLogger(InapCS1ScfOutgoingDialogue.class);

	/**
	 * Method used to send IDP
	 * @param tcapSession
	 * @param action
	 * @return
	 * @throws Exception
	 */
	public static void initiateIDP(TcapSession tcapSession, Action action) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside initiateIDP, actionMode:" + action.getConnectionMode());
		}
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.
				get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		InitialDPArg idpArg;

		try {
			byte[] idpByteArray = encodeIdp(callData);

			byte[] idpOpCode = {InapOpCodes.IDP_BYTE};

			// Send Component Request 
			sendOutgoingComponentReq(idpByteArray, idpOpCode, callData, tcapSession);

			sendOutgoingDialogue(tcapSession, 1, false);

			// send as Begin
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Error in encoding IDP " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Error in InapOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException("[PH]:: IDP Encoding failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Error in encoding IDP " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Exception in encoding IDP", e);
			}
			throw new ASNParsingException("[PH]:: IDP encoding failure occured.", e, MESSAGE.IDP);
		}
	}

	/**
	 * Method to create IDP
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] encodeIdp(CallData callData) throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.
				get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		if(logger.isDebugEnabled()){
			logger.debug(dialogueId + " Inside encodeIdp");
		}

		InitialDPArg idpArg = new InitialDPArg();

		// Encode Service Key - mandatory
		String sk = (String) leg2Data.get(LegDataAttributes.P_SERVICE_KEY);
		if(!StringUtils.isNotBlank(sk)){
			logger.error(dialogueId + " ServiceKey Not Defined by application for outgoing dialogue, using default 2400");
			sk = "2400";
		}

		ServiceKey skAsn = new ServiceKey();		
		skAsn.setValue(new Integer4(Integer.parseInt(sk)));
		idpArg.setServiceKey(skAsn);

		// encode Called Party Number 
		PhoneNumber destinationNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);
		NatureOfAddEnum natureOfAddrEnum = NatureOfAddEnum.fromInt(destinationNumber.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = NatureOfAddEnum.NATIONAL_NO;
		}

		// check if NOA is set to be inserted, APplication may send 1 or 4 (actual value)
		String noaInt = (String) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER_NOA_INT);
		if(StringUtils.isNotBlank(noaInt) && (StringUtils.equalsIgnoreCase(noaInt, "1") 
				|| StringUtils.equalsIgnoreCase(noaInt, "4"))){
			natureOfAddrEnum = NatureOfAddEnum.INTER_NO;

			if(logger.isDebugEnabled()){
				logger.debug("NOA of Destination number is set to International:4");
			}
		}

		NumPlanEnum numberPlan = NumPlanEnum.fromInt(destinationNumber.getNumberingPlan());
		if (numberPlan == null) {
			numberPlan = NumPlanEnum.ISDN_NP;
		}

		byte[] calledPartyNum = CalledPartyNum.encodeCaldParty(destinationNumber.getAddress(),
				natureOfAddrEnum, numberPlan, IntNwNumEnum.ROUTING_ALLWD);

		CalledPartyNumber calledPartyNumber = new CalledPartyNumber();
		calledPartyNumber.setValue(calledPartyNum);

		idpArg.setCalledPartyNumber(calledPartyNumber);

		// Encode IDP 
		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(InapOpCodes.IDP);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(idpArg);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}


	/**
	 * Method is used for sending component Request 
	 * @param operation
	 * @param opcode
	 * @param callData
	 * @throws Exception
	 */
	public static void sendOutgoingComponentReq(byte[] operationByteArray, byte[] opCode, 
			CallData callData, TcapSession tcapSession) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendOutgoingComponentReq, opcode:" + 
					CommonUtils.formatBytes(opCode) + ", buffer:" + 
					CommonUtils.formatBytes(operationByteArray));
		}

		int outgoingDlgId = 0; 
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		// check if outgoing dialogue ID need to be created or already created. 
		if(callData.get(CallDataAttribute.P_OUTGOING_DIALOG_ID) == null){
			outgoingDlgId = generateOutgoingDialogueID(callData, tcapSession);
		}else{
			outgoingDlgId = tcapSession.getOutgoingDialogueId();
		}

		Operation operation = new Operation(Operation.OPERATIONTYPE_LOCAL, opCode);

		InvokeReqEvent invokeReqEvent = new InvokeReqEvent(src, tcapSession.getOutgoingDialogueId(), operation);
		invokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
		invokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, operationByteArray));
		invokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		// Send Request 
		PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).
		getTcapProvider().sendComponentReqEvent(invokeReqEvent);

		// set call state as outgoing initiate call 

		InapCS1ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.OTG_INITIATE_CALL);

		InapCS1ScfCallTraceHelper.traceComponent(invokeReqEvent, callData);

		if(logger.isDebugEnabled()){
			logger.debug("Exiting sendOutgoingComponentReq");
		}
	}

	/**
	 * Method to send Dialogue (Begin, continue, end
	 * @param tcapSession
	 * @throws Exception
	 */
	public static void sendOutgoingDialogue(TcapSession tcapSession, 
			int dialogueType, boolean preArrangedEnd) throws Exception {

		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		SccpUserAddress sua = (SccpUserAddress) legData.get(LegDataAttributes.P_SUA);
		DialoguePortion dialoguePortion = (DialoguePortion) legData.get(LegDataAttributes.P_DIALOG_PORTION);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendOutgoingDialogue, type:" + dialogueType + 
					", end Type:" + ((preArrangedEnd==true)?"Pre-arranged":"Basic"));
		}

		DialogueReqEvent dlgRequestEvent = null;
		// 1 - Begin, 2 - Continue, 3- end
		if(dialogueType == 1){

			//Begin request would require SUA orig and remote. Currently it is assumed
			// that incoming leg is also INAP. In case incoming get is not same protocol 
			// like http to map then we would need to create TCAP session too.
			
			List<SccpUserAddress> inapScfSua = InapCS1ScfProtocolConfig.getInstance().getSccpLocalAddressList((String)callData.get(CallDataAttribute.SERVICE_ID));
			SccpUserAddress remoteSua = (SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS);			
			SccpUserAddress localSua = (SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS);

			if(inapScfSua.isEmpty()) {
				logger.error("Local MAP SCF not configured");
				throw new InvalidInputException("Local MAP SCF not configured, failed to create ATI request");
			}else{
				//localSua = inapScfSua.get(0); 
			}

			// check if remote SUA is null thenwrite logic to create remote SUA
			if(remoteSua ==null){
				remoteSua= createRemoteSUA(callData);
				logger.error("Send Begin: remote SUA is null.. todo");
			}

			if(logger.isDebugEnabled()){
				logger.debug("Local SUA = "+localSua+", Remote SUA = "+remoteSua);
			}

			dlgRequestEvent = new BeginReqEvent(src, tcapSession.getOutgoingDialogueId(), localSua, remoteSua);

		}else if(dialogueType == 2){
			dlgRequestEvent = new ContinueReqEvent(src, tcapSession.getOutgoingDialogueId());
			((ContinueReqEvent) dlgRequestEvent).setOriginatingAddress(sua);
			if (dialoguePortion != null) {
				((ContinueReqEvent) dlgRequestEvent).setDialoguePortion(dialoguePortion);
			}
		}else if(dialogueType == 3){
			dlgRequestEvent = new EndReqEvent(src, tcapSession.getOutgoingDialogueId());

			if (preArrangedEnd) {
				((EndReqEvent) dlgRequestEvent).setTermination(DialogueConstants.TC_PRE_ARRANGED_END);
			} else {
				((EndReqEvent) dlgRequestEvent).setTermination(DialogueConstants.TC_BASIC_END);
			}
		}

		InapCS1ScfProtocolHelper.incrementTcapCounters(dlgRequestEvent);
		PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
		.getTcapProvider().sendDialogueReqEvent(dlgRequestEvent);

		InapCS1ScfCallTraceHelper.traceDialog(dlgRequestEvent, callData, PhUtilityServices
				.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());

		InapCS1ScfProtocolUtil.setOutgoingLastInvokeIdStartRange(InapCS1ScfProtocolUtil.getOutgoingLastInvokeIdEndRange(callData) + 1, callData);
		InapCS1ScfProtocolUtil.setOutgoingLastInvokeIdEndRange(InapCS1ScfProtocolUtil.getOutgoingLastInvokeId(callData), callData);

		// reset dialout flag as there could be a scneario of calling next route call node
		callData.set(CallDataAttribute.P_DIALOUT, false);
		
		if(logger.isDebugEnabled()){
			logger.debug("Exiting sendOutgoingDialogue");
		}
	}

	/**
	 * @param callData
	 * @return
	 */
	private static SccpUserAddress createRemoteSUA(CallData callData) {
		SccpUserAddress remoteSua = null;

		// ROuting based on PC-SSN 
		if(true){
			int member   = 0;
			int cluster = 92;
			int zone    = 0;
			short ssn   = 197;

			SignalingPointCode spc = new SignalingPointCode(member, cluster, zone);
			SubSystemAddress ssa = new SubSystemAddress(spc, ssn);
			remoteSua = new SccpUserAddress(ssa);
		}else{ 
			// ROuting based on GTT
			@SuppressWarnings("deprecation")
			// GTT Type 2 - translationType and addressInformation
			GlobalTitle gtt = new GTIndicator0010((byte)0x0A, new byte[] {0x02, 0x00, 0x01, 0x00});
			remoteSua = new SccpUserAddress(gtt);
			remoteSua.setRoutingIndicator(jain.protocol.ss7.AddressConstants.ROUTING_GLOBALTITLE);
		}


		return remoteSua;
	}

	/**
	 * Method to generate outgoing dialogue id. 
	 * @param callData
	 * @return
	 */
	private static int generateOutgoingDialogueID(CallData callData, TcapSession tcapSession){
		// hard coded for testing 		
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		int correlateddlgId =  Integer.parseInt("99" + (Integer.toString(dialogueId)));

		if(logger.isDebugEnabled()){
			logger.debug("Actual dialogue ID:" + dialogueId + 
					", Outgoing dialogue id geenrated:" + correlateddlgId);
		}

		// set outgoing dialogue id in call data
		callData.set(CallDataAttribute.P_OUTGOING_DIALOG_ID, correlateddlgId);
		tcapSession.setOutgoingDialogueId(correlateddlgId);

		return correlateddlgId;
	}


	/**
	 * Handle Connect received as response of IDP send in new dialogue
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 * @throws Exception
	 */
	public static Action[] processConnect(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) throws Exception {
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogueId = invokeIndEvent.getDialogueId();

		if(logger.isDebugEnabled()){
			logger.debug("Connect, DlgIDininvokeIndEvent:" + dialogueId + 
					", dlgId In callData:"+ callData.get(CallDataAttribute.P_DIALOG_ID));
		}

		// this dialogue id shall be outgoing dialogue ID. Need to fetch original Dlg id. 
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);


		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extract Argument from Connect");
		}

		ConnectArg conArg = null;
		try {
			conArg = (ConnectArg) InapOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Error in decoding Connect " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Error in InapOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException("[PH]:: Connect parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Error in decoding Connect " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Exception in parse Connect", e);
			}
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, Connect parsing failure occured.", e, MESSAGE.IDP);
		}

		try {
			LinkedList<CalledPartyNumber> cldList = (LinkedList<CalledPartyNumber>) 
					conArg.getDestinationRoutingAddress().getValue();

			// fetch the first destination
			CalledPartyNumber calledPartyNumber = new CalledPartyNumber();
			calledPartyNumber = cldList.get(0);

			// called party number. 
			CalledPartyNum calledPartyNum = CalledPartyNum.decodeCaldParty(calledPartyNumber.getValue());
			PhoneNumber calledNumber = InapCS1ScfProtocolParser.parseCalledPartyNum(dialogueId, calledPartyNum);

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exit parseCalledPartyNum, Connect calledaprty:" + calledNumber);
			}

			leg2Data.set(LegDataAttributes.P_EXTERNAL_RN, calledNumber);
			
			InapCS1ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.SERVICE_LOGIC);
			
		} catch (InvalidInputException e) {
			logger.error("[PH]:: Error in parseIdp " + e.getMessage());
			throw new ASNParsingException("ASN Parsing Failure: IDP parsing failure occured.:", e, MESSAGE.IDP);
		} catch (Exception e) {
			logger.error("[PH]:: Error in parseIdp " + e.getMessage());
			throw new ASNParsingException("[PH]:: ASN Parsing Failure: IDP parsing failure occured.", e, MESSAGE.IDP);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Connect parsed successfully");
		}
		
		return null;
	}
	
	/**
	 * This method is called in case of outgoing dialogue 
	 * id especially if original call leg is ss7. While 
	 * generating dialogue id for Begin, application adds
	 * 99 to incoming dialogue id. This method check for 99 in incoming dialogue id
	 * and remove it so that original call leg can be hit. 
	 * @return
	 */
	public static int checkForCorrelatedDlgId(int dialogueId){
		int originalDlgId= 0;
		
		// check for outgoing dialogue. In case the dialogue id starts 
		// with 99 then fetch the original Dialogue id after removing 99 
		// from it 
        if(StringUtils.startsWith(Integer.toString(dialogueId), "99")){
        	originalDlgId = Integer.parseInt(StringUtils.substring(Integer.toString(dialogueId), 2));
        	
        	if(logger.isDebugEnabled()){
        		logger.debug("Incoming Dlg ID:" + dialogueId + " correlated DlgId:" + originalDlgId);
        	}
        }else{
    		originalDlgId= dialogueId;
        }
        
        return originalDlgId;
	}
	
	public static boolean isOutgoingDialogue(int dialogueId){
		boolean retVal = false;
		
		// check for outgoing dialogue. In case the dialogue id starts 
		// with 99 then fetch the original Dialogue id after removing 99 
		// from it 
        if(StringUtils.startsWith(Integer.toString(dialogueId), "99")){
        	retVal = true;
        	
        	if(logger.isDebugEnabled()){
        		logger.debug("Its outgoing dialogue");
        	}
        }
        return retVal;
	}
}
