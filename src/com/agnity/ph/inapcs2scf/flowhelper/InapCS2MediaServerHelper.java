package com.agnity.ph.inapcs2scf.flowhelper;

import com.agnity.inapitutcs2.asngenerated.*;
import com.agnity.inapitutcs2.datatypes.CalledPartyNum;
import com.agnity.inapitutcs2.datatypes.GenericDigits;
import com.agnity.inapitutcs2.datatypes.ScfId;
import com.agnity.inapitutcs2.enumdata.*;
import com.agnity.inapitutcs2.exceptions.InvalidInputException;
import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.inapitutcs2.operations.InapOperationsCoding;
import com.agnity.mphdata.common.*;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.CriticalityTypeException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.common.measurement.MeasurementCounter;
import com.agnity.ph.common.measurement.PhMeasurementService;
import com.agnity.ph.common.measurement.enums.MSConnectionMode;
import com.agnity.ph.inapcs2scf.InapCS2ScfProtocolConfig;
import com.agnity.ph.inapcs2scf.InapCS2ScfProtocolHelper;
import com.agnity.ph.inapcs2scf.InapCS2ScfProtocolParser;
import com.agnity.ph.inapcs2scf.InapCS2ScfProtocolUtil;
import com.agnity.ph.inapcs2scf.InapCS2ScfRelReasonCode;
import com.agnity.ph.inapcs2scf.messagehelper.InapCS2MSPromptHelper;
import com.agnity.ph.sip.SipProtocolConfig;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.tcap.component.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.apache.log4j.MDC;
import org.bn.exceptions.EnumParamOutOfRangeException;

import javax.servlet.sip.SipApplicationSession;

import java.util.LinkedList;

import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.returnTokenToSharedPool;
import static com.agnity.ph.inapcs2scf.InapCS2ScfProtocolHelper.sendComponentReq;
import static com.agnity.ph.inapcs2scf.InapCS2ScfProtocolHelper.sendContinueRequestEvent;
import static com.agnity.ph.inapcs2scf.InapCS2ScfProtocolUtil.*;
import static com.agnity.ph.sip.SipProtocolConfig.DFC_IN_SEPARATE_DIALOGUE;

/**
 * Created by ankitsinghal on 09/10/16.
 */
public class InapCS2MediaServerHelper {

	private static Logger logger = Logger.getLogger(InapCS2MediaServerHelper.class);

	/**
	 * This method is used to Send PlayAnnouncement to SS7 IP media server.
	 *
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendPlayAnnouncement(TcapSession tcapSession, CallData callData, Action action) throws Exception {
		LegData legData;
		String legSetInAction = action.getLeg();
		if (StringUtils.isNotEmpty(legSetInAction)) {
			legData = (LegData) callData.get(CallDataAttribute.valueOf(legSetInAction));
		} else {
			logger.warn("[PH]:: No leg set in action, using leg1 as default");
			legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendPlayAnnouncement");
		}

		byte[] playAnnouncement = InapCS2MSPromptHelper.createPlayAnnouncement((AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC));

		byte[] playAnnOpCode = {InapOpCodes.PA_BYTE};

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, playAnnOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, playAnnouncement));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData);
		sendContinueRequestEvent(tcapSession);

		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.USER_INTREACTION_IN_PROGRESS);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendPlayAnnouncement() .....");
		}
	}

	public static void playAndRecord(TcapSession tcapSession, CallData callData, Action action) {

	}

	/**
	 * This method is used to Send PlayAnnouncement to SS7 IP media server.
	 *
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendPromptAndCollectAnnouncement(TcapSession tcapSession, CallData callData, Action action) throws Exception {
		LegData legData;
		String legSetInAction = action.getLeg();
		if (StringUtils.isNotEmpty(legSetInAction)) {
			legData = (LegData) callData.get(CallDataAttribute.valueOf(legSetInAction));
		} else {
			logger.warn("[PH]:: No leg set in action, using leg1 as default");
			legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendPromptAndCollectAnnouncement");
		}

		byte[] promptAndCollectAnnouncement = InapCS2MSPromptHelper.createPromptAndCollectUserInformation((AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC));

		byte[] playAnnOpCode = {InapOpCodes.PAC_BYTE};

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, playAnnOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, promptAndCollectAnnouncement));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		//Setting PromptAndCollect Invoke ID to be used while processing result
		callData.set(CallDataAttribute.PROMPT_COLLECT_INVOKE_ID, connectInvokeReqEvent.getInvokeId());

		sendComponentReq(connectInvokeReqEvent, callData);
		sendContinueRequestEvent(tcapSession);

		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.USER_INTREACTION_IN_PROGRESS);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendPromptAndCollectAnnouncement() .....");
		}
	}

	/**
	 * This method is used by INAP PH to intiate a media server connection e.g. for ASIST flow
	 *
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void initiateMediaServerConnection(TcapSession tcapSession, CallData callData, Action action) throws Exception {

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		InapCallStates inapCallState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		boolean useSS7IP = false;

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside initiateMediaServerConnection with tcapSession");
			logger.debug("[PH]:: InapCallState is " + inapCallState);
			logger.debug("[PH]:: ConnectionMode is " + action.getConnectionMode().name());
		}

		// There could be case of connecting to media server for each PA or P&C. So that last event sent 
		// would be DFC which will transit state to MS_DISCONNECTED state.
		switch (inapCallState) {
		case SERVICE_LOGIC:
		case ASSIST: 
		case MS_DISCONNECTED: { 
			switch (action.getConnectionMode()) {
			case ASSIST: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Connection Mode is ASSIST. Send ETC");
				}
				InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.ASSIST);
				String correlationId = (String)callData.get(CallDataAttribute.P_CORRELATION_ID);


				String sharedPoolEnabled=InapCS2ScfProtocolConfig.getConfigData(InapCS2ScfProtocolConfig.SHARED_TOKEN_POOL_ENABLED);

				if (PhConstants.TRUE.equals(sharedPoolEnabled)) {

					correlationId = InapCS2ScfProtocolUtil.getTokenFromSharedPool(callData);

					if(correlationId == null) {
						logger.error("PRI number exhausted. Cleaning up call for DialogueID:" + tcapSession.getDialogueId());
					}
				}

				if (correlationId != null) {
					/*
					 * Setting dialouge-id for the correlation-id so that SAS
					 * can give INVITE of ETC to same thread where IDP were
					 * delivered. This has been done to avoid deadlock due to
					 * parallel processing of INAP and SIP in different thread.
					 */
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Save correlationId in correlationMap .." + correlationId);
					}
					PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap().put(correlationId, Integer.toString(tcapSession.getDialogueId()));
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Updated correlation map is  .." + PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap());
					}
					sendAssistEtc(tcapSession, action);
				} else {
					logger.warn("[PH]:: Correlation id is not set ..seems like deployment is pure SS7. " +
							"If deployment is pure SS7, use DIRECT_MS mode..");

					callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
					callData.set(CallDataAttribute.NP_ETC_SEND_FAILURE, 1);
					InapCS2ScfProtocolHelper.dropCall(tcapSession, callData);
				}
				MeasurementCounter measurementCounter = PhMeasurementService.getInstance().getMeasurementCounter(Protocol.ITUINAPCS2_SCF);
				if(measurementCounter != null) {
					measurementCounter.incrementMediaServerCount(MSConnectionMode.ASSIT);
				}
				
			}
			break;
			case B2BUA: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Connection Mode is B2BUA. Send connect for handoff");
				}
				InapCS2ScfProtocolHelper.sendConnectHandoff(tcapSession, action);
				InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.HANDOFF);
			}
			break;
			case DIRECT_MS: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Connection Mode is DIRECT_MS. Send CTR");
				}
				useSS7IP = true;
				sendConnectToResource(tcapSession, callData, action);
				MeasurementCounter measurementCounter = PhMeasurementService.getInstance().getMeasurementCounter(Protocol.ITUINAPCS2_SCF);
				if(measurementCounter != null) {
					measurementCounter.incrementMediaServerCount(MSConnectionMode.SS7);
				}
			}
			break;
			default: {
				logger.error("[PH]:: Invalid connection mode for connect ivr, drop call");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.INVALID_CONNMODE_ORIG_IVR);
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				InapCS2ScfProtocolHelper.dropCall(tcapSession, callData);
				return;
			}
			}
			if (!useSS7IP) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Start Correlation Timer");
				}

				InapCS2ScfProtocolUtil.startTimer(tcapSession, InapCS2ScfProtocolUtil.getCorrelationTime(callData), true, PhConstants.CORRELATION_TIMER);
				/*
				 * setting tcapsession id in corr map; not setting entire object
				 * to avoid ft issues
				 */
				String correlationId = (String) callData.get(CallDataAttribute.P_CORRELATION_ID);
				PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap().put(correlationId, tcapSession.getDialogueId());
				tcapSession.setAttribute(PhConstants.CORRELATION_ID, correlationId);
			}
		}
		break;
		default: {
			logger.error("[PH]:: Connect ivr invoked in invalid state, drop call");
			logger.error(inapCallState + " ");
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXP_ACT_CONIVR_ORIG);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			InapCS2ScfProtocolHelper.dropCall(tcapSession, callData);
		}
		}
	}

	/**
	 * Method to send ETC with continue in Assist mode.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 * @throws Exception
	 */

	public static void sendAssistEtc(TcapSession tcapSession, Action action) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendAssistEtc");
			logger.debug("[PH]:: Send ETC with CONTINUE");
		}

		sendEtc(tcapSession);
		sendContinueRequestEvent(tcapSession);

		String sendRTAfterETC=InapCS2ScfProtocolConfig.
				getConfigData(InapCS2ScfProtocolConfig.SEND_RESET_TIMER_AFTER_ETC);

		try {
			if (PhConstants.TRUE.equals(sendRTAfterETC)) {
				sendResetTimer(tcapSession);
				InapCS2ScfProtocolHelper.sendContinueRequestEvent(tcapSession);
			}
		}
		catch(Exception ex){
			logger.error("Error in processing resetTimer:"+ ex.getMessage());
		}
	}

	/**
	 * This method is used to connect SS7 IP media server.
	 *
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendConnectToResource(TcapSession tcapSession, CallData callData, Action action) throws Exception {
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendConnectToResource");
		}

		byte[] connect = createConnectForIP(callData, action.getLeg());

		byte[] connectOpCode = {InapOpCodes.CONNECT_TO_RESOURCE_BYTE};

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, connectOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, connect));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData);

		/*
		 * Currently setting state to MS connected as no success response comes
		 * for CTR PH need to notify application that MS has got connected now
		 * it can proceed with media operations later on PH will wait for error
		 * response of CTR if it comes. so PH will create a invoke operation
		 * timer of (10 secs ) timeout time of CTR . only then we will send
		 * connected event to app.
		 */
		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.MS_CONNECTED);
		Event event = new Event(Event.EventType.EVENT_MS_SUCCESS, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG1.name());
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Notify application that MS has got connected .");
		}
		ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendConnectToResource() .....");
		}
	}

	/**
	 * Method to send ETC component indication event.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @throws Exception
	 */
	private static void sendEtc(TcapSession tcapSession) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendEtc");
		}

		byte[] etc = createEtc(callData, (SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS));
		byte[] etcOpCode = {InapOpCodes.ETC_BYTE};

		Operation etcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, etcOpCode);

		InvokeReqEvent etcInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), etcOperation);
		etcInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		etcInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, etc));
		etcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(etcInvokeReqEvent, callData);

		// done to handle scenarios of multiple assist -- for future use
		tcapSession.removeAttribute(PhConstants.DFC_SENT);
	}

	/**
	 * This method is used to create connect for a IP
	 *
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createConnectForIP(CallData callData, String legId) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createConnectForIP");
		}

		LegData legData = null;
		PhoneNumber destinationNumber = null;
		IPRoutingAddress ipRoutingAddress = null;

		if (legId != null) {
			//Check if application has configured CTR_RESOURCE_ADDRESS in 
			// leg 
			legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
			if (legData != null && legData.get(LegDataAttributes.CTR_RESOURCE_ADDRESS) != null) {

				destinationNumber = (PhoneNumber) legData.get(LegDataAttributes.CTR_RESOURCE_ADDRESS);

				NatureOfAddEnum natureOfAddrEnum = NatureOfAddEnum.fromInt(destinationNumber.getNatureOfAddress());
				if (natureOfAddrEnum == null) {
					natureOfAddrEnum = NatureOfAddEnum.NATIONAL_NO;
				}
				NumPlanEnum numberPlan = NumPlanEnum.fromInt(destinationNumber.getNumberingPlan());
				if (numberPlan == null) {
					numberPlan = NumPlanEnum.ISDN_NP;
				}
				byte[] calledPartyNum = CalledPartyNum.encodeCaldParty(destinationNumber.getAddress(),
						natureOfAddrEnum,
						numberPlan,
						IntNwNumEnum.ROUTING_ALLWD);
				CalledPartyNumber calledPartyNumber = new CalledPartyNumber();
				calledPartyNumber.setValue(calledPartyNum);

				ipRoutingAddress = new IPRoutingAddress();
				ipRoutingAddress.setValue(calledPartyNumber);
			}
		}

		ConnectToResourceArg connectArg = new ConnectToResourceArg();
		ConnectToResourceArg.ResourceAddressChoiceType resourceAddressChoiceType = new ConnectToResourceArg.ResourceAddressChoiceType();

		// By default set none in case IPRouting address is not defined. 
		if(ipRoutingAddress != null) {
			resourceAddressChoiceType.selectIpRoutingAddress(ipRoutingAddress);
		} else {
			resourceAddressChoiceType.selectNone();
		}

		connectArg.setResourceAddress(resourceAddressChoiceType);

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(InapOpCodes.CONNECT_TO_RESOURCE);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(connectArg);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler for creating INAP ETC message.
	 *
	 * @param callData  represents an instance of CallData
	 * @param localAddr represents an instance of SccpUserAddress
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createEtc(CallData callData, SccpUserAddress localAddr) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createEtc");
		}

		/*
		 *  assistingSSPIPRoutingAddress
		 */
		String assistSspIpRoutAddrStr = null;
		if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
			assistSspIpRoutAddrStr = callData.get(CallDataAttribute.P_CORRELATION_ID).toString();
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Assisting SSP IP Routing Address formed as " + assistSspIpRoutAddrStr);
		}

		byte[] assistSspIpRoutAddrByteArr = com.agnity.inapitutcs2.datatypes.GenericNumber.encodeGenericNum(NumQualifierIndEnum.RESERVED, 
				assistSspIpRoutAddrStr, NatureOfAddEnum.NATIONAL_NO, 
				NumPlanEnum.ISDN_NP, AddPrsntRestEnum.PRSNT_ALLWD, 
				ScreeningIndEnum.USER_PROVD_NOT_VERFD, NumIncmpltEnum.COMPLETE);

		Digits assistSspIpRoutAddrDigits = new Digits(assistSspIpRoutAddrByteArr);
		AssistingSSPIPRoutingAddress assistSspIpRoutAddr = new AssistingSSPIPRoutingAddress();
		assistSspIpRoutAddr.setValue(assistSspIpRoutAddrDigits);

		EstablishTemporaryConnectionArg etcArg = new EstablishTemporaryConnectionArg();
		etcArg.setAssistingSSPIPRoutingAddress(assistSspIpRoutAddr);

		// Handling of Optional fields depending on cofnigurable parameters. 

		String sendCorrelationId=InapCS2ScfProtocolConfig.
				getConfigData(InapCS2ScfProtocolConfig.SHARED_TOKEN_POOL_ENABLED);

		if (PhConstants.FALSE.equals(sendCorrelationId)) {

			/*
			 * coding scheme should be odd or even decreateEtcpending on length of corrID
			 */
			String corrId = String.valueOf(callData.get(CallDataAttribute.P_CORRELATION_ID));
			EncodingSchemeEnum encodeSchemeEnum = null;
			if (corrId.length() % 2 == 0) {
				encodeSchemeEnum = EncodingSchemeEnum.BCD_EVEN;
			} else {
				encodeSchemeEnum = EncodingSchemeEnum.BCD_ODD;
			}

			byte[] correlationIdByteArr = GenericDigits.encodeGenericDigits(encodeSchemeEnum, DigitCatEnum.CORRELATION_ID, corrId);
			Digits correlationIdDigits = new Digits(correlationIdByteArr);

			CorrelationID correlationId = new CorrelationID();
			correlationId.setValue(correlationIdDigits);
			etcArg.setCorrelationID(correlationId);

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: correlationId is " + correlationId);
			}
		}

		// Check whether to send SCF and party to Connect params in ETC
		// By default not sent. 
		String sendEtcOptionalParams=InapCS2ScfProtocolConfig.
				getConfigData(InapCS2ScfProtocolConfig.SEND_OPTIONAL_PARAMS_ETC);

		if(PhConstants.TRUE.equals(sendEtcOptionalParams)) {

			// partyToConnect
			LegID leg1Id = new LegID();
			leg1Id.selectSendingSideID(leg1Type);

			EstablishTemporaryConnectionArg.PartyToConnectChoiceType etcPartyToConnect = new EstablishTemporaryConnectionArg.PartyToConnectChoiceType();
			etcPartyToConnect.selectLegID(leg1Id);
			etcArg.setPartyToConnect(etcPartyToConnect);

			// scfID
			byte[] scfIdByteArr = ScfId.encodeScfId(SPCIndicatorEnum.SPC_PRESENT, SSNIndicatorEnum.SSN_PRESENT, 
					GTIndicatorEnum.NO_GT, RoutingIndicatorEnum.ROUTING_PC_SSN, 
					localAddr.getSubSystemAddress().getSignalingPointCode().getZone(), 
					localAddr.getSubSystemAddress().getSignalingPointCode().getCluster(), 
					localAddr.getSubSystemAddress().getSignalingPointCode().getMember(), 
					localAddr.getSubSystemAddress().getSubSystemNumber());

			ScfID scfId = new ScfID(scfIdByteArr);
			etcArg.setScfID(scfId);

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: scfId is " + scfId);
			}
		}
		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(etcArg);
		opCode.add(InapOpCodes.ETC);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method execute the DISCONNECT_IVR action, requested by services. In
	 * case of an exception in disconnecting ivr connection, call is dropped
	 * with CV=41.
	 *
	 * @param action represents the instance of Action
	 * @throws Exception
	 */
	public static void disconnectIvr(TcapSession tcapSession, CallData callData, Action action) throws Exception {
		try {
			sendDfc(tcapSession, action);

			LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);

			// Override as per the instruction of application, if configured. By default its value is -1.
			// In case application wants to override the value then it should be other than -1. 
			// Possible values - 0: No to send DFC in separate Dialogue 
			//                   1: Send DFC in separate dialogue. 
			// Logic checks that is application has set this value then ignore configured value. 
			int overideSendingDfcInSeperateDlg = action.dfcToBeSentInSeperateDialogue();

			/*
			 * Checking here, if the only action sent by service was DisconnectMS and nothing else, then
			 * sending the event MS_DISCONNECTED to service
			 */
			Action[] serviceActions = (Action[]) callData.get(CallDataAttribute.SERVICE_REQUESTED_ACTION_ARRAY);
			if (!isActionExistsInArray(serviceActions, Action.ActionType.ACTION_END_CALL) &&
					((overideSendingDfcInSeperateDlg == -1) || (overideSendingDfcInSeperateDlg == 1)) ) {
				logger.debug("Sending the continue dialogue with DFC only..");
				sendContinueRequestEvent(tcapSession);

				ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(Event.EventType.EVENT_MS_DISCONNECT, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG1.name());
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			} else {
				logger.debug("Some other actions exists in the array, checking whether to send dialogue with DFC or not");
				Boolean dfcInSeparateDialogue = Boolean.valueOf(SipProtocolConfig.getConfigData(DFC_IN_SEPARATE_DIALOGUE));

				logger.debug("Application flag for handling DFC:"+overideSendingDfcInSeperateDlg 
						+ ", Configured through mph properties:" + dfcInSeparateDialogue);

				if (dfcInSeparateDialogue && overideSendingDfcInSeperateDlg == -1) {
					sendContinueRequestEvent(tcapSession);
				} else if (overideSendingDfcInSeperateDlg > 0){
					sendContinueRequestEvent(tcapSession);
				} else {
					logger.debug("Not sending the dialogue right now as per the configuration..");
				}
			}
		} catch (Exception ex) {
			logger.error("[PH]:: Failed to disconnect ivr. Error is " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Error in disconnecting orig ivr.", ex);
			}
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.EXCEP_DISCON_ORIG_IVR);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			InapCS2ScfProtocolHelper.dropCall(tcapSession, callData);
		}
	}

	/**
	 * This method is used to send Disconnect forward connection to disconnect an
	 * IVR connection
	 *
	 * @param tcapSession
	 * @throws Exception
	 */
	public static void sendDfc(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendDfc");
		}

		returnTokenToSharedPool(callData);

		// There could be a case where application may want to connect to IVR multiple time
		// which would lead to sending DFC multiple time. Therefore we need to unset DFC_SENT
		// flag. Right now checking based on flag set in action by an application 
		if (action.dfcToBeSentInSeperateDialogue() != -1) {
			tcapSession.removeAttribute(PhConstants.DFC_SENT);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]::Removing DFC_SENT from tcapSession as need to send it again " 
						+ action.dfcToBeSentInSeperateDialogue());
			}
		}

		if (tcapSession.getAttribute(PhConstants.DFC_SENT) == null) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send DFC");
			}
			byte[] dfcOpCode = {InapOpCodes.DFC_WITHOUT_ARGS_BYTE};

			Operation dfcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, dfcOpCode);

			InvokeReqEvent dfcInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), dfcOperation);
			dfcInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
			dfcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

			sendComponentReq(dfcInvokeReqEvent, callData);
			tcapSession.setAttribute(PhConstants.DFC_SENT, PhConstants.TRUE);

			/*
			 * ASSIST INVITE leg should be cleaned if DFC has not been sent
			 */
			SipApplicationSession sipApplicationSession = InapCS2ScfProtocolUtil.getAssistAppSession(tcapSession,
					PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());
			if (sipApplicationSession != null) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Setting DFC_SENT flag in appSessionId: " + sipApplicationSession.getId());
				}
				sipApplicationSession.setAttribute(PhConstants.DFC_SENT, PhConstants.TRUE);
			}
			InapCS2ScfProtocolUtil.updateSS7CallState(origLegData, InapCallStates.MS_DISCONNECTED);
		}
	}

	public static void parsePACResult(ResultIndEvent resultIndEvent, CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extract ReceivedInformation Argument from ResultIndEvent");
		}
		ReceivedInformationArg receivedInformationArg;
		try {
			receivedInformationArg = (ReceivedInformationArg) InapOperationsCoding.decodeOperation(resultIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Error in decoding ReceivedInformation " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Error in InapOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException("[PH]:: ReceivedInformation parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Error in decoding ReceivedInformation " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Exception in parse ReceivedInformation", e);
			}
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, ReceivedInformation parsing failure occured.", e, ASNParsingException.MESSAGE.IDP);
		}
		try {
			String digitsCollected = null;
			if (null != receivedInformationArg && null != receivedInformationArg.getDigitsResponse()) {
				digitsCollected = GenericDigits.decodeGenericDigits(receivedInformationArg.getDigitsResponse().getValue()).getDigits();
				if (logger.isDebugEnabled()) {
					logger.debug("Digits collected: " + digitsCollected);
				}
			} else {
				logger.warn("No digits found in the input!");
			}
			legData.set(LegDataAttributes.P_COLLECTED_DIGITS, digitsCollected);
		} catch (InvalidInputException e) {
			logger.error("[PH]:: Error in ReceivedInformation " + e.getMessage());
			throw new ASNParsingException("ASN Parsing Failure: ReceivedInformation parsing failure occured.:", e, ASNParsingException.MESSAGE.IDP);
		} catch (Exception e) {
			logger.error("[PH]:: Error in parsePACResult " + e.getMessage());
			throw new ASNParsingException("[PH]:: ASN Parsing Failure: ReceivedInformation parsing failure occured.", e, ASNParsingException.MESSAGE.IDP);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: ReceivedInformation parsed successfully");
		}
	}

	public static Action[] processSRR(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside processSRR");
		}
		try {
			InapCS2ScfProtocolParser.parseSRR(invokeIndEvent, callData);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Notify service for PLAY_SUCCESS_EVENT");
			}
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Action lastAction = (Action) callData.get(CallDataAttribute.P_LAST_CALL_ACTION);
			if (null != lastAction) {
				if (lastAction.getActionType() == Action.ActionType.ACTION_PLAY) {
					Event event = new Event(Event.EventType.EVENT_PLAY_SUCCESS, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG1.name());
					ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
				} else {
					logger.error("Last action requested by service: " + lastAction + " is not supported on SRR receival! Ignoring SRR..");
				}
			} else {
				logger.error("Last action requested by service was null! Ignoring SRR!");
			}
		} catch (ASNParsingException ape) {
			logger.error("[PH]:: ASN pasring Exception in SRR.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.SRR_ASN_PARSING_FAIL);
			return InapCS2ScfProtocolHelper.getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error("[PH]:: ParameterOutOfRangeException in SRR.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.SRR_PARAM_OUT_OF_RANGE);
			return InapCS2ScfProtocolHelper.getOutOfRangeParamterAction(tcapSession, ASNParsingException.MESSAGE.ACR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting processSRR() .....");
		}
		return null;
	}

	/**
	 * Method to send DFC with continue in Assist mode.
	 * MDC logging is needed here because in case of Assist, Dialogue id is not set in MDC by SipProtocol.
	 * @param tcapSession represents the instance of TcapSessio
	 * @throws Exception
	 */

	public static void sendAssistDfc(TcapSession tcapSession, Action action) throws Exception {
		try {
			CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
			MDC.put(PhConstants.MDC_CALL_ID_CONST, "[" + callData.get(CallDataAttribute.P_DIALOG_ID) + "]");
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Inside sendAssistDfc");
				logger.debug("[PH]:: Send DFC with CONTINUE");
			}
			sendDfc(tcapSession, action);
			sendContinueRequestEvent(tcapSession);
		} finally {
			MDC.remove(PhConstants.MDC_CALL_ID_CONST);
		}
	}

	/**
	 * Method to send ResetTimer component indication event.
	 * ResetTimer shall follow ETC in order to stop timer for IDP. The timer value shall 
	 * be read from mph properties. 
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @throws Exception
	 */
	private static void sendResetTimer(TcapSession tcapSession) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendResetTimer");
		}

		byte[] resetTimer = createResetTimer(callData, (SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS));
		byte[] rtOpcode = {InapOpCodes.RESET_TIMER_BYTE};

		Operation rtOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rtOpcode);

		InvokeReqEvent rtInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rtOperation);
		rtInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		rtInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, resetTimer));
		rtInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(rtInvokeReqEvent, callData);
	}

	/**
	 * This method is called by protocol handler for creating INAP ResetTimer message.
	 *
	 * @param callData  represents an instance of CallData
	 * @param localAddr represents an instance of SccpUserAddress
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createResetTimer(CallData callData, SccpUserAddress localAddr) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createResetTimer");
		}

		ResetTimerArg rtArg = new ResetTimerArg();

		String resetTimerValue=InapCS2ScfProtocolConfig.
				getConfigData(InapCS2ScfProtocolConfig.RESET_TIMER_VALUE);

		// TimerID
		TimerID t = new TimerID();
		t.setValue(TimerID.EnumType.tssf);
		rtArg.setTimerID(t);

		// Timer Value
		Integer timerIdValue = 300;
		if(StringUtils.isNotEmpty(resetTimerValue)) {
			timerIdValue = Integer.valueOf(resetTimerValue);
		}

		TimerValue timeValueParam = new TimerValue();
		Integer4 int4 = new Integer4(timerIdValue);
		timeValueParam.setValue(int4);

		rtArg.setTimervalue(timeValueParam);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(rtArg);
		opCode.add(InapOpCodes.RESET_TIMER);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}
}
