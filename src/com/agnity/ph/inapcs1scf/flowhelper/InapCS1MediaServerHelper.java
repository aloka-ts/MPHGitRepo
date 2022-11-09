package com.agnity.ph.inapcs1scf.flowhelper;

import com.agnity.inapitutcs2.asngenerated.*;
import com.agnity.inapitutcs2.datatypes.CalledPartyNum;
import com.agnity.inapitutcs2.datatypes.GenericDigits;
import com.agnity.inapitutcs2.datatypes.ScfId;
import com.agnity.inapitutcs2.enumdata.*;
import com.agnity.inapitutcs2.exceptions.InvalidInputException;
import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.inapitutcs2.operations.InapOperationsCoding;
import com.agnity.mphdata.common.*;
import com.agnity.ph.common.CommonUtils;
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
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolConfig;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolHelper;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolParser;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil;
import com.agnity.ph.inapcs1scf.InapCS1ScfRelReasonCode;
import com.agnity.ph.inapcs1scf.messagehelper.InapCS1ERBSetHelper;
import com.agnity.ph.inapcs1scf.messagehelper.InapCS1MSPromptHelper;
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

import static com.agnity.mphdata.common.InapCallStates.NULL;
import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolHelper.sendComponentReq;
import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.*;
import static com.agnity.ph.sip.SipProtocolConfig.DFC_IN_SEPARATE_DIALOGUE;

/**
 * Created by ankitsinghal on 09/10/16.
 */
public class InapCS1MediaServerHelper {

	private static Logger logger = Logger.getLogger(InapCS1MediaServerHelper.class);

	/**
	 * This method is wrapper on sendPlayAnnouncement and SendPlayConnectAnnouncement. 
	 * This handles FSM_RRBCSM_FCI_CON_SEPARATE specific handling.  
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @param operation
	 * @throws Exception
	 */
	public static void performIVRInteraction(TcapSession tcapSession, CallData callData, 
			Action action, int operation) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside performIVRInteraction, operation:" + ((operation==1)?"Play":"Prompt&Collect"));
		}

		// in case of fsmModel is enabled then we need to send RRBCSM, FCI and CTR+PlayANn as seperate dialogue
		String fsmModel = InapCS1ScfProtocolConfig
				.getConfigData(InapCS1ScfProtocolConfig.FSM_RRBCSM_FCI_CON_SEPARATE);

		// In this FSM model - Order of sending will be RRBCSM, FCI and CONNECT and 
		// each shall be sent as separate dialogue. 
		if (PhConstants.TRUE.equals(fsmModel)) {
			// send RRBCSM - need to send oAbandon and oDisconnect for leg1
			// followed by FCI 
			// CTR + Play or CTR + PlayCOllect
			LinkedList<BCSMEvent> bcsmEventList = new LinkedList<BCSMEvent>();
			// oDisconnect
			BCSMEvent bcsmEvent = InapCS1ERBSetHelper.getBCSMEvent(
					EventTypeBCSM.EnumType.oDisconnect,
					MonitorMode.EnumType.interrupted,
					1);

			bcsmEventList.add(bcsmEvent);
			// oAbandon
			bcsmEvent = InapCS1ERBSetHelper.getBCSMEvent(
					EventTypeBCSM.EnumType.oAbandon,
					MonitorMode.EnumType.interrupted,
					1);
			bcsmEventList.add(bcsmEvent);


			InapCS1BCSMHelper.sendRRBCSMForArming(tcapSession, action, bcsmEventList);
			InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);

			// Send FCI as continue
			InapCS1ScfProtocolHelper.sendFciAsDialogue(tcapSession);

			// Send CTR + Play Announcement 
			sendSpecificConnectToResource(tcapSession, callData, action);

			if(operation == PhConstants.PLAY_OPERATION){
				sendPlayAnnouncement(tcapSession, callData, action);
			}else{
				sendPromptAndCollectAnnouncement(tcapSession, callData, action);
			}
		}else{
			// Play Announcement
			if(operation == PhConstants.PLAY_OPERATION){
				sendPlayAnnouncement(tcapSession, callData, action) ;
			}else{
				sendPromptAndCollectAnnouncement(tcapSession, callData, action);
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit performIVRInteraction");
		}
	}

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

		byte[] playAnnouncement = InapCS1MSPromptHelper.createPlayAnnouncement((AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC));

		byte[] playAnnOpCode = {InapOpCodes.PA_BYTE};

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, playAnnOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, playAnnouncement));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData);
		InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);

		InapCS1ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.MS_PLAY);

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

		byte[] promptAndCollectAnnouncement = InapCS1MSPromptHelper.createPromptAndCollectUserInformation((AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC));

		byte[] playAnnOpCode = {InapOpCodes.PAC_BYTE};

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, playAnnOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, promptAndCollectAnnouncement));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		//Setting PromptAndCollect Invoke ID to be used while processing result
		callData.set(CallDataAttribute.PROMPT_COLLECT_INVOKE_ID, connectInvokeReqEvent.getInvokeId());

		sendComponentReq(connectInvokeReqEvent, callData);
		InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);

		InapCS1ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.MS_PLAYCOLLECT);

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
		switch (inapCallState) {
		case SERVICE_LOGIC:
		case ASSIST: 
		case MS_DISCONNECTED: {
			switch (action.getConnectionMode()) {
			case ASSIST: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Connection Mode is ASSIST. Send ETC");
				}
				InapCS1ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.ASSIST);
				String correlationId = (String)callData.get(CallDataAttribute.P_CORRELATION_ID);

				callData.set(CallDataAttribute.P_ASSIST_IND, 1);

				String sharedPoolEnabled=InapCS1ScfProtocolConfig.getConfigData(InapCS1ScfProtocolConfig.SHARED_TOKEN_POOL_ENABLED);

				if (PhConstants.TRUE.equals(sharedPoolEnabled)) {

					correlationId = InapCS1ScfProtocolUtil
							.getTokenFromSharedPool(callData);

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

					MeasurementCounter measurementCounter = PhMeasurementService.getInstance().getMeasurementCounter(Protocol.ITUINAPCS1_SSF);
					if(measurementCounter != null) {
						measurementCounter.incrementMediaServerCount(MSConnectionMode.ASSIT);
					}
				} else {
					logger.warn("[PH]:: Correlation id is not set ..seems like deployment is pure SS7. " +
							"If deployment is pure SS7, use DIRECT_MS mode..");

					callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
					callData.set(CallDataAttribute.NP_ETC_SEND_FAILURE, 1);
					InapCS1ScfProtocolHelper.dropCall(tcapSession, callData);
				}
			}
			break;
			case B2BUA: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Connection Mode is B2BUA. Send connect for handoff");
				}
				InapCS1ScfProtocolHelper.sendConnectHandoff(tcapSession, action);
				InapCS1ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.HANDOFF);
			}
			break;
			case DIRECT_MS: {

				// In case of an optus, FSM_RRBCSM_FCI_CON_SEPERATE should be enabled. In case
				// if flag is on then we need to send CTR and Play together. So we will send 
				// MS_CONNECTED first
				String fsmModel = InapCS1ScfProtocolConfig
						.getConfigData(InapCS1ScfProtocolConfig.FSM_RRBCSM_FCI_CON_SEPARATE);

				// no need to start correlation timer
				useSS7IP = true;

				// In this FSM model - Order of sending will be RRBCSM, FCI and CONNECT and 
				// each shall be sent as separate dialogue. 
				if (PhConstants.TRUE.equals(fsmModel)) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Connection Mode is DIRECT_MS. Send MS_CONNECTED");
					}

					// remove any correlation key if stored.
					if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
						if (logger.isDebugEnabled()) {
							logger.debug("DirectMS scenario, correlation Id already set, removing it:"
									+ (String) callData.get(CallDataAttribute.P_CORRELATION_ID));
						}
						callData.remove(CallDataAttribute.P_CORRELATION_ID);
					}

					/*
					 * Since this is the request from an application for IVR connection (MS_CONNECT), 
					 * and CTR needs to be sent on next action MS_PLAY or MS_PLAYCOLECT therefore it 
					 * returns MS_SUCCESS from here.
					 */
					legData.set(LegDataAttributes.P_LEG_SS7_STATE, InapCallStates.MS_CONNECTED);
					Event event = new Event(Event.EventType.EVENT_MS_SUCCESS, Protocol.ITUINAPCS1_SCF,
							CallDataAttribute.P_LEG1.name());
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Notify application MS Connected loopback .");
					}
					ServiceInterface serviceHandler = PhUtilityServices
							.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
					ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Returning after MS_CONNECT execution");
					}
				}else{
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Connection Mode is DIRECT_MS. Send CTR");
					}
					useSS7IP = true;
					sendConnectToResource(tcapSession, callData, action);
					MeasurementCounter measurementCounter = PhMeasurementService.getInstance().getMeasurementCounter(Protocol.ITUINAPCS1_SSF);
					if(measurementCounter != null) {
						measurementCounter.incrementMediaServerCount(MSConnectionMode.SS7);
					}
				}
			}
			break;
			default: {
				logger.error("[PH]:: Invalid connection mode for connect ivr, drop call");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS1ScfRelReasonCode.INVALID_CONNMODE_ORIG_IVR);
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				InapCS1ScfProtocolHelper.dropCall(tcapSession, callData);
				return;
			}
			}
			if (!useSS7IP) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Start Correlation Timer");
				}

				InapCS1ScfProtocolUtil.startTimer(tcapSession, InapCS1ScfProtocolUtil.getCorrelationTime(callData), true, PhConstants.CORRELATION_TIMER);
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
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS1ScfRelReasonCode.UNEXP_ACT_CONIVR_ORIG);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			InapCS1ScfProtocolHelper.dropCall(tcapSession, callData);
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
		}

		String fsmModel = InapCS1ScfProtocolConfig
				.getConfigData(InapCS1ScfProtocolConfig.FSM_RRBCSM_FCI_CON_SEPARATE);

		// In this FSM model - Order of sending will be RRBCSM, FCI and ETC and 
		// each shall be sent as separate dialogue. 
		if (PhConstants.TRUE.equals(fsmModel)) {
			// check if send mode if contiue then send RRBCSM
			if(action.getSendMode() == Action.SEND_MODE.CONTINUE){
				// Request Report BCSM Event
				InapCS1BCSMHelper.sendRRBCSMForArming(tcapSession, action, null);
				InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);
			}

			// Send FCI as continue
			InapCS1ScfProtocolHelper.sendFciAsDialogue(tcapSession);
		}

		// send ETC
		sendEtc(tcapSession);
		InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);

		// stopping sending Reset Timer in case of fsmModel is true. 
		if (!PhConstants.TRUE.equals(fsmModel)) {
			String sendRTAfterETC=InapCS1ScfProtocolConfig.
					getConfigData(InapCS1ScfProtocolConfig.SEND_RESET_TIMER_AFTER_ETC);


			try {
				if (PhConstants.TRUE.equals(sendRTAfterETC)) {
					sendResetTimer(tcapSession);
					InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);
				}
			}
			catch(Exception ex){
				logger.error("Error in processing resetTimer:"+ ex.getMessage());
			}
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
		connectInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
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
		InapCS1ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.MS_CONNECTED);
		Event event = new Event(Event.EventType.EVENT_MS_SUCCESS, Protocol.ITUINAPCS1_SCF, CallDataAttribute.P_LEG1.name());
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
	 * This method is to send Connect To Resource specific to Optus
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendSpecificConnectToResource(TcapSession tcapSession, CallData callData, Action action) throws Exception {
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendSpecificConnectToResource");
		}

		// customized CTR
		byte[] connect = {0x30, 0x05, (byte) 0xa1, 0x03, (byte) 0x80, 0x01, 0x01};

		byte[] connectOpCode = {InapOpCodes.CONNECT_TO_RESOURCE_BYTE};

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, connectOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, connect));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendSpecificConnectToResource()");
		}
	}

	/**
	 * Method to send ETC component indication event.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @throws Exception
	 */
	private static void sendEtc(TcapSession tcapSession) throws Exception {
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendEtc");
		}

		byte[] etc = createEtc(callData, (SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS));
		byte[] etcOpCode = {InapOpCodes.ETC_BYTE};

		Operation etcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, etcOpCode);

		InvokeReqEvent etcInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), etcOperation);
		etcInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
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
	 * @param legId 
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

		String sendCorrelationId=InapCS1ScfProtocolConfig.
				getConfigData(InapCS1ScfProtocolConfig.SHARED_TOKEN_POOL_ENABLED);

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
		String sendEtcOptionalParams=InapCS1ScfProtocolConfig.
				getConfigData(InapCS1ScfProtocolConfig.SEND_OPTIONAL_PARAMS_ETC);

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
		if(logger.isDebugEnabled()){
			logger.debug("Inside disconnectIvr");
		}

		try {
			LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			InapCallStates ss7CallState = NULL;
			ss7CallState = (InapCallStates) origLegData.get(LegDataAttributes.P_LEG_SS7_STATE);

			int assistVal=0;
			if(callData.get(CallDataAttribute.P_ASSIST_IND) != null){
				assistVal = (int)callData.get(CallDataAttribute.P_ASSIST_IND);
			}
			
			if(logger.isDebugEnabled()){
				logger.debug("In disconnect IVR InapCallState"+ss7CallState
						+" ,ASSIST Indiaction set as::"+assistVal);
			}
			// in case of fsmModel is enabled then we need to send RRBCSM, FCI and CTR+PlayANn as seperate dialogue
			String fsmModel = InapCS1ScfProtocolConfig
					.getConfigData(InapCS1ScfProtocolConfig.FSM_RRBCSM_FCI_CON_SEPARATE);

			// In this FSM model - Order of sending will be RRBCSM, FCI and CONNECT and 
			// each shall be sent as separate dialogue. 
			if (PhConstants.TRUE.equals(fsmModel)) {
				// Send FCI as continue
				if(assistVal != 1){
					//FCI will not send in assit flow
					InapCS1ScfProtocolHelper.sendFciAsDialogue(tcapSession);
				}

				// send DFC
				sendDfc(tcapSession, action);

				// if ACTION_END is there is send array then we need to send DFC as TC_END 
				// else continue
				Action[] serviceActions = (Action[]) callData.get(CallDataAttribute.SERVICE_REQUESTED_ACTION_ARRAY);
				if (isActionExistsInArray(serviceActions, Action.ActionType.ACTION_END_CALL) ||
						action.getSendMode() == Action.SEND_MODE.END) {
					logger.debug("Sending the End dialogue with DFC ..");
					InapCS1ScfProtocolHelper.sendEndRequestEvent(tcapSession, false);

					// since we have already send End Request, drop call should not send anything
					// explicitly send mode to NONE in ACTION_END 
					InapCS1ScfProtocolUtil.updateSS7CallState(origLegData, InapCallStates.MS_DISCONNECTED_END);

				}else if(ss7CallState== InapCallStates.ASSIST){
					if(logger.isDebugEnabled()){
						logger.debug("Sending the End dialogue with DFC .. and waiting for Response");
					}
					InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);
				}
				else{
					InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);
					logger.debug("Sending the Continue dialogue with DFC ..");

					ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
					Event event = new Event(Event.EventType.EVENT_MS_DISCONNECT, Protocol.ITUINAPCS1_SCF, CallDataAttribute.P_LEG1.name());
					ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
				}
			}else{
				sendDfc(tcapSession, action);

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
					InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);

					ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
					Event event = new Event(Event.EventType.EVENT_MS_DISCONNECT, Protocol.ITUINAPCS1_SCF, CallDataAttribute.P_LEG1.name());
					ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
				} else {

					logger.debug("Some other actions exists in the array, checking whether to send dialogue with DFC or not");
					Boolean dfcInSeparateDialogue = Boolean.valueOf(SipProtocolConfig.getConfigData(DFC_IN_SEPARATE_DIALOGUE));

					logger.debug("Application flag for handling DFC:"+overideSendingDfcInSeperateDlg 
							+ ", Configured through mph properties:" + dfcInSeparateDialogue);

					if (dfcInSeparateDialogue && overideSendingDfcInSeperateDlg == -1) {
						InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);
					} else if (overideSendingDfcInSeperateDlg > 0){
						InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);
					} else {
						logger.debug("Not sending the dialogue right now as per the configuration..");
					}
				}
			}
		} catch (Exception ex) {
			logger.error("[PH]:: Failed to disconnect ivr. Error is " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Error in disconnecting orig ivr.", ex);
			}
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS1ScfRelReasonCode.EXCEP_DISCON_ORIG_IVR);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			InapCS1ScfProtocolHelper.dropCall(tcapSession, callData);
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
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
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
			dfcInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
			dfcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

			sendComponentReq(dfcInvokeReqEvent, callData);
			tcapSession.setAttribute(PhConstants.DFC_SENT, PhConstants.TRUE);

			/*
			 * ASSIST INVITE leg should be cleaned if DFC has not been sent
			 */
			SipApplicationSession sipApplicationSession = InapCS1ScfProtocolUtil.getAssistAppSession(tcapSession,
					PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());
			if (sipApplicationSession != null) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Setting DFC_SENT flag in appSessionId: " + sipApplicationSession.getId());
				}
				sipApplicationSession.setAttribute(PhConstants.DFC_SENT, PhConstants.TRUE);
			}
			InapCS1ScfProtocolUtil.updateSS7CallState(origLegData, InapCallStates.MS_DISCONNECTED);
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
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside processSRR");
		}
		try {
			InapCS1ScfProtocolParser.parseSRR(invokeIndEvent, callData);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Notify service for PLAY_SUCCESS_EVENT");
			}
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Action lastAction = (Action) callData.get(CallDataAttribute.P_LAST_CALL_ACTION);
			if (null != lastAction) {
				if (lastAction.getActionType() == Action.ActionType.ACTION_PLAY) {
					Event event = new Event(Event.EventType.EVENT_PLAY_SUCCESS, Protocol.ITUINAPCS1_SCF, CallDataAttribute.P_LEG1.name());
					ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
				} else {
					logger.error("Last action requested by service: " + lastAction + " is not supported on SRR receival! Ignoring SRR..");
				}
			} else {
				logger.error("Last action requested by service was null! Ignoring SRR!");
			}
		} catch (ASNParsingException ape) {
			logger.error("[PH]:: ASN pasring Exception in SRR.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS1ScfRelReasonCode.SRR_ASN_PARSING_FAIL);
			return InapCS1ScfProtocolHelper.getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error("[PH]:: ParameterOutOfRangeException in SRR.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS1ScfRelReasonCode.SRR_PARAM_OUT_OF_RANGE);
			return InapCS1ScfProtocolHelper.getOutOfRangeParamterAction(tcapSession, ASNParsingException.MESSAGE.ACR);
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
			CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
			MDC.put(PhConstants.MDC_CALL_ID_CONST, "[" + callData.get(CallDataAttribute.P_DIALOG_ID) + "]");
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Inside sendAssistDfc");
				logger.debug("[PH]:: Send DFC with CONTINUE");
			}
			sendDfc(tcapSession, action);
			InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);
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
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendResetTimer");
		}

		byte[] resetTimer = createResetTimer(callData, (SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS));
		byte[] rtOpcode = {InapOpCodes.RESET_TIMER_BYTE};

		if(logger.isDebugEnabled()){
			String resetTimerBytes = CommonUtils.formatBytes(resetTimer);
			logger.debug("[PH]:: Encoded ResetTimer: " + resetTimerBytes);

		}
		Operation rtOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rtOpcode);

		InvokeReqEvent rtInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rtOperation);
		rtInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
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

		String resetTimerValue=InapCS1ScfProtocolConfig.
				getConfigData(InapCS1ScfProtocolConfig.RESET_TIMER_VALUE);

		// Timer ID
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
