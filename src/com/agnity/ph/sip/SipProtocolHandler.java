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
package com.agnity.ph.sip ;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.ServletException;
import javax.servlet.sip.Address;
import javax.servlet.sip.Proxy;
import javax.servlet.sip.Rel100Exception;
import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipApplicationSessionEvent;
import javax.servlet.sip.SipErrorEvent;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.SipURI;
import javax.servlet.sip.URI;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;
import org.apache.log4j.pattern.LogEvent;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.Action.CONNECTIONMODE;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CapV2CallStates;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.InapCallStates;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.MultiChoiceContact;
import com.agnity.mphdata.common.MultipartBody;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.SignalingTypeEnum;
import com.agnity.mphdata.common.State;
import com.agnity.mphdata.common.TermRedirectionContact;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolHandler;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolHelper;
import com.agnity.ph.sip.isup.SipIsupHelper;
import com.baypackets.ase.common.Registry;
import com.baypackets.ase.router.acm.AseAppChainManager;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.GroupedMsSessionController;
import com.baypackets.ase.sipconnector.AseDialogManager;
import com.baypackets.ase.sipconnector.AseSipServletRequest;
import com.baypackets.ase.util.CallTraceService;
import com.baypackets.ase.util.Constants;
import com.genband.tcap.provider.TcapSession;
/**
 * <p>
 * While creating a new sipsession or receiving initial INVITE set LEG_ID
 * attribute on that sip session. This attribute (LEG_ID) is the key in CallData
 * hashmap to fetch LegData
 * <p>
 * While initiating new media-server connection set LEG_ID attribute on
 * MsSbbController, which can later be used on call-backs from Ms-Sbb to obtain
 * LegData object from CallData hashmap.
 * <p>
 * Do not store any information on SIP sessions other than LEG_ID.
 * <p>
 * CallData object is stored on SipApplicationSession on receiving initial
 * INVITE
 * <p>
 * If service wants to connect any term leg to media server then first it would
 * send CONNECT_TERM action and after receiving 200 OK from term, mPH would send
 * EVENT_SUCCESS to service for that term leg. And then service would return
 * CONNECT_MS with leg name of this term leg
 * <p>
 * Initial INVITE from orig side must contain SDP as of now
 * <p>
 * IS_OFFER_RECEIVED flag will be true on Legdata of the leg on which SDP/Offer
 * has been received but not answered yet. After sending answer, this attribute
 * will be removed
 * <p>
 * IS_OFFER_SENT flag will be true on Legdata of the leg on which SDP/Offer has
 * been sent by mPH but has not received answer yet. After receiving answer,
 * this attribute will be removed.
 * 
 */
public class SipProtocolHandler implements ProtocolHandler {


	private static Logger logger = Logger.getLogger(SipProtocolHandler.class);

	private static final Set<Integer> BUSY_SIP_CODES = new HashSet<Integer>();
	private static final Set<Integer> NOANS_SIP_CODES = new HashSet<Integer>();

	private static final SipProtocolHandler INSTANCE = new SipProtocolHandler();

	static {
		/*
		 * Sip busy codes
		 */
		BUSY_SIP_CODES.add(486);

		/*
		 * Sip no-answer cause
		 */
		NOANS_SIP_CODES.add(480);
		NOANS_SIP_CODES.add(408);
	}

	/**
	 * The fllowing two dialogManager related fields are used to maintain refer dialogs
	 */
	private ConcurrentHashMap<String, String> m_ReferDialogMap;
	private AseDialogManager dialogManager;

	private SipProtocolHandler() {

		m_ReferDialogMap = new ConcurrentHashMap<String, String>();

		dialogManager=(AseDialogManager) Registry.lookup(Constants.DIALOG_MGR);

		if (logger.isDebugEnabled()) {
			logger.debug("::Look up dialog manager from registry "+dialogManager);
		}
	}

	public static SipProtocolHandler getInstance() {
		return INSTANCE;
	}

	/**
	 * This method is used to execute action returned by service. this method is
	 * called by ProtocolRouter
	 * 
	 * @param callData
	 *            data object for the call
	 * @param action
	 *            action returned by the service
	 */
	public void executeAction(CallData callData, Action action)
			throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside executeAction");
		}
		SipApplicationSession appSession = SipProtocolUtil
				.getAppSession((String) callData
						.get(CallDataAttribute.P_APP_SESSION_ID), (String)callData.get(CallDataAttribute.SERVICE_ID));
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside executeAction for appsesson is"+appSession);
			logger.debug(origLegCallId + ":: Action " + action.getActionType());
		}
		

		Object dialogIdObj = (Object) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug( "[PH]:: executeAction() Enter with callData and action " +dialogIdObj);
		}
		if (dialogIdObj != null &&  appSession==null) {
			
			int dialogId=(Integer)dialogIdObj;
			
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + "[PH]:: executeAction() Enter with callData and getappsession from tcapsession ");
			}
			TcapSession tcapSession = PhUtilityServices
					.getInstance(
							(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getTcapProvider().getTcapSession(dialogId);

			appSession = SipProtocolUtil.getAppSession(tcapSession);
		}

		traceMessage(callData, appSession);

		if (action == null) {
			logger.warn(origLegCallId
					+ ":: NULL action received fromservice. Do nothing");
			return;
		}
		/*
		 * Store action in callData for future references
		 */
		callData.set(CallDataAttribute.P_CURRENT_ACTION, action);

		switch (action.getActionType()) {
		case ACTION_REDIRECT: {

			String isChainingEnabled=SipProtocolConfig.getConfigData(SipProtocolConfig.SERVICE_CHAINING_ENABLED);

			if (PhConstants.FALSE.equals(isChainingEnabled)
					|| !SipProtocolHelper.invokeServiceChaining(appSession,
							callData, action)) {
				SipProtocolHelper.sendRedirectionResponse(appSession, callData,
						action);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "::Could not invoke Redirection Action "
							+ action.getActionType());
				}
			}
			break;
		}
		case ACTION_CONNECT: {

			String isChainingEnabled=SipProtocolConfig.getConfigData(SipProtocolConfig.SERVICE_CHAINING_ENABLED);

			if (PhConstants.FALSE.equals(isChainingEnabled)
					|| !SipProtocolHelper.invokeServiceChaining(appSession, callData, action)) {
				
				if (action.getConnectionMode() == Action.CONNECTIONMODE.REDIRECTION) {
					SipProtocolHelper.sendRedirectionResponse(appSession,
							callData, action);
				} else {
					SipProtocolHelper.connectTerm(appSession, callData, action);
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "::Could not invoke Connect Action "
							+ action.getActionType());
				}
			}
			break;
		}
		case ACTION_RESYNC_CALL: {
			SipProtocolHelper.resyncCall(appSession, callData, action);
			break;
		}
		case ACTION_END_CALL: {
			action.setServiceComplete(true);
			SipProtocolHelper.dropCall(appSession, callData, action);
			break;
		}
		case ACTION_ALLOW_TRANSFER: {
			SipProtocolHelper.allowAndPerformCallTransfer(appSession, callData, action,dialogManager);
			break;
		}
		case ACTION_REJECT_TRANSFER: {
			SipProtocolHelper.rejectCallTransfer(appSession, callData, action);
			break;
		}
		case ACTION_DTMF_TRANSFER_CONNECT:{
			SipProtocolHelper.initiateDtmfCallTransfer(appSession,callData,action);
		}
		break;
		case ACTION_TRANSFER_CONNECT:{
			SipProtocolHelper.initiateTransferConnect(appSession,callData,action);
		}
		break;
		case ACTION_INVOKE_SVC_CHAINING: {
			if (logger.isDebugEnabled()) {
				logger.debug("Inside ACTION_INVOKE_SVC_CHAINING ");
			}
			if(StringUtils.isNotBlank(action.getNextAppId())) {
				
				if (logger.isDebugEnabled()) {
					logger.debug("Inside Service chainging AppId exists:- "+ action.getNextAppId());
				}
				Event event = new Event(EventType.EVENT_INITIAL,
						Protocol.SIP, CallDataAttribute.P_LEG1.name());
			
				
				ServiceInterface serviceObj= PhUtilityServices.getInstance(action.getNextAppId())
						.getServiceHandler();


				if (logger.isDebugEnabled()) {
					logger.debug("Calling ProtcolRouter with eventInitial "+ action.getNextAppId());
				}
				ProtocolRouter.getInstance().execute(event, callData, serviceObj);
			}else {
				if (logger.isDebugEnabled()) {
					logger.debug("Next App id doesnot exists doing db call for next id ");
				}
				action.setInvokeServiceChaining(true);
			boolean serviceInvoked=	SipProtocolHelper.invokeServiceChaining(appSession, callData,action);
			
			if(serviceInvoked) {
				
				Event event = new Event(EventType.EVENT_SERVICE_CHAINING_INVOKE_SUCCESS,
						Protocol.SIP, CallDataAttribute.P_LEG1.name());
				if (logger.isDebugEnabled()) {
					logger.debug("Raising EVENT_SERVICE_CHAINING_INVOKE_SUCCESS ");
				}
				ServiceInterface serviceObj= PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				ProtocolRouter.getInstance().execute(event, callData, serviceObj);
			}else {
			
				Event event = new Event(EventType.EVENT_SERVICE_CHAINING_INVOKE_FAILURE,
						Protocol.SIP, CallDataAttribute.P_LEG1.name());
				if (logger.isDebugEnabled()) {
					logger.debug("Raising EVENT_SERVICE_CHAINING_INVOKE_FAILURE ");
				}
				ServiceInterface serviceObj= PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				ProtocolRouter.getInstance().execute(event, callData, serviceObj);
			}
			}
			
			break;
		}

		case ACTION_PICKUP_CALL:{
			SipProtocolHelper.initiateCallPickup(appSession,callData,action);
		}
		break;
		case ACTION_DISCONNECT: {
			SipProtocolHelper.disconnectLeg(appSession, callData, action);
			break;
		}
		case ACTION_RESYNCH_LEGS: {
			SipProtocolHelper.resynchLegs(callData, action);
			break;
		}
		case ACTION_CONNECT_MS: {
			SipProtocolHelper.initiateMediaServerConnection(appSession,
					callData, action.getLeg());
			break;
		}
		case ACTION_DISCONNECT_MS: {
			SipProtocolHelper.disconnectIvr(appSession, callData, action);
			break;
		}
		case ACTION_PLAY: {
			SipProtocolHelper.playAnnouncement(appSession, callData, action);
			break;
		}
		case ACTION_PLAY_COLLECT: {
			SipProtocolHelper.playAndCollect(appSession, callData, action);
			break;
		}
		case ACTION_TRY_REDIRECT_CONTACTS: {
			SipProtocolHelper.tryRecievedRedirectContacts(appSession, callData,
					action);
			break;
		}
		case ACTION_START_TIMER: {
			SipProtocolHelper.startApplicationTimer(appSession, callData,
					action);
			break;
		}
		case ACTION_STOP_TIMER: {
			SipProtocolHelper.stopApplicationTimer(appSession, callData,
					action);
			break;
		}
		case ACTION_STOP_MS_OPERATION: {
			SipProtocolHelper.stopMediaOperation(appSession, callData,
					action);
			break;
		}
		case ACTION_PROCESS_NEXT:
			@SuppressWarnings("unchecked")
			ArrayList<String> appSessionIds = (ArrayList<String>) callData.get(CallDataAttribute.P_DQ_CALLS_APP_ID);
			SipProtocolHelper.processNextCalls(appSessionIds, origLegCallId, (String)callData.get(CallDataAttribute.SERVICE_ID));
			break;	
		case ACTION_NONE:
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Action is ACTION_NONE; do nothing");
			}
			break;
		case ACTION_SERVICE_COMPLETE: {
			SipProtocolHelper.serviceComplete(appSession, callData,
					action);
			break;
		}
		//	   case ACTION_INVOKE_SVC_CHAINING: {
		//			SipProtocolHelper.invokeServiceChaining(appSession, callData,
		//					action);
		//			break;
		//		}

		case ACTION_CONNECT_PARALLEL:
			SipRingParallel.connectParallel(appSession, callData, action);
			break;

		case ACTION_CONNECT_SERIAL:{
			//SipProtocolHelper.connectMultipleDestinations(appSession, callData, action, false);
			SipRingSerial.connectSerial(callData, action, appSession);
		}
		break;
		case ACTION_WRITE_CDR: {
			SipProtocolHelper.writeServiceCdr(callData,action);
			break;
		}

		case ACTION_HOLD_CALL: {
			SipProtocolHelper.holdCall(appSession, callData, action);
			break;
		}

		case ACTION_CREATE_CONF : {
			SipProtocolHelper.createConference(appSession, callData, action);
			break;
		}

		case ACTION_RECORD : {
			SipProtocolHelper.recordCall(appSession, callData, action);
			break;
		}

		case ACTION_PLAY_RECORD : {
			SipProtocolHelper.playAndRecord(appSession, callData, action);
			break;
		}
		
		case ACTION_DO_REPLICATION:{
			PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getAppDataReplicator()
			.doReplicate(appSession);
			break;
		}
		case ACTION_FORCE_CALL_CLEANUP:{
			if(logger.isDebugEnabled()) {
				logger.debug("Inside Action Force Call cleanup ");
			}
			SipProtocolHelper.forceCallCleanup(appSession,callData,action);
			break;
		}

		default: {
			logger.error(origLegCallId + ":: Incorrect action type received "
					+ action.getActionType());
			break;
		}
		}

	}

	/**
	 * This method is used to trace a message
	 * 
	 * @param callData
	 * @param appSession
	 */
	private void traceMessage(CallData callData,
			SipApplicationSession appSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("Inside traceMessage()...");
		}

		String traceFlag = (String) callData
				.get(CallDataAttribute.P_TRACE_FLAG);
		if (PhConstants.TRUE.equals(traceFlag)) {
			StringBuilder traceMsg = (StringBuilder) callData
					.get(CallDataAttribute.P_TRACE_MESSAGE);

			if (traceMsg.length() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug(callData
							.get(CallDataAttribute.P_ORIG_LEG_CALL_ID)
							+ ":: Trace message is " + traceMsg);
				}
				SipServletRequest origSipRequest = SipProtocolUtil
						.getOrigInitialInvite(appSession, callData);

				/*
				 * This check is added to take care of trace messages, if MS is connected in Assist Mode
				 * in case of INAP call. As for SIP, criteria didn't matches for trace.
				 */
				if(callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null){
					if(logger.isDebugEnabled()){
						logger.debug("Call received in Assist Mode");
					}
					int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
					int callState = CallTraceService.CALL_IN_PROGRESS;
					List<Integer> constraintList = (List<Integer>) callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					for (int constraint : constraintList) {
						PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService().trace(constraint, String.valueOf(dialogueId), traceMsg.toString(), callState);
					}
					// In most of the cases sipServletMessage would not be null, it
					// is just preventive check
				}else if (origSipRequest != null) {
					traceMsg.append(PhConstants.TRACE_MESSAGE_END_FOOTER);
					PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService()
					.trace(origSipRequest, traceMsg.toString());
				}
				traceMsg.delete(0, traceMsg.length());

			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("Exitting traceMessage()...");
		}
	}

	/**
	 * This method is is used to process initial incoming invite. this method is
	 * called by ProtocolHandlerServlet
	 * 
	 * @param sipRequest
	 * @param serviceHandler
	 * @throws ServletException
	 * @throws IOException
	 */
	public final void doInvite(SipServletRequest sipRequest,
			ServiceInterface serviceHandler) throws ServletException,
	IOException {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = null;
		String origLegCallId = sipRequest.getCallId();
		boolean isTransferedCall = false;
		try {

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Entering doInvite");
			}
			/*
			 * For SIPRedirect + B2BUA callData would not be null for redirected 
			 * INVITE
			 */

			appSession.setAttribute(PhConstants.SERVICE_ID, serviceHandler.getApplicationName());
			callData = SipProtocolUtil.getCallData(appSession);
			logger.info("get callData in doInvite(): "+callData);
			isTransferedCall = SipProtocolUtil.isTransferedCall(sipRequest, origLegCallId, m_ReferDialogMap);
			// Re-Invite handling
			if (!sipRequest.isInitial()) {
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Handle Reinvite");
				}
				// Validate the INVITE request
				//				boolean isRejected = SipProtocolHelper
				//						.validateInviteRequest(sipRequest);
				//				if (isRejected) {
				//					logger.error(origLegCallId + ":: INVITE request rejected");
				//					return;
				//				}
				// Handler Reinvite
				// Set received SDP for future references
				boolean receivedSdp = SipProtocolUtil.setReceivedSdp(
						sipRequest, callData);
				SipProtocolHelper.handleReinvite(sipRequest);
				return;
			}


			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: setInvalidateWhenReady(false)");
			}
			sipRequest.getSession().setInvalidateWhenReady(false);

			String disableProxy=SipProtocolConfig.getConfigData(SipProtocolConfig.DISABLE_OUTBOUND_PROXY);

			if(PhConstants.TRUE.equalsIgnoreCase(disableProxy)){
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: DISABLE_OUTBOUND_PROXY ");
				}
				sipRequest.getSession().setAttribute("DISABLE_OUTBOUND_PROXY",true);
			}

			// Set start time for performance analysis
			long startTimeMilliSec = 0;
			int callsToCalcAvgResTime = Integer.parseInt(SipProtocolUtil.getConfig(SipProtocolConfig.AVG_TIME_CALC_COUNT));
			if (callsToCalcAvgResTime > 0) {
				startTimeMilliSec = System.currentTimeMillis();
			}

			/*
			 * SBTM-UAT-555: If ADDITIONAL_HEADERS is set on orig SIP session,
			 * then on receival of CANCEL from orig, SAS will not send 487 for
			 * INVITE
			 */
			sipRequest.getSession().setAttribute(PhConstants.ADDITIONAL_HEADERS, PhConstants.TRUE);
			/*
			 * For every sipSession, set its LEG_ID as an attribute on that
			 * sip-session
			 */
			sipRequest.getSession().setAttribute(PhConstants.LEG_ID,
					CallDataAttribute.P_LEG1.name());

			/*
			 * Process the initial invite
			 */
			if (PhConstants.TRUE.equals(SipProtocolUtil.getConfig(SipProtocolConfig.INVITE_100_TRYING_FLAG))) {
				sipRequest.createResponse(SipServletResponse.SC_TRYING).send();
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Send 100 Trying");
				}
			}

			// Check for the type of call -Handoff, Assist etc
			TcapSession tcapSession = SipProtocolUtil.getTcapSession(appSession);
			callData = SipProtocolUtil.getCallData(appSession);

			if (tcapSession != null) {

				String serviceId=(String)tcapSession.getAttribute(PhConstants.SERVICE_ID);

				if(serviceId!=null){
					appSession.setAttribute(PhConstants.SERVICE_ID, serviceId);
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId + "::serviceid found in tcapsession !!!");
					}
				}

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + "::Tcap Session found in AppSession . its Assist/Hand-off scenario");
				}
				// Inter-network call - INAP Origination Assist/Hand-off

				// callData.setOrigLegCallId(sipRequest.getCallId());
				// origLegCallId = callData.getOrigLegCallId();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Assist/Handoff INVITE received with call id "
							+ sipRequest.getCallId() + " for dialogue id "
							+ callData.get(CallDataAttribute.P_DIALOG_ID));
				}

				/*
				 * Handoff[CONNECT_TERM B2BUA]/Redirection[CONNECT_TERM
				 * REDIRECTION] happened, so move calldata from tcap session to
				 * app session here. As in cleanupCorrelationResources() method
				 * tcapsession will be invalidated.
				 */
				LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

				callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID, sipRequest.getCallId());
				//FIXME: Commented for timebeing by Ankit since no call to similar getters could be found
				// callData.setPersistableData(sipRequest.getCallId(), legData);

				Object ss7State = legData.get(LegDataAttributes.P_LEG_SS7_STATE);

				if (ss7State.equals(InapCallStates.HANDOFF) || ss7State.equals(AinCallStates.HANDOFF)) {
					appSession.setAttribute(CallData.CALL_DATA, callData);
				} else if (ss7State.equals(InapCallStates.ASSIST)
						|| ss7State.equals(AinCallStates.ASSIST)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Set assist indicator to 1");
					}
					callData.set(CallDataAttribute.P_ASSIST_IND, 1);
					tcapSession.setAttribute(PhConstants.ASSIST_APP_SESSION_ID, appSession.getId());
					sipRequest.getSession().setAttribute(PhConstants.ASSIST_LEG, PhConstants.TRUE);
				}

				// callData.setPersistableData(LegDataAttributes.P_LEG_SIP_STATE,
				// State.SERVICE_LOGIC);
				// callData.setTermSignaling(signalingType);

				callData.set(CallDataAttribute.P_APP_SESSION_ID, sipRequest.getApplicationSession().getId());

				// Set recent via header ip and port in call data
				// bestell changes
				SipProtocolUtil.setLoopBackAddress(origLegCallId, callData, sipRequest);

				callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID, sipRequest.getCallId());
				// Store CDR class reference in appSession for future reference
				CDR cdrRef = (CDR) sipRequest.getSession().getAttribute(CDR.class.getName());
				callData.set(CallDataAttribute.P_CDR_REF, cdrRef);
				callData.set(CallDataAttribute.P_APP_SESSION_ID, appSession.getId());

				legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.INIT);
				legData.set(LegDataAttributes.P_SESSION_ID, sipRequest.getSession().getId());
				legData.set(LegDataAttributes.P_CONNECTION_TYPE, ConnectionType.ORIG_CONNECTION);
				// legData.setPersistableData(
				// LegDataAttributes.SESSION_REFRESH_TIMER_NAME,
				// PhConstants.SESSION_REFRESH_TIMER);
				legData.set(LegDataAttributes.P_CALL_ID, sipRequest.getCallId());

				// Set received SDP for future references
				boolean receivedSdp = SipProtocolUtil.setReceivedSdp(sipRequest, callData);
				if (receivedSdp) {
					legData.set(LegDataAttributes.NP_IS_OFFER_RECEIVED, PhConstants.TRUE);
				}

				// Parse the message
				//	SipProtocolUtil.parseInvite(callData, legData, sipRequest);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: " +
							"RemoteAddr=" + callData.get(CallDataAttribute.P_ORIGIN_IP)
							+ ", RemotePort=" + callData.get(CallDataAttribute.P_ORIGIN_PORT));
				}

				// Perform action requested by service, those needs to be
				// handled after ASSIST/HANDOFF
				Action lastAction = (Action) callData.get(CallDataAttribute.P_LAST_CALL_ACTION);
				if (lastAction.getActionType() == Action.ActionType.ACTION_CONNECT_MS) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: lastAction is CONNECT_IVR, initiate MS Connection");
					}
					SipProtocolHelper.initiateMediaServerConnection(appSession, callData, (String) sipRequest.getSession().getAttribute(PhConstants.LEG_ID));
				}

				/*
				 * Cleanup the correlation related resources using appsession
				 * corresponding to tcapsession Cleanup should be performed at
				 * the last so that INVITE request gets replicated to other SAS
				 * This would ensure the CDR generation in case FT happens just
				 * after receiving the INVITE
				 */
				SipProtocolUtil.cleanupCorrelationResources(SipProtocolUtil.getAppSession(tcapSession));

				/*
				 * Set tcap session to null for handoff case so that any event
				 * on appSession of tcapSession does not get calldata and does
				 * not impact normal call processing
				 */
				if (ss7State.equals(InapCallStates.HANDOFF)) {
					tcapSession = null;
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: New Intra-network INVITE received. Parse the request");
				}

				callData = new CallData();
				callData.set(CallDataAttribute.P_NETWORK_TRANSACTION,new MutableInt(0));

				callData.set(
						CallDataAttribute.P_CALL_START_TIME, new Date());
				appSession.setAttribute(CallData.CALL_DATA, callData);
				// callData.setPersistableData(CallDataAttribute.P_SERVICE_HANDLER,
				// serviceHandler);
				LegData legData = new LegData();
				callData.set(CallDataAttribute.P_LEG1, legData);
				callData.set(
						CallDataAttribute.P_ORIG_LEG_CALL_ID,
						sipRequest.getCallId());
				// Store CDR class reference in appSession for future reference
				CDR cdrRef = (CDR) sipRequest.getSession().getAttribute(
						CDR.class.getName());
				callData.set(CallDataAttribute.P_CDR_REF, cdrRef);

				callData.set(CallDataAttribute.P_APP_SESSION_ID,
						appSession.getId());
				// Set recent via header ip and port in call data
				SipProtocolUtil.setLoopBackAddress(origLegCallId, callData, sipRequest);

				legData.set(LegDataAttributes.P_LEG_SIP_STATE,
						State.INIT);
				legData.set(LegDataAttributes.P_SESSION_ID,
						sipRequest.getSession().getId());
				legData.set(LegDataAttributes.P_CONNECTION_TYPE,
						ConnectionType.ORIG_CONNECTION);
				// legData.setPersistableData(
				// LegDataAttributes.SESSION_REFRESH_TIMER_NAME,
				// PhConstants.SESSION_REFRESH_TIMER);
				legData.set(LegDataAttributes.P_CALL_ID,
						sipRequest.getCallId());

				// Store session expire duration for future reference
				SipProtocolUtil.setSessionExpiryTime(sipRequest, legData,
						origLegCallId);

				// Set received SDP for future references
				SipProtocolUtil.setReceivedSdp(sipRequest, callData);

				/**
				 * If this call is sipt call then set the signalling key as SIPT
				 */
				SipIsupHelper.isSiptCall(sipRequest, origLegCallId);

				// Parse the message
				SipProtocolUtil.parseInvite(callData, legData, sipRequest);

				/*
				 * Not Setting FT_FLAG as absence of this flag in CallData means
				 * that FT has not been performed
				 */
				Boolean traceFlag = (Boolean) appSession
						.getAttribute(PhConstants.MATCHES_CALL_CRITERIA);
				if (traceFlag.booleanValue()) {
					callData.set(CallDataAttribute.P_TRACE_FLAG,
							PhConstants.TRUE);
					callData.set(
							CallDataAttribute.P_TRACE_MESSAGE,
							new StringBuilder());
				}

				/*
				 * Drop the call, if service failed to read configuration
				 */
				if (!PhUtilityServices.getInstance(serviceHandler.getApplicationName()).isServiceInitialized()) {
					logger.error(origLegCallId
							+ ":: Service failed to read configuration. Drop Call. Restart CAS to solve the issue");
					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							SipProtocolRelReasonCode.FAILED_TO_INITIALIZE_SRV);

					// callData.setFailedCallInd(1);
					legData.set(
							LegDataAttributes.P_CAUSE_CODE,
							Integer.parseInt(SipProtocolUtil
									.getConfig(

											SipProtocolConfig.SIP_RES_CODE_SRV_INIT_FAILED)));

					SipProtocolHelper.dropCall(appSession);
					return;
				}

				// Validate the INVITE request
				boolean isRejected = SipProtocolHelper
						.validateInviteRequest(sipRequest);
				if (isRejected) {
					logger.error(origLegCallId + ":: INVITE request rejected");
					return;
				}

				/*
				 * Fix for bug#16706. If INVITE is not intended for this service
				 * then disconnect with CV=41
				 */
				String xIscSvcHeader = sipRequest
						.getHeader(PhConstants.X_ISC_SVC);
				if (xIscSvcHeader != null
						&& !xIscSvcHeader.equals(serviceHandler
								.getApplicationName())) {
					logger.error(origLegCallId
							+ "::Drop the call. Invite is not intended for "
							+ serviceHandler.getApplicationName());
					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							SipProtocolRelReasonCode.INVITE_NOT_FOR_SRV);

					// callData.setFailedCallInd(1);
					legData.set(
							LegDataAttributes.P_CAUSE_CODE,
							Integer.parseInt(SipProtocolUtil
									.getConfig(

											SipProtocolConfig.SIP_RES_CODE_INVITE_NOT_FOR_SRV)));
					SipProtocolHelper.dropCall(appSession);

					return;
				}


				if(sipRequest.getRequestURI().isSipURI()){
					if (((SipURI)sipRequest.getRequestURI()).getUser().contains(PhConstants.CONF_IDENTIFIER)) {

						com.agnity.ph.sip.conference.ConferenceHandler.getInstance().handleConferenceRequest(sipRequest);
						return;
					}
				}



				/*
				 * First SIP-T and INAP call were taking almost 11 seconds to
				 * process. To solve this issue SAS will generate an invite and
				 * send a warm up call to service.
				 */
				// if (sipRequest.getHeader(PhConstants.WARMUP_HEADER) != null)
				// {
				// try {
				// if (logger.isInfoEnabled()) {
				// logger
				// .info(origLegCallId
				// +
				// ":: This is a warm-up SIP call. Send no response for this call");
				// }
				// //Notify service that call is dropped
				// legData.setPersistableData(LegDataAttributes.STATE,
				// State.INIT);
				// notifyCallDropped(appSession);
				// legData.setPersistableData(LegDataAttributes.READY_TO_INVALIDATE,
				// PhConstants.TRUE);
				// invalidateAppSession(appSession);
				// //Note:- Do not write CDR for warm-up call
				// } catch (Throwable e) {
				// logger.warn(origLegCallId +
				// " :: Failed to handle SIP-T warm-up call"
				// + e.getMessage());
				// }
				// return;
				// }

				// Parse the message and fill leg data in call data

				SipSession transferedSession = SipProtocolUtil.getTransferedSession(
						sipRequest, origLegCallId, m_ReferDialogMap, dialogManager, callData);

				if (isTransferedCall && transferedSession!=null) {

					//	if (transferedSession != null) {
					SipProtocolUtil.stopTimer(
							transferedSession.getApplicationSession(),
							PhConstants.CALL_TRANSFER_TIMER);
					//	}

					Event event = new Event(EventType.EVENT_TRANSFER,
							Protocol.SIP, CallDataAttribute.P_LEG2.name());

					CallData transferedCallData = SipProtocolUtil
							.getCallData(transferedSession.getApplicationSession());

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Transfered call data is found .Notify applictaion of cal tranfer for this call( data)" +transferedCallData);
					}

					transferedCallData.set(CallDataAttribute.NP_TRANSFERED_SESSION, sipRequest.getSession());

					if (PhConstants.TRUE
							.equals(callData
									.get(CallDataAttribute.NP_IS_CONSULTED_TRANSFER))){// && transferedSession!=null) {

						transferedCallData.set(CallDataAttribute.NP_IS_CONSULTED_TRANSFER, PhConstants.TRUE);

					} else {
						//						SipProtocolHelper.handleCallTransferInvite(sipRequest,
						//								appSession, transferedSession, callData);

						transferedCallData.set(CallDataAttribute.NP_IS_CONSULTED_TRANSFER, PhConstants.FALSE);
					}

					ProtocolRouter.getInstance().execute(event, transferedCallData,
							serviceHandler);

					return;
				} else {

					callData.set(CallDataAttribute.SERVICE_ID, serviceHandler.getApplicationName());

					Event event = new Event(EventType.EVENT_INITIAL,
							Protocol.SIP, CallDataAttribute.P_LEG1.name());

					PhUtilityServices phUtility=PhUtilityServices
							.getInstance(
									(String) callData
									.get(CallDataAttribute.SERVICE_ID));

					if (phUtility.isNewCallsAllowed()&& phUtility.getAseCompMonMgr().canAcceptNewCalls()) {

						callData.set(CallDataAttribute.NP_CM_CALL_ALLOWED,
								PhConstants.TRUE);
					} else {
						callData.set(CallDataAttribute.NP_CM_CALL_ALLOWED,
								PhConstants.FALSE);
					}

					ProtocolRouter.getInstance().execute(event, callData,
							serviceHandler);

				}
			}

			// Calculate average time for performance analysis
			if (callsToCalcAvgResTime > 0) {
				SipProtocolUtil.calculateAvgTime(callsToCalcAvgResTime,
						startTimeMilliSec, origLegCallId);
			}

		} catch (Exception ex) {
			logger.error(
					origLegCallId + ":: Failed to process INVITE with Call Id "
							+ sipRequest.getCallId(), ex);
			callData = SipProtocolUtil.getCallData(appSession);
			if (callData != null) {

				callData.set(
						CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.EXCEP_IN_INVITE_HANDLING);
				LegData leg1 = (LegData) callData
						.get(CallDataAttribute.P_LEG1);
				leg1.set(LegDataAttributes.P_CAUSE_CODE, Integer
						.parseInt(SipProtocolUtil.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
				SipProtocolHelper.dropCall(appSession);
			}
		}
	}




	/**
	 * Invoked by the server (via the doResponse method) to handle incoming 1xx
	 * class responses. It checks the connection type, termination sip call
	 * state and based on that does following:
	 * <ul>
	 * <li>For CONNECTION_IN_PROGRESS if sdp is received then Set a=sendonly for
	 * CONNECT_TERM_IVR case replicate the session on peer node for first 18x
	 * response on the session. In case 18x received reliable, check
	 * sRelProvRespReplicationReq too. If response is reliable add SDP content
	 * to it on receiving 200 OK of update/reinvite sent on leg1.
	 * <li>If origSipSession state is initial or Early update P-CDR-INFO on orig
	 * session so that it can be used by SAS in case orig party send CANCEL. In
	 * that case SAS would send this P-CDR-INFO in 487. This information is
	 * required to be updated every time in case of Invite to party B/Invite to
	 * IVR/Play/PlayNCollect/On ACM to keep latest info in cdr. *
	 * <li>If response is reliable and SDP is changed from last sent, Store
	 * provisional response to be sent and Send update with SDP as offer.
	 * <li>In rest of the cases if response is reliable then send origination
	 * provisionla response reliably so else unreliably
	 * </ul>
	 * In case of an exception in handling 1XX, call is dropped with CV=41.
	 * 
	 * @param sipResponse
	 *            represents the instance of SipServletResponse
	 * @see javax.servlet.sip.SipServlet#doProvisionalResponse(javax.servlet.sip.SipServletResponse)
	 */
	public final void doProvisionalResponse(SipServletResponse sipResponse)
			throws ServletException, IOException {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData termLegData = null;
		LegData origLegData = null;
		String resCallId = sipResponse.getCallId();

		try {

			appSession = sipResponse.getApplicationSession();
			callData = SipProtocolUtil.getCallData(appSession);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			/*
			 * Note that provisional response will only come from term len. It
			 * will never come from orig leg
			 */
			// Term details
			termLegData = SipProtocolUtil.getLegDataForSipSession(appSession,
					sipResponse.getSession(), callData);
			ConnectionType connType = (ConnectionType) termLegData
					.get(LegDataAttributes.P_CONNECTION_TYPE);
			State termState = (State) termLegData
					.get(LegDataAttributes.P_LEG_SIP_STATE);

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Received Provisional Response "
						+ sipResponse.getStatus() + " with call id "
						+ resCallId + " for connection " + connType
						+ " at State =" + termState);
			}

			// Orig leg information
			SipServletRequest origSipRequest = SipProtocolUtil
					.getOrigInitialInvite(appSession, callData);
			SipSession origSipSession = origSipRequest.getSession();
			origLegData = SipProtocolUtil.getLegDataForSipSession(appSession,
					origSipRequest.getSession(), callData);

			switch (connType) {
			case TERM_CONNECTION: {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: "
							+ sipResponse.getStatus()
							+ " response received on term leg");
				}

				State origState = (State) origLegData
						.get(LegDataAttributes.P_LEG_SIP_STATE);

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Orig leg State ="
							+ origState);
				}

				Date ringingTime=(Date)callData.get(
						CallDataAttribute.P_CALL_RINGING_TIME);
				//added for handling ringing time
				if (ringingTime == null) {
					// Set call ringing time
					callData.set(
							CallDataAttribute.P_CALL_RINGING_TIME, new Date());

				}

				switch (termState) {
				case CONN_IN_PROGRESS: {
					boolean isSdpSentOnLeg1Earlier = PhConstants.TRUE
							.equals((String) origLegData
									.get(LegDataAttributes.NP_IS_SDP_SENT));

					boolean isSdpRcvdFromTermInProvRes = SipProtocolUtil
							.setReceivedSdp(sipResponse, callData);


					boolean isTermProvResReliable = (sipResponse
							.getHeader(PhConstants.R_SEQ_HEADER) != null) ? true
									: false;


					boolean isSdpChanged = false;
					boolean isUnicastStream = false;

					MultipartBody currentReceivedSdpFromTerm = (MultipartBody) termLegData
							.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

					if (isSdpRcvdFromTermInProvRes) {
						MultipartBody lastSendSdpOnOrig = (MultipartBody) origLegData
								.get(LegDataAttributes.P_LAST_SENT_SDP);
						isSdpChanged = SipProtocolUtil.isSdpChanged(
								lastSendSdpOnOrig, currentReceivedSdpFromTerm,
								origLegCallId);		
						appSession.setAttribute(PhConstants.SESSION_ID_WITH_ORIG_SDP_EXCHANGED, sipResponse.getSession().getId());
					}

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: isSdpRecieved="
								+ isSdpRcvdFromTermInProvRes
								+ ", isSdpSentOnLeg1Earlier="
								+ isSdpSentOnLeg1Earlier + ", isSdpChanged="
								+ isSdpChanged + ", isResponseReliable="
								+ isTermProvResReliable);
					}

					/*
					 * SBTM-UAT-1401 Fix- Start no-anwer timer on first 18x
					 * reponse from party-B
					 */
					// Start no-answer timer
					Integer termNoAnsTimerDuration = (Integer) termLegData
							.get(LegDataAttributes.NP_NO_ANSWER_TIMER_DURATION);

					if (termNoAnsTimerDuration != null
							&& !SipProtocolUtil.isTimerRunning(appSession,
									PhConstants.NO_ANSWER_TIMER)) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ " :: Start no answer timer for time(ms) "
									+ termNoAnsTimerDuration);
						}
						// This timer should be non-persistable to avoid data
						// replication
						SipProtocolUtil.startTimer(appSession,
								termNoAnsTimerDuration, false,
								PhConstants.NO_ANSWER_TIMER);
					} else {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ " :: Dnot Start no answer timer for time(ms) "
									+ termNoAnsTimerDuration
									+ " it may be already running or duration provided is null");
						}
					}

					/*
					 * Replicate the session on peer node for first 18x response
					 * on the session. In case 18x received reliable, check
					 * sRelProvRespReplicationReq too.
					 */
					if (!isTermProvResReliable
							|| (isTermProvResReliable && PhConstants.TRUE
									.equals(SipProtocolUtil
											.getConfig(
													SipProtocolConfig.REL_PROV_RESP_REPLICATION)))) {
						if (!PhConstants.TRUE
								.equals(termLegData
										.get(LegDataAttributes.NP_IS_PROV_RESP_REPLICATED))&& isSdpRcvdFromTermInProvRes) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: Replicate the session on peer node for first 18x response");
							}
							termLegData.set(
									LegDataAttributes.NP_IS_PROV_RESP_REPLICATED,
									PhConstants.TRUE);
							PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
							.getAppDataReplicator()
							.doReplicate(appSession);
						}
					}

					boolean notify18XWithoutSdp = PhConstants.TRUE.equals(SipProtocolUtil.getConfig(SipProtocolConfig.NOTIFY_180_WITHOUT_SDP));

					if (!isSdpRcvdFromTermInProvRes) {

						/*
						 * Check if we need to notify application of 18x without sdp or not
						 */
						if(notify18XWithoutSdp) {

							String sessionIdWithOrigSdpExchanged = (String)appSession.getAttribute(PhConstants.SESSION_ID_WITH_ORIG_SDP_EXCHANGED);

							if (sessionIdWithOrigSdpExchanged!=null && !sessionIdWithOrigSdpExchanged.equals(sipResponse.getSession().getId())) {

								if (isTermProvResReliable) {
									if (logger.isDebugEnabled()) {
										logger.debug(origLegCallId
												+ " :: send prack for provisional response");
									}
									sipResponse.createPrack().send();
								}
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ " :: Notify application of 180 provisional response without sdp as " +
											"SDP has already been received from some term party.");
								}
								callData.set(CallDataAttribute.P_PROV_WO_SDP_NOTIFIED, PhConstants.TRUE);
								Event event = new Event(EventType.EVENT_PROVISIONAL_WO_SDP, Protocol.SIP, CallDataAttribute.P_LEG2.name());
								ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
								ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
								return;
							} else {
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId + " :: Not notifying application for this 18x as SDP is still not received from any term or 18x no sdp received from same leg afster 18xwithsdp!");
								}
							}
						} else {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId + " :: Not notifying application for this 18x as notifying configuration is disabled!");
							}
						}
					}

					SipServletRequest prackRequest = null;
					if (isTermProvResReliable) {
						if (termLegData
								.get(LegDataAttributes.NP_PENDING_PRACK) == null) {
							prackRequest = sipResponse.createPrack();
							// Add SDP content to it on receiving 200 OK of
							// update/reinvite sent on leg1
							termLegData.set(
									LegDataAttributes.NP_PENDING_PRACK,
									prackRequest);
						}
						if (PhConstants.FALSE
								.equals(termLegData
										.get(LegDataAttributes.NP_IS_OFFER_SENT))&& isSdpRcvdFromTermInProvRes && isSdpChanged) {
							termLegData.set(
									LegDataAttributes.NP_IS_OFFER_RECEIVED,
									PhConstants.TRUE);

							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: offer is received from term in rel provisional response!!!");
							}
						}
					}

					if (origSipSession.getState() == javax.servlet.sip.SipSession.State.CONFIRMED) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Provisional response received ");
						}

						// code changes need to be done for path switch feature
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Orig is in confirmed state and provisional response is recieved from term . it may be second destination connection case");
						}

						if (isTermProvResReliable) {

							if (isSdpRcvdFromTermInProvRes) {

								boolean isDialOutCall = false;

								Object dialOut =  callData.get(CallDataAttribute.P_DIALOUT);
								if(dialOut != null){
									isDialOutCall = (Boolean) dialOut;
								}

								if(isDialOutCall) {
									if(logger.isDebugEnabled()) {
										logger.debug("Term is connected in dialOut mode, so just sending prack");
									}

									SipProtocolUtil.setReceivedSdp(sipResponse, callData);
									prackRequest.send();
									termLegData.remove(LegDataAttributes.NP_PENDING_PRACK);

								}
							else {
									if (logger.isDebugEnabled()) {
										logger.debug(origLegCallId
												+ ":: Sending Re-invite to orig as sdp is received from term in rel prov response ");
									}
									origLegData.set(
											LegDataAttributes.NP_IS_OFFER_SENT,
											PhConstants.TRUE);

									MultipartBody currentRcvdSdpFromLeg = (MultipartBody) termLegData
											.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
									SipServletRequest reInviteReq = SipProtocolMessageCreator
											.createReinviteRequest(origSipRequest,
													currentRcvdSdpFromLeg);

									ServiceInterface serviceHandler = PhUtilityServices
											.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
									origSipRequest.getSession().setHandler(
											serviceHandler.getServletName());
									reInviteReq.send();
								}


							}else{

								if (prackRequest != null) {

									if (logger.isDebugEnabled()) {
										logger.debug(origLegCallId
												+ ":: send prack for reliable 180 without sdp for second dest case");
									}
									prackRequest.send();

									termLegData
									.remove(LegDataAttributes.NP_PENDING_PRACK);
								}
							}
						}


					} else if (origSipSession.getState() == javax.servlet.sip.SipSession.State.EARLY
							|| origSipSession.getState() == javax.servlet.sip.SipSession.State.INITIAL) {

						// Set the orig sip call state to CONNECTION_IN_PROGRESS
						origLegData.set(
								LegDataAttributes.P_LEG_SIP_STATE,
								State.CONN_IN_PROGRESS);

						int statusCode = sipResponse.getStatus();
						SipServletResponse origProvisionalResponse = SipProtocolMessageCreator
								.createProvisionalResponse(origSipRequest,
										statusCode,
										currentReceivedSdpFromTerm,
										origLegData, sipResponse);

						/*
						 * Update P-CDR-INFO on orig session so that it can be
						 * used by SAS in case orig party send CANCEL. In that
						 * case SAS would send this P-CDR-INFO in 487. This
						 * information is required to be updated every time in
						 * case of Invite to party B/Invite to
						 * IVR/Play/PlayNCollect/On ACM to keep latest info in
						 * cdr.
						 */
						/*
						 * SBTM-UAT-555: Comment setPCDRHeader method call as
						 * now PH will send 487 for INVITE on receival of CANCEL
						 */

						if (isSdpRcvdFromTermInProvRes
								&& isSdpSentOnLeg1Earlier && isSdpChanged) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: SDP is changed from last sent");
							}

							if (isTermProvResReliable) {
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ ":: Send update with SDP as offer");
								}
								// Store provisional response to be sent
								origLegData.set(
										LegDataAttributes.NP_PENDING_PROV_RESP,
										origProvisionalResponse);

								SipServletRequest updateReq = SipProtocolMessageCreator
										.createUpdate(origSipSession,
												currentReceivedSdpFromTerm,callData);
								updateReq.send();

								if (isUnicastStream) {
									if (logger.isDebugEnabled()) {
										logger.debug(origLegCallId
												+ ":: Update sent with unicast SDP");
									}
									origLegData
									.set(
											LegDataAttributes.NP_IS_UNICAST_SDP_UPDATE,
											PhConstants.TRUE);
								}

								// Update attribute for future reference
								SipProtocolUtil.setSentSdp(updateReq,
										origLegData, origLegCallId);
							} else {
								if (logger.isInfoEnabled()) {
									logger.info(origLegCallId
											+ ":: Do nothing as response received unrelaibly");
								}
							}
						} else {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: SDP has not been sent yet on orig leg");
							}

							if (isTermProvResReliable) {
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ ":: Response received reliably so send reliably");
								}

								origProvisionalResponse.sendReliably();
								ServiceInterface serviceHandler = PhUtilityServices
										.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
								origProvisionalResponse.getSession().setHandler(
										serviceHandler.getServletName());
							} else {
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ ":: Response received unreliably so send unreliably");
								}

								origProvisionalResponse.send();
							}
							SipProtocolUtil.setSentSdp(origProvisionalResponse,
									origLegData, origLegCallId);
						}
					} else {
						/*
						 * This is to handle the scenario where
						 * doProvisionalResposne method gets called for the call
						 * which is getting cleaned as caller disconnected it.
						 */
						logger.error(origLegCallId
								+ ":: Orig session state is terminated, do nothing");
						// TODO: Confirm if call needs to be dropped here
					}


					if (isSdpRcvdFromTermInProvRes) {

						String provWoSdpNotified = (String) callData
								.get(CallDataAttribute.P_PROV_WO_SDP_NOTIFIED);

						if (PhConstants.TRUE.equals(provWoSdpNotified)) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ " :: Prov response without sdp was notfied to application ned to notify for now when 180 response with sdp received");
							}

							callData.set(
									CallDataAttribute.P_PROV_WO_SDP_NOTIFIED,
									PhConstants.FALSE);
						}

						Event event = new Event(
								EventType.EVENT_SDP_RECEIVED, Protocol.SIP,
								CallDataAttribute.P_LEG2.name());
						ServiceInterface serviceHandler = PhUtilityServices
								.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
						ProtocolRouter.getInstance().execute(event,
								callData, serviceHandler);		
					}

					Event event = new Event(
							EventType.EVENT_PROVISIONAL_RECEIVED, Protocol.SIP,
							CallDataAttribute.P_LEG2.name());
					ServiceInterface serviceHandler = PhUtilityServices
							.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
					ProtocolRouter.getInstance().execute(event,
							callData, serviceHandler);
					break;
				}
				case TERMINATED:
					/*
					 * This is to handle the retransmission scenario where
					 * doProvisionalResposne method gets called for the call
					 * which is getting cleaned.
					 */
					logger.warn(origLegCallId
							+ ":: Term sip call state is terminated, do nothing");
					// TODO: Confirm if call needs to be dropped here
					break;
				default:
					logger.error(origLegCallId
							+ ":: Do nothing as received in unexpected call state");
					logger.error(origLegCallId
							+ ":: "
							+ origLegData
							.get(LegDataAttributes.P_LEG_SIP_STATE)
							+ " "
							+ termLegData
							.get(LegDataAttributes.P_LEG_SIP_STATE));
					// TODO: Confirm if call needs to be dropped here
				}
				break;
			}
			default:{
			if(logger.isDebugEnabled()){
				
				logger.debug(origLegCallId
						+ ":: Unexpected provisional response, do nothing");
				logger.debug(origLegCallId
						+ ":: "
						+ origLegData
						.get(LegDataAttributes.P_LEG_SIP_STATE)
						+ " "
						+ termLegData

						.get(LegDataAttributes.P_LEG_SIP_STATE));

				// TODO: Confirm if call needs to be dropped here
			}
			}
			}
		} catch (Rel100Exception ex) { // To fix exceptions observed in
			// production due to cancelled requests
			logger.warn(origLegCallId
					+ ":: Failed to send 18x to party-A as request already cancelled");
			if (logger.isInfoEnabled()) {
				logger.info(
						origLegCallId
						+ ":: Ignore Rel100Exception as mostly due to cancelled request",
						ex);
			}

			if(sipResponse.getSession().getState()!=SipSession.State.TERMINATED){
				try {
					SipProtocolUtil.start100relFailureRetryTimer(appSession, origLegData, sipResponse);
				} catch (Exception e) {
					logger.error("start100relFailureRetryTimer Exception " +e);
				}
			}
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Failed to process Provisional Response with Call Id "
					+ resCallId, ex);
			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_PROV_HANDLING);
			origLegData.set(LegDataAttributes.P_CAUSE_CODE,
					Integer.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);

		}
	}


	/**
	 * Invoked by the server (via the doResponse method) to handle incoming 2xx
	 * class responses. Depending upong the type of sucess response either of
	 * the handleInviteSuccess, handleUpdateSuccess, handleByeSuccess and
	 * dropCall is called. In case of an exception in handling INFO, call is
	 * dropped with CV=41.
	 * 
	 * @see javax.servlet.sip.SipServlet#doSuccessResponse(javax.servlet.sip.SipServletResponse)
	 * @param sipResponse
	 *            represents the SipServletResponse instance
	 */
	public final void doSuccessResponse(SipServletResponse sipResponse)
			throws ServletException, IOException {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData legData = null;
		String resCallId = sipResponse.getCallId();

		try {
			appSession = sipResponse.getApplicationSession();
			callData = SipProtocolUtil.getCallData(appSession);


			if (sipResponse.getMethod().equals(PhConstants.INVITE_REQUEST)) {
				SipProtocolUtil.incrementNetworkTransactions(callData, 1);
			}

			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			legData = SipProtocolUtil.getLegDataForSipSession(appSession,
					sipResponse.getSession(), callData);

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Received Success Response with call id "
						+ resCallId + " for " + sipResponse.getMethod() +" with response status as "+sipResponse.getStatus());
			}

			// Connection type from where response received
			ConnectionType connectionType = (ConnectionType) legData
					.get(LegDataAttributes.P_CONNECTION_TYPE);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Connection type is "
						+ connectionType);
				logger.debug(origLegCallId
						+ ":: State "
						+ legData
						.get(LegDataAttributes.P_LEG_SIP_STATE));
			}
			
			State state = (State) legData
					.get(LegDataAttributes.P_LEG_SIP_STATE);
			
			
			if (PhConstants.TRUE.equals(callData
					.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))
					&& !PhConstants.BYE_REQUEST.equals(sipResponse.getMethod())) {
				
				/*
				 * Fix for bug#16188. If 200 OK is received for INVITE then
				 * first PH should send ACK for that 200 OK. And after that
				 * dropCall() method call will clean-up the call.
				 */
				
				if(State.PROXY_CONNECT.equals(state)){
					
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ ":: State is PROXY_CONNECT  call already processed returning !!!");
					}
					return;
				}
				
				logger.warn(origLegCallId
						+ ":: Call already dropped. Cleanup remaining SIP sessions");
				
				if (sipResponse.getMethod().equals(PhConstants.INVITE_REQUEST)) {
					sipResponse.createAck().send();
				}
				SipProtocolHelper.dropCall(appSession);
				return;
			}
			if (sipResponse.getMethod().equals(PhConstants.INVITE_REQUEST)) {
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Success received for INVITE request");
				}
				
				Action lastAction = (Action) callData
						.get(CallDataAttribute.P_CURRENT_ACTION);
				
				if(State.PROXY_CONNECT.equals(state)){
					
					ServiceInterface serviceHandler = PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getServiceHandler();
					Event event = new Event(EventType.EVENT_SUCCESS,
							Protocol.SIP, CallDataAttribute.P_LEG1.name());
					ProtocolRouter.getInstance().execute(event, callData,
							serviceHandler);
					

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: return from state PROXY_CONNECT");
					}
					
				  if(lastAction.getConnectionMode()==Action.CONNECTIONMODE.PROXY_STATELESS){
					  SipProtocolHelper.writeServiceCdr(callData, lastAction);
					SipProtocolUtil.setAppSessionTimeout(appSession, 1, origLegCallId);
				  }
				  
					return;
				}
				legData.remove(LegDataAttributes.P_INVITE_PENDING_TRANS);

				// SBTM-UAT-1583 Fix - send BYE if 200 OK received after no
				// answer timer expiry
				if (PhConstants.TRUE
						.equals(legData
								.get(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND))) {
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ ":: Success response received after no-answer timeout, send BYE");
					}
					SipProtocolMessageCreator.createByeRequest(origLegCallId,
							sipResponse.getSession(),
							PhConstants.NO_ANS_TIMEOUT_CV_TERM_LEG, false)
					.send();
					return;
				}
				SipProtocolHelper.handleInviteSuccess(sipResponse, callData,
						legData);
			} else if (sipResponse.getMethod().equals(
					PhConstants.UPDATE_REQUEST)) {
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Success received for UPDATE request");
				}
				SipProtocolHelper.handleUpdateSuccess(sipResponse, callData,
						legData);
			} else if (sipResponse.getMethod().equals(PhConstants.BYE_REQUEST)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Success received for BYE request");
				}
				SipProtocolHelper.handleByeSuccess(sipResponse, callData,
						legData);
			} else if (sipResponse.getMethod().equals(
					PhConstants.CANCEL_REQUEST)) {
				// Do nothing
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Success received for CANCEL request, do nothing");
				}
			} else if (sipResponse.getMethod()
					.equals(PhConstants.PRACK_REQUEST)) {
				// Do nothing
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Success received for PRACK request, check for any pending update");
				}
				SipProtocolHelper.handlePrackSuccess(sipResponse, callData,
						legData);

			}else if (sipResponse.getMethod().equals(PhConstants.INFO_REQUEST)) {
				// Do nothing
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Success received for INFO request, do nothing");
				}
			} else if (sipResponse.getMethod().equals(PhConstants.REFER_REQUEST)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Success received for REFER request");
				}
				SipProtocolHelper.handleReferAccepted(sipResponse, callData,
						legData);
			} else if (sipResponse.getMethod().equals(PhConstants.NOTIFY_REQUEST)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Success received for NOTIFY request");
				}
			}else {
				logger.error(origLegCallId
						+ ":: Unexpected success response received, do nothing");
				logger.error(origLegCallId
						+ ":: Connection "
						+ connectionType
						+ " at State "
						+ legData
						.get(LegDataAttributes.P_LEG_SIP_STATE));
				// TODO: Confirm if call needs to be dropped here
			}
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Failed to process Success Response with Call Id "
					+ resCallId, ex);
			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_SUCC_HANDLING);
			legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
					.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);
		}
	}

	/**
	 * This method handles BYE for the DFC sent, sends success response for BYE
	 * request, stops the No-Answer timer and marks the sip session ready to
	 * invalidate as all transaction are completed on it.This method sets this
	 * sip message to other leg to copy content, reason header etc.In case of an
	 * exception in handling BYE, call is dropped with CV=41.Don't do anything
	 * if cancel has been sent for the session, just Mark this session ready to
	 * invalidate as all transaction has been completed on it and return.For an
	 * error received for PRACK or BYE drop the call with error code 41.For
	 * error from ORIG_CONNECTION_TYPE cases does as follows:
	 * <ul>
	 * <li>Sets this sip message to other leg to copy content, reason header
	 * etc.
	 * <li>For an error response in CONNECTED or CONNECTION_IN_PROCESS state, if
	 * 491 error response is received for Re-Invite then service should start a
	 * timer and after that timer times out, PH re-send the Re-Invite, else drop
	 * the call with error code 41.
	 * <li>For an error response in TERMINATION_IN_PROGRESS and TERMINATED state
	 * invalidate the appSession.
	 * </ul>
	 * For error from TERM_CONNECTION_TYPE cases does as follws:
	 * <ul>
	 * <li>For an error response in CONNECTED or CONNECTION_IN_PROCESS state,
	 * 491 error response is received for Re-Invite then service starts a timer
	 * and after that timer times out, PH re-send the Re-Invite. PH drops the
	 * call(with CV=41) if other than 491 error is for REINVITE sent in resync
	 * call. In case REL does not come in 4xx, the release cause value would not
	 * be set so it is set to 41
	 * <li>For an error response in TERMINATION_IN_PROGRESS and TERMINATED for
	 * BYE/CANCEL sent on the leg. Just gives an attempt to invalidate the
	 * appSession.
	 * </ul>
	 * 
	 * @see javax.servlet.sip.SipServlet#doErrorResponse(javax.servlet.sip.SipServletResponse)
	 * @param sipResponse
	 *            represents the incoming SIP ERROR response
	 */
	public final void doErrorResponse(SipServletResponse sipResponse)
			throws ServletException, IOException {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData legData = null;
		String resCallId = sipResponse.getCallId();
	//	Protocol protocol = (Protocol) callData.get(CallDataAttribute.P_PROTOCOL);

		try {

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Received "
						+ sipResponse.getStatus() + " Error response with call id "
						+ resCallId + " for " + sipResponse.getMethod());
			}

			appSession = sipResponse.getApplicationSession();
			callData = SipProtocolUtil.getCallData(appSession);
			Protocol protocol = (Protocol) callData.get(CallDataAttribute.P_PROTOCOL);
			SipProtocolUtil.incrementNetworkTransactions(callData, 1);

			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			legData = SipProtocolUtil.getLegDataForSipSession(appSession,
					sipResponse.getSession(), callData);
			logger.info("leg data inside doErrorResponse::"+legData);
			State legState = null;
			legState = (State) legData
					.get(LegDataAttributes.P_LEG_SIP_STATE);
			
			if(legState==null &&protocol == Protocol.SIP){
				return;
			}
			/*
			 * changes  for PROXY
			 */
			if(protocol == Protocol.AIN_SCF && legState == null &&
					AinCallStates.PSX_ROUTING ==legData.get(LegDataAttributes.P_LEG_SS7_STATE)){
				legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);
				legData.set(LegDataAttributes.P_CAUSE_CODE, sipResponse.getStatus());
				Event event = new Event(EventType.EVENT_FAILURE, Protocol.AIN_SCF,
						CallDataAttribute.P_LEG1.name());
				ProtocolRouter.getInstance().execute(event, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
				sipResponse.getSession().invalidate();
				return;
			}else if ((protocol == Protocol.ITUINAPCS1_SCF || protocol == Protocol.ITUINAPCS2_SCF) &&
					legState == null && 
					InapCallStates.PSX_ROUTING==legData.get(LegDataAttributes.P_LEG_SS7_STATE)){
				legData.set(LegDataAttributes.P_LEG_SS7_STATE, InapCallStates.SERVICE_LOGIC);
				legData.set(LegDataAttributes.P_CAUSE_CODE, sipResponse.getStatus());
				Event event = new Event(EventType.EVENT_DISCONNECT, 
						(protocol == Protocol.ITUINAPCS1_SCF)?Protocol.ITUINAPCS1_SCF:Protocol.ITUINAPCS2_SCF,
								CallDataAttribute.P_LEG1.name());
				ProtocolRouter.getInstance().execute(event, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
				sipResponse.getSession().invalidate();
				return;
			}else if (protocol == Protocol.CAPV2_SCF  && legState == null && 
					CapV2CallStates.PSX_ROUTING==legData.get(LegDataAttributes.P_LEG_SS7_STATE)){
				legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.SERVICE_LOGIC);
				legData.set(LegDataAttributes.P_CAUSE_CODE, sipResponse.getStatus());
				Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.CAPV2_SCF,
								CallDataAttribute.P_LEG1.name());
				ProtocolRouter.getInstance().execute(event, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
				sipResponse.getSession().invalidate();
				return;
			}


			logger.info("leg state inside doErrorResponse::"+legState);
			String legId = (String) sipResponse.getSession().getAttribute(
					PhConstants.LEG_ID);
			logger.info("leg id inside doErrorResponse::"+legId);
			
			if (State.PROXY_CONNECT.equals(legState)) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: return from state PROXY_CONNECT");
				}
				
				legData.set(LegDataAttributes.P_CAUSE_CODE,sipResponse.getStatus() );

				Event event = new Event(EventType.EVENT_FAILURE, Protocol.SIP,
						legId);

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				return;

			}
			
			legData.set(LegDataAttributes.P_CAUSE_CODE,
					sipResponse.getStatus());
			Action lastAction = (Action) callData
					.get(CallDataAttribute.P_CURRENT_ACTION);

			if(SipProtocolHelper.isResendInvite(callData,sipResponse)){
				return;
			}

			if (lastAction != null
					&& lastAction.getActionType() == Action.ActionType.ACTION_CONNECT_PARALLEL) {
				SipRingParallel.handleErrorResponse(sipResponse, callData);
				return;
			}

			if (lastAction != null
					&& lastAction.getActionType() == Action.ActionType.ACTION_CONNECT_SERIAL
					&& sipResponse.getStatus() != SipServletResponse.SC_REQUEST_TERMINATED) {
				SipRingSerial.handleErrorResponse(sipResponse, callData);
				return;
			}

			if(sipResponse.getStatus()==SipServletResponse.SC_REQUEST_TERMINATED && lastAction.getActionType()==Action.ActionType.ACTION_DISCONNECT){
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Error response received for CANCEL sent return from here ");
				}

				Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.SIP,
						legId);
				ProtocolRouter.getInstance().execute(event, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
				// Mark this session  invalidate as all transaction has
				// been completed on it setting ready to invalidate attribute true causes problem in alternate routing 
				sipResponse.getSession().invalidate();
				return;
			}

			if(sipResponse.getStatus()==SipServletResponse.SC_REQUEST_TERMINATED && lastAction.getActionType()==Action.ActionType.ACTION_PICKUP_CALL){
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Error response received for CANCEL sent return from here for call pickup ");
				}
				sipResponse.getSession().invalidate();

				SipProtocolHelper.resynchCallPickupLegs(callData);
				return;
			}

			SipSession legSipSession = SipProtocolUtil.getSipSessionFromSessionId(
					origLegCallId, appSession, (String) legData.get(LegDataAttributes.P_SESSION_ID));



			// Connection type from where response received
			ConnectionType connectionType = (ConnectionType) legData
					.get(LegDataAttributes.P_CONNECTION_TYPE);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Connection type is "
						+ connectionType);
				logger.debug(origLegCallId
						+ ":: State "
						+ legData
						.get(LegDataAttributes.P_LEG_SIP_STATE));
			}

			/**
			 * Handle REFER error response
			 */
			if(PhConstants.REFER_REQUEST.equalsIgnoreCase(sipResponse
					.getMethod())){

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Received "
							+ sipResponse.getStatus() + " response with call id "
							+ resCallId + " for " + sipResponse.getMethod());
				}

				SipProtocolHelper.handleReferRejected(sipResponse, callData, legData,m_ReferDialogMap);
				return;	
			}

			/*
			 * Don't do anything if cancel has been sent for the session
			 * IMPORTANT: Don't put any logic before this check, otherwise
			 * behavior would become unpredictable . Reeta--> here we r returning from here if the response code is 487 
			 * commenting the previous code 
			 */


			if(sipResponse.getStatus()==SipServletResponse.SC_REQUEST_TERMINATED && lastAction.getActionType() != Action.ActionType.ACTION_TRANSFER_CONNECT){
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Error response received for CANCEL sent return from here ");
				}

				// Mark this session  invalidate as all transaction has
				// been completed on it setting ready to invalidate attribute true causes problem in alternate routing 
				sipResponse.getSession().invalidate();
				return;
			}

			if (sipResponse.getStatus() == PhConstants.SERVER_TRXN_TIMEOUT) {
				legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
			}

			if (PhConstants.PRACK_REQUEST.equalsIgnoreCase(sipResponse
					.getMethod())) {
				logger.warn(origLegCallId + ":: Error response "
						+ sipResponse.getStatus()
						+ " received for PRACK sent, do nothing");
				// SBTM-UAT-1583 Dont drop the call on error for PRACK
				/*
				 * No need to drop call on error for PRACK as it comes when
				 * error response received/sent from/to party-B. Even for non
				 * error response cases, party-B will send error on not receving
				 * the PRACK and call cleanup would trigger
				 * callData.setReasonForRelease
				 * (ReleaseReasonCode.ERR_RCVD_FOR_TERM_PRACK);
				 * callData.setReleaseCauseValue(41); dropCall(appSession);
				 */
				return;
			} else if (PhConstants.BYE_REQUEST.equalsIgnoreCase(sipResponse
					.getMethod())) {
				logger.warn(origLegCallId + ":: Error response "
						+ sipResponse.getStatus()
						+ " received for BYE sent, do nothing");
				// Set this session to READY_TO_INVALIDATE. FIx for bug#13800
				legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);

				// Invalidate appsession if this error is not for the BYE sent
				// after no-answer timer expiry
				if (!PhConstants.TRUE
						.equals(legData
								.get(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND))) {
					SipProtocolUtil.invalidateAppSession(appSession);
				}

				/*
				 * Commented to fix the issue related to unnecessary triggering
				 * of call cleanup of already dropped call No need to do
				 * anything as SN sends BYE when cleaning up the call if
				 * (connType == ServiceInterface.ORIG_CONNECTION_TYPE) {
				 * callData
				 * .setReasonForRelease(ReleaseReasonCode.ERR_RCVD_FOR_ORIG_BYE
				 * ); } else { callData.setReasonForRelease(ReleaseReasonCode.
				 * ERR_RCVD_FOR_TERM_BYE); } //Set this session to
				 * READY_TO_INVALIDATE. FIx for bug#13800
				 * sipResponse.getSession(
				 * ).setAttribute(PhConstant.READY_TO_INVALIDATE,
				 * PhConstant.TRUE); callData.setReleaseCauseValue(41);
				 * dropCall(appSession);
				 */
				return;
			}
			// Set this sip message to other leg to copy content, reason header
			// etc
			SipIsupHelper.setErrByeCanMsgOnOtherLeg(sipResponse);
			// As error response is received for INVITE, so remove
			// INVITE_PENDING_TRANS
			legData.remove(LegDataAttributes.P_INVITE_PENDING_TRANS);

			/*
			 * If error response is received from TERM connection then stop
			 * no-answer timer
			 */
			if (connectionType == ConnectionType.TERM_CONNECTION) {
				SipProtocolUtil.stopTimer(appSession,
						PhConstants.NO_ANSWER_TIMER);
			}

			Event event =null;
			switch (legState) {
			case CONN_IN_PROGRESS: {
				/*
				 * This case will only come if error response is received for
				 * initial INVITE request. And mPH sends initial INVITE to TERM
				 * leg only. Do not start InvitePendingTransTimer, as this
				 * timershould be started for Re-INVITE only
				 */

				if((lastAction.getActionType()==Action.ActionType.ACTION_CONNECT && lastAction.getConnectionMode() ==CONNECTIONMODE.EQSROUTING)){


					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: last action was  ACTION_CONNECT with  CONNECTIONMODE.EQSROUTING . its EQS writing failure try another EQSDestination ");
					}

					Object obj = legData
							.get(LegDataAttributes.NP_LAST_GW_ID);

					if (obj != null) {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: last action ACTION_CONNECT with  CONNECTIONMODE.EQSROUTING got failed for GWId  "
									+ obj +" Try another EQS GW");
						}

						SipProtocolHelper.connectTerm(appSession, callData,
								lastAction);
						return;
					}
				}

				event = SipProtocolHelper.tryNextContactFromList(origLegCallId,callData,sipResponse.getStatus(),legData,lastAction,appSession,sipResponse.getSession(),true);

				if(!sipResponse.getSession().isValid()){

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: sipSession of first contact was invalidated because trying on next contact .Returning form here no event to be notified to app");
					}

					return;
				}

				legData.set(LegDataAttributes.P_LEG_SIP_STATE,
						State.TERMINATED);
				legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
				// Unlink the sip sessions associated with this term session
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Unlink sip sessions");
				}
				sipResponse.getRequest().getB2buaHelper()
				.unlinkSipSessions(sipResponse.getSession());

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Inform service that error is received from "
							+ connectionType + " for INVITE");
				}

				if (SignalingTypeEnum.SIGNALING_TYPE.SIP_T
						.equals(legData
								.get(LegDataAttributes.P_SIGNALING_TYPE_KEY))) {
					SipIsupHelper.isBusyNoAnswerError(sipResponse);
				}

				// record the reason for release to capture it in CDR

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

				if(sipResponse.getStatus()==SipServletResponse.SC_BUSY_HERE){
					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							SipProtocolRelReasonCode.BUSY_RCVD_FROM_TERM);
				}

				if(sipResponse.getStatus()==SipServletResponse.SC_TEMPORARLY_UNAVAILABLE){
					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							SipProtocolRelReasonCode.NOANS_RCVD_FROM_TERM);
				}

				legData.set(LegDataAttributes.P_CAUSE_CODE,
						sipResponse.getStatus());


				if (lastAction.getActionType() == Action.ActionType.ACTION_TRANSFER_CONNECT) {
					event = new Event(EventType.EVENT_TRANSFER_FAILURE, Protocol.SIP,
							legId);
				} else {

					if (event == null) {
						event = new Event(EventType.EVENT_FAILURE,
								Protocol.SIP, legId);
					}
				}

				if (sipResponse.getSession().isValid()) {

					if(logger.isDebugEnabled()){
						logger.debug(origLegCallId
								+ ":: Invalid Sip session so that its not avaialble in cleansessions ");
					}
					sipResponse.getSession().invalidate();
				}
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				break;

			}
			case CONNECTED:
			case MS_DISCONNECTED:
			case MS_CONN_IN_PROGRESS:{
				/*
				 * Fix for SBTM-UAT-745. If 491 error response is received for
				 * Re-Invite then service should start a timer and after that
				 * timer times out, PH should re-send the Re-Invite.
				 */
				if (sipResponse.getStatus() == SipServletResponse.SC_REQUEST_PENDING) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Received 491 for Reinvite. Start Re-Invite timer");
					}

					SipProtocolUtil.startInvitePendingTransTimer(appSession,
							legData,sipResponse);
				} else {
					/*
					 * The CONNECTED state is to handle the errors come for
					 * RE-INVITE sent to the leg. Don't invalidate sip session
					 * in this case because BYE needs to be sent to this leg for
					 * cleanup
					 */

					logger.error(origLegCallId
							+ ":: Drop call due to error resp of reinvite to "
							+ connectionType);
					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							SipProtocolRelReasonCode.ERR_RCVD_FOR_REINV);
					legData.set(LegDataAttributes.P_CAUSE_CODE,
							Integer.parseInt(SipProtocolUtil.getConfig(

									SipProtocolConfig.SIP_RES_CODE_IN_ERROR)));
					/*
					 * Do not mark sip session as ready to invalidate so that
					 * cleanupSipLeg() can give an attempt to cleanup this leg.
					 */
					SipProtocolHelper.dropCall(appSession);
				}
				break;
			}
			default: {
				logger.error(origLegCallId
						+ ":: Error response received in invalid state, "
						+ legState + " for connection " + connectionType
						+ ". Drop call");
				callData.set(
						CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.UNEXPECTED_ERR_MSG);
				legData.set(LegDataAttributes.P_CAUSE_CODE,
						Integer.parseInt(SipProtocolUtil.getConfig(
								SipProtocolConfig.SIP_RES_CODE_IN_ERROR)));
				/*
				 * Do not mark sip session as ready to invalidate so that
				 * cleanupSipLeg() can give an attempt to cleanup this leg.
				 */
				SipProtocolHelper.dropCall(appSession);
				break;
			}
			}
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Failed to process Error Response with Call Id "
					+ resCallId, ex);
			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_ERR_HANDLING);
			legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
					.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);
		}
	}


	public final void doAck(SipServletRequest sipRequest)
			throws ServletException, IOException {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData legData = null;
		String reqCallId = sipRequest.getCallId();

		try {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: doAck received ");
			}
			appSession = sipRequest.getApplicationSession();
			callData = SipProtocolUtil.getCallData(appSession);
			legData = SipProtocolUtil.getLegDataForSipSession(appSession,
					sipRequest.getSession(), callData);
			ConnectionType connType = (ConnectionType) legData
					.get(LegDataAttributes.P_CONNECTION_TYPE);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			SipSession legSipSession = SipProtocolUtil.getSipSessionFromSessionId(
					origLegCallId, appSession, (String) legData.get(LegDataAttributes.P_SESSION_ID));

			/*
			 * Fix for bug#16863
			 */
			String ackForRes = (String) sipRequest.getSession().getAttribute(
					PhConstants.ACK_FOR_RESPONSE);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: ackForRes " + ackForRes);
			}

			if(PhConstants.SUCCESS.equals(ackForRes)){
				SipProtocolUtil.incrementNetworkTransactions(callData, 1);
			}

			Action lastAction = (Action) callData
					.get(CallDataAttribute.P_CURRENT_ACTION);

			if (Action.ActionType.ACTION_REDIRECT.equals(lastAction
					.getActionType())) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: looks like ack received for 302 !!!");
				}
				legSipSession.setAttribute(
						PhConstants.SESSION_READY_TO_INVALIDATE,
						PhConstants.TRUE);

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_REDIRECTION_COMPLETED,
						Protocol.SIP, CallDataAttribute.P_LEG1.name());
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				return;
			}

			if (PhConstants.TRUE.equals(callData
					.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))
					&& !PhConstants.SUCCESS.equals(ackForRes)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Call already dropped. Invalidating App Session");
				}
				/*
				 * Mark the sip session ready to invalidate as this ACK is
				 * received for some error response that was send to A Party
				 */
				legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
				SipProtocolUtil.invalidateAppSession(appSession);
				return;
			}

			switch (connType) {
			case TERM_CONNECTION: {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: ACK received from term. Do nothing.");
				}
				break;
			}
			case ORIG_CONNECTION: {
				// Term leg information
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: ACK received from orig");
				}

				//				if (Action.ActionType.ACTION_CONNECT_PARALLEL.equals(lastAction
				//						.getActionType())) {
				//					if (logger.isDebugEnabled()) {
				//						logger.debug(origLegCallId
				//								+ ":: parallel connect completed!!!");
				//					}
				//	
				//					ServiceInterface serviceHandler = PhUtilityServices
				//							.getInstance().getServiceHandler();
				//					Event event = new Event(EventType.EVENT_SUCCESS,
				//							Protocol.SIP, CallDataAttribute.P_LEG2.name());
				//					ProtocolRouter.getInstance().execute(event, callData,
				//							serviceHandler);
				//					return;
				//				}
				LegData termLegData = SipProtocolUtil
						.getLegDataForConnectionType(appSession,
								ConnectionType.TERM_CONNECTION, callData);
				if (termLegData != null) {

					/**
					 * while resynching legs the legs may belong to diffrent app session thats why taking app sesison from leg id
					 */
					if (lastAction.getActionType() == Action.ActionType.ACTION_PICKUP_CALL) {
						appSession = SipProtocolUtil.getAppSessionForLegId(
								origLegCallId, termLegData,callData);
					}

					SipSession termSipSession = SipProtocolUtil
							.getSipSessionFromSessionId(
									origLegCallId,
									appSession,
									(String) termLegData
									.get(LegDataAttributes.P_SESSION_ID));

					SipProtocolUtil.setReceivedSdp(sipRequest, callData);

					SipServletRequest pendingTermAck = (SipServletRequest) termLegData
							.get(LegDataAttributes.P_PENDING_ACK);

					if (pendingTermAck != null) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Send pending ACK to term with updated SDP");
						}

						MultipartBody currentReceivedSdp = (MultipartBody) legData
								.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

						pendingTermAck.setContent(
								currentReceivedSdp.getContent(),
								currentReceivedSdp.getContentType());
						pendingTermAck.send();
						termLegData
						.remove(LegDataAttributes.P_PENDING_ACK);

						// Set last send sdp
						SipProtocolUtil.setSentSdp(pendingTermAck, termLegData,
								origLegCallId);

						termLegData
						.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
					}
					Action currentAction = (Action) callData
							.get(CallDataAttribute.P_CURRENT_ACTION);
					State termState = (State) termLegData
							.get(LegDataAttributes.P_LEG_SIP_STATE);
					String termLegId = (String) termSipSession
							.getAttribute(PhConstants.LEG_ID);
					if (currentAction.getActionType() == ActionType.ACTION_CONNECT_MS
							&& currentAction.getLeg().equals(termLegId)
							&& termState == State.CONNECTED) {
						/*
						 * Connect Term to Media-server Fix for SBTM-UAT-928
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Initiate media server connection for party B");
						}
						SipProtocolHelper.initiateMediaServerConnection(
								appSession, callData, termLegId);
					}

					/**
					 * call flow will reach here . when orig was connected with ivr in early media and then connected with term
					 * update is sent to A after receiving 200 ok from term and then on success response of update from orig. 200 ok
					 * is sent to orig .so orig sends ack for this 200 ok which is received here
					 */

					Action lastLegAction = (Action) termLegData
							.get(LegDataAttributes.P_LEG_CURRENT_ACTION);

					if (((Action) callData
							.get(CallDataAttribute.P_CURRENT_ACTION))
							.getActionType() == ActionType.ACTION_TRY_REDIRECT_CONTACTS
							|| ((Action) callData
									.get(CallDataAttribute.P_CURRENT_ACTION))
							.getActionType() == ActionType.ACTION_RESYNC_CALL /*Check added to send event once ack is received for orig leg */
							|| (lastLegAction != null && (lastLegAction
									.getActionType() == ActionType.ACTION_TRY_REDIRECT_CONTACTS||lastLegAction
									.getActionType() == ActionType.ACTION_RESYNCH_LEGS))) {
						if (logger.isDebugEnabled()) {
							logger
							.debug(origLegCallId
									+ ":: Ack received from orig for initial invite ( it was connected with ivr in early media first) . Current action is ACTION_TRY_REDIRECT_CONTACTS was in early media. Inform service that orig/term connected successfully ");
						}
						// Set call connected time
						callData.set(CallDataAttribute.P_CALL_CONNECT_TIME, new Date());
						
						Date startTime = (Date) callData.get(CallDataAttribute.P_CALL_START_TIME);
						Date answerTime = (Date) callData.get(CallDataAttribute.P_CALL_CONNECT_TIME);
						long diffSeconds = (answerTime.getTime() - startTime.getTime())/ 1000 ;
						callData.set(CallDataAttribute.P_ALERTING_DURATION, Long.toString(diffSeconds));

						ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
						Event event = new Event(EventType.EVENT_RESYNC_SUCCESS, Protocol.SIP,
								CallDataAttribute.P_LEG2.name());
						ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
					}
				}
				break;
			}
			default: {
				logger.error("ACK received from unexpected connection type");
			}
			}

		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Failed to process ACK with Call Id " + reqCallId, ex);
			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_ACK_HANDLING);
			legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
					.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);
		}
	}

	/*
	 * Invoked by the server (via the service method) to handle incoming UPDATE
	 * requests. The implementation is for handling update messages as per RFC
	 * 4028. In case of an exception in handling UPDATE, call is dropped with
	 * CV=41.
	 * 
	 * @see
	 * javax.servlet.sip.SipServlet#doUpdate(javax.servlet.sip.SipServletRequest
	 * )
	 * 
	 * @param sipRequest represents the incoming SIP UPDATE request
	 */
	public final void doUpdate(SipServletRequest sipRequest)
			throws ServletException, IOException {
		if (logger.isDebugEnabled()) {
			logger.debug(sipRequest.getCallId() + " :: Inside doUpdate");
		}
		/*
		 * RFC 4028:
		 * "It is RECOMMENDED that the UPDATE request not contain an offer [4]"
		 * So we are not checking SDP in UPDATE request
		 */
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		SipServletResponse sipResponse = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData legData = null;
		String reqCallId = sipRequest.getCallId();
		try {
			appSession = sipRequest.getApplicationSession();
			callData = SipProtocolUtil.getCallData(appSession);

			if (callData != null) {
				origLegCallId = (String) callData
						.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

				// Fetch leg details
				legData = SipProtocolUtil.getLegDataForSipSession(appSession,
						sipRequest.getSession(), callData);
				ConnectionType connType = (ConnectionType) legData
						.get(LegDataAttributes.P_CONNECTION_TYPE);
				State state = (State) legData
						.get(LegDataAttributes.P_LEG_SIP_STATE);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Update received with call id = "
							+ sipRequest.getCallId() + " for connection "
							+ connType);
					logger.debug(origLegCallId + ":: connection " + connType
							+ " State=" + state);
				}
				
				if(State.PROXY_CONNECT.equals(state)){
					
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: return from state PROXY_CONNECT");
					}
					return;
					
				}
				/*
				 * if(callData.getOrigSipCallState() ==
				 * SIP_CALL_STATES.CONNECTED && callData .getTermSipCallState()
				 * == SIP_CALL_STATES.CONNECTED){
				 */
				/*
				 * Validate refresher tag 1. Update from A and B party should
				 * not contain refresher=UAS
				 */
				// Store session expire duration for future reference
				SipProtocolUtil.setSessionExpiryTime(sipRequest, legData,
						origLegCallId);

				String unsupportedRefresher = PhConstants.REFRESHER_UAS;
				String sessionExpiresHeaderVal = sipRequest
						.getHeader(PhConstants.SESSION_EXPIRE_HEADER);
				if (sessionExpiresHeaderVal != null
						&& sessionExpiresHeaderVal
						.contains(unsupportedRefresher)) {
					// As of now, SN does not support INVITE with session
					// refresher UAS
					logger.error(origLegCallId + ":: UPDATE with refresher "
							+ unsupportedRefresher + " not supported");
					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							SipProtocolRelReasonCode.UNSUPPORTED_SE_HEADER);
					//					sipResponse = sipRequest
					//							.createResponse(SipServletResponse.SC_NOT_ACCEPTABLE_HERE);

					SipProtocolMessageCreator.createResponse(origLegCallId, sipRequest, SipServletResponse.SC_NOT_ACCEPTABLE_HERE, callData).send();
					//	sipResponse.send();
					return;
				}
				/*
				 * Restart Session refresh timers
				 */
				SipProtocolUtil.startSessionRefreshTimer(appSession, legData);

				/** 
				 * bug LEV-1308 code here
				 */
				if (sipRequest.getContentLength() > 0) {


					/**
					 * set received sdp
					 */
					SipProtocolUtil.setReceivedSdp(sipRequest, callData);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Forward UPDATE it transparently to other leg");
					}

					legData.set(LegDataAttributes.P_PENDING_UPDATE, sipRequest);

					LegData peerLegData = SipProtocolUtil.getPeerLegData(
							callData, connType);
					SipSession peerLegSipSession = SipProtocolUtil
							.getSipSessionFromSessionId(
									origLegCallId,
									appSession,
									(String) peerLegData
									.get(LegDataAttributes.P_SESSION_ID));

					// To fix "Dialog Terminated" exception observed in
					// production
					if (peerLegSipSession == null
							|| peerLegSipSession.getState() == javax.servlet.sip.SipSession.State.TERMINATED) {
						if (logger.isInfoEnabled()) {
							logger.info(origLegCallId
									+ ":: Peer SIP session state is terminated, can not send UPDATE");
						}

						sipResponse = SipProtocolMessageCreator
								.createSuccessResponseUpdate(sipRequest);
						sipResponse.send();
						return;
					}

					SipServletRequest peerUpdateRequest = SipProtocolMessageCreator
							.createRequest(origLegCallId, peerLegSipSession,
									PhConstants.UPDATE_REQUEST, callData);

					if (sipRequest.getContentLength() > 0) {
						peerUpdateRequest.setContent(sipRequest.getContent(),
								sipRequest.getContentType());
					}
					SipProtocolUtil.setSentSdp(peerUpdateRequest, peerLegData, origLegCallId);
					SipProtocolUtil.copyHeaders(callData, sipRequest, peerUpdateRequest);
					peerUpdateRequest.send();

				} else {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Donot send UPDATE to other leg as it donot contain content");
					}

					sipResponse = SipProtocolMessageCreator
							.createSuccessResponseUpdate(sipRequest);
					sipResponse.send();
				}

			} else {
				logger.error("CallData is null while handling UPDATE for call-id "
						+ reqCallId);

				sipResponse = SipProtocolMessageCreator
						.createSuccessResponseUpdate(sipRequest);
				sipResponse.send();
			}

		} catch (Exception ex) {
			logger.error(
					origLegCallId + ":: Failed to process UPDATE with Call Id "
							+ sipRequest.getCallId(), ex);
			if (callData != null) {
				callData.set(
						CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.EXCEP_IN_UPDATE_HANDLING);
				legData.set(LegDataAttributes.P_CAUSE_CODE,
						Integer.parseInt(SipProtocolUtil.getConfig(
								SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
				SipProtocolHelper.dropCall(appSession);
			}
		}
	}

	public final void doBye(SipServletRequest sipRequest)
			throws ServletException, IOException {

		if (logger.isDebugEnabled()) {
			logger.debug(sipRequest.getCallId() + ":: Inside doBye()");
		}

		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData legData = null;
		SipSession legSipSession = null;
		String reqCallId = sipRequest.getCallId();

		try {
			appSession = sipRequest.getApplicationSession();
			callData = SipProtocolUtil.getCallData(appSession);

			if(callData == null) {
				if(logger.isDebugEnabled()) {
					logger.debug("callData is null, so returning");
				}
				return;
			}
			origLegCallId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			String legId = (String) sipRequest.getSession().getAttribute(PhConstants.LEG_ID);

			legData = SipProtocolUtil.getLegDataForSipSession(appSession, sipRequest.getSession(), callData);

			legSipSession = SipProtocolUtil.getSipSessionFromSessionId(origLegCallId, appSession, (String) legData.get(LegDataAttributes.P_SESSION_ID));

			SipSession sipSession = sipRequest.getSession();
			// This is to handle BYE for the DFC sent
			if (PhConstants.TRUE.equals(sipRequest.getSession().getAttribute(PhConstants.ASSIST_LEG))) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: BYE received for ASSIST leg");
				}

				/*
				 * This check is added to handle the case where Assist
				 * appSession is used for Handoff. As per the written logic in
				 * this class, there is only one place i.e.
				 * connectTermIvr(appSession, action) where appSession of Assist
				 * is used for Handoff. It is also not certain whether that
				 * piece of code gets executed in any case or not because there
				 * is already a check in executeAction(appSession, actions [])
				 * method to call tcap session method if current inap call state
				 * is ASSIST. However just to ensure that appSession does not
				 * get invalidated in such cases, following check added.
				 */
				if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) == InapCallStates.HANDOFF
						&& PhConstants.TRUE.equals(appSession.getAttribute(PhConstants.FOR_HANDOFF))) {
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ ":: Assist AppSession is used for Handoff so just send 200 OK");
					}
					// Send success response for BYE request
					SipProtocolMessageCreator.createSuccessResponseBye(sipRequest).send();
					// Mark the sip session ready to invalidate as all
					// transaction are completed on it
					sipSession.setAttribute(PhConstants.READY_TO_INVALIDATE, PhConstants.TRUE);
					return;
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Disconnect IVR leg if exist");
				}

				LegData ivrLegData = SipProtocolUtil.getLegDataForConnectionType(appSession, ConnectionType.IVR_CONNECTION, callData);
				SipServletRequest ivrInviteRequest = SipProtocolUtil.getInitialInvite(appSession, ivrLegData);
				Object ftCall = callData.get(CallDataAttribute.NP_FT_CALL);
				if (ivrInviteRequest != null) {
					SipSession ivrSipSession = ivrInviteRequest.getSession();
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId + ":: Disconnect Orig MS leg. Sip Session State is " + ivrSipSession.getState());
					}
					try {
						switch (ivrSipSession.getState()) {
						case INITIAL:
						case EARLY:
						case CONFIRMED:
							GroupedMsSessionController msController = SipProtocolUtil
							.getMsController(appSession, legData, legId);
							/*
							 * Sometimes B is already available to SAS after FT,
							 * in that case SAS throws runtime exception. So
							 * adding following try catch block to ignore that
							 * exception
							 */
							try {
								if (!(ftCall != null && ftCall.equals(PhConstants.TRUE) || msController.getB() == null)) {
									msController.addB(ivrSipSession);
								}
							} catch (Throwable e) {
								logger.warn(origLegCallId + " B already added in Orig MS Sip Session");
							}
							msController.setApplicationSession(appSession);
							msController.setEventListener(SipProtocolHelper.cMsEventListener);
							msController.disconnectMediaServer();
							break;
						default:
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId + ":: Orig MS leg is already in terminated state");
							}
							ivrSipSession.setAttribute(PhConstants.READY_TO_INVALIDATE, PhConstants.TRUE);
							break;
						}
					} catch (Exception ex) {
						logger.warn(origLegCallId + ":: Failed to cleanup IVR leg. " + ex.getMessage());
						if (logger.isInfoEnabled()) {
							logger.warn(origLegCallId + ":: Error cleaning ASSIST IVR leg. ", ex);
						}
						ivrSipSession.setAttribute(PhConstants.READY_TO_INVALIDATE, PhConstants.TRUE);
					}
				}
				legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.TERMINATION_IN_PROGRESS);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Send 200 OK for BYE");
				}

				// Send success response for BYE request
				SipProtocolMessageCreator.createSuccessResponseBye(sipRequest).send();
				SipProtocolUtil.stopTimer(appSession, PhConstants.CORRELATED_ORIG_CLEANUP_TIMER);
				// Mark the sip session ready to invalidate as all transaction
				// are completed on it
				sipSession.setAttribute(PhConstants.READY_TO_INVALIDATE, PhConstants.TRUE);
				SipProtocolUtil.invalidateAppSession(appSession);
				return;
			}

			// Set this sip message to other leg to copy content, reason header
			// etc
			SipIsupHelper.setErrByeCanMsgOnOtherLeg(sipRequest);

			ConnectionType connType = (ConnectionType) legData
					.get(LegDataAttributes.P_CONNECTION_TYPE);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			State state = (State) legData
					.get(LegDataAttributes.P_LEG_SIP_STATE);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: BYE received with call id = "
						+ sipRequest.getCallId() + " for connection "
						+ connType);
				logger.debug(origLegCallId + ":: connection " + connType
						+ " State=" + state);
			}

			
			if(state == State.PROXY_CONNECT){
				
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Return as it is proxy");
				}
				return;
			}
			
			Event event = null;
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			
			switch (connType) {
			
			 case ORIG_CONNECTION: {
				
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: BYE received for Orig Leg. Inform service");
				}

				// Stop No-Answer timer
				SipProtocolUtil.stopTimer(appSession,
						PhConstants.NO_ANSWER_TIMER);
				// Stop session-refresh timer
				SipProtocolUtil.stopSessionRefreshTimer(appSession, legData);
				legData.set(LegDataAttributes.P_LEG_SIP_STATE,
						State.TERMINATED);
				// Send success response for BYE request
				SipProtocolMessageCreator.createSuccessResponseBye(sipRequest)
				.send();

				legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);

				callData.set(
						CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.BYE_RCVD_FROM_ORIG);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Inform service that BYE is received from orig");
				}

				if (state == State.CALL_TRANSFERED) { //|| (isTransferedCall != null && isTransferedCall.equals(PhConstants.TRUE))

					if (callData
							.get(CallDataAttribute.P_TRANSFERED_CALL_ID) == null) {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: P_TRANSFERED_CALL_ID not found update it as "+origLegCallId);
						}
						callData.set(
								CallDataAttribute.P_TRANSFERED_CALL_ID,
								origLegCallId);
					}

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Inform service that transfered call is disconnected with transfer indicator  "+callData
								.get(CallDataAttribute.P_TRANSFERED_CALL_IND));
					}
					event = new Event(EventType.EVENT_CALL_TRANSFER_DISCONNECT,
							Protocol.SIP, legId);
				}else{
					event= new Event(EventType.EVENT_DISCONNECT,
							Protocol.SIP, legId);
				}


				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				break;
			}
			case TERM_CONNECTION: {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: BYE received for Term Leg. Inform service");
				}
				
				// Stop session-refresh timer
				SipProtocolUtil.stopSessionRefreshTimer(appSession, legData);
				legData.set(LegDataAttributes.P_LEG_SIP_STATE,
						State.TERMINATED);
				// Fix for bug#24438
				legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);

				// Send success response for BYE request
				SipProtocolMessageCreator.createSuccessResponseBye(sipRequest)
				.send();

				callData.set(
						CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.BYE_RCVD_FROM_TERM);

				//				ServiceInterface serviceHandler = PhUtilityServices
				//						.getInstance().getServiceHandler();

				if (state == State.CALL_TRANSFERED) { //|| (isTransferedCall != null && isTransferedCall.equals(PhConstants.TRUE))

					if (callData
							.get(CallDataAttribute.P_TRANSFERED_CALL_ID) == null) {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: P_TRANSFERED_CALL_ID not found update it as "+origLegCallId);
						}
						callData.set(
								CallDataAttribute.P_TRANSFERED_CALL_ID,
								origLegCallId);
					}

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Inform service that transfered call is disconnected with transfer indicator  "+callData
								.get(CallDataAttribute.P_TRANSFERED_CALL_IND));
					}
					event = new Event(EventType.EVENT_CALL_TRANSFER_DISCONNECT,
							Protocol.SIP, legId);
				} else {
					event =new Event(EventType.EVENT_DISCONNECT, Protocol.SIP, legId);
				}

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				break;
			}
			default: {
				logger.error(origLegCallId
						+ ":: Unknown connection type, do nothing");
				break;
			}
			}
		} catch (Exception ex) {
			logger.error(
					origLegCallId + ":: Failed to process BYE with Call Id "
							+ sipRequest.getCallId(), ex);

			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_BYE_HANDLING);
			legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
					.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);

		}
	}

	/**
	 * Invoked by the server (via the service method) to handle incoming CANCEL
	 * requests. Sets this CANCEL sip message to other leg to copy content,
	 * reason header. It stops the No-Answer timer and marks the sip session
	 * ready to invalidate as all transaction are completed on it In case of an
	 * exception in handling CANCEL, call is dropped with CV=41.
	 * 
	 * @see javax.servlet.sip.SipServlet#doCancel(javax.servlet.sip.SipServletRequest)
	 * @param sipRequest
	 *            represents the incoming SIP CANCEL request
	 */
	public final void doCancel(SipServletRequest sipRequest)
			throws ServletException, IOException {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData legData = null;
		try {
			appSession = sipRequest.getApplicationSession();
			callData = SipProtocolUtil.getCallData(appSession);
			// Get LegData
			legData = SipProtocolUtil.getLegDataForSipSession(appSession,
					sipRequest.getSession(), callData);
			State state = (State) legData
					.get(LegDataAttributes.P_LEG_SIP_STATE);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: CANCEL received with call id = "
						+ sipRequest.getCallId() + " from orig at state "
						+ state);
			}
			
			
			String legId = (String) sipRequest.getSession().getAttribute(
					PhConstants.LEG_ID);
			if (State.PROXY_CONNECT.equals(state)) {

				
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: return from state PROXY_CONNECT for proxying  "+sipRequest);
				}
				
			//	sipRequest.send();
				
				return;
				
				
//				Event event = new Event(EventType.EVENT_FAILURE, Protocol.SIP,
//						legId);
//
//				ServiceInterface serviceHandler = PhUtilityServices
//						.getInstance(
//								(String) callData
//										.get(CallDataAttribute.SERVICE_ID))
//						.getServiceHandler();
//
//				ProtocolRouter.getInstance().execute(event, callData,
//						serviceHandler);
//				return;

			}
			/*
			 * SBTM-UAT-555: If following attribute is set on orig SIP Session,
			 * then SipInapIsupHandlerServlet.cleanupSipSession() will send 487
			 * for INVITE on orig leg
			 */
			legData.set(LegDataAttributes.P_IS_CANCEL_RECEIVED,
					PhConstants.TRUE);

			// Set this sip message to other leg to copy content, reason header
			// etc
			SipIsupHelper.setErrByeCanMsgOnOtherLeg(sipRequest);

			// Set this sip message to other leg to copy content, reason header
			// etc
			// SipProtocolUtil.setErrByeCanMsgOnOtherLeg(sipRequest);

			// Stop No-Answer timer
			SipProtocolUtil.stopTimer(appSession, PhConstants.NO_ANSWER_TIMER);

			SipProtocolUtil.stopSessionRefreshTimer(appSession, legData);

			legData.set(LegDataAttributes.P_LEG_SIP_STATE,
					State.TERMINATED);

			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.CANCEL_RCVD_FROM_ORIG);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inform service that orig leg disconnected");
			}
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();//.getAppChainManager().getServiceInterface((String)callData.get(CallDataAttribute.SERVICE_ID));

			
			
			Action lastAction = (Action) callData
					.get(CallDataAttribute.P_CURRENT_ACTION);

			if(lastAction!=null && lastAction.getActionType()==Action.ActionType.ACTION_CONNECT_PARALLEL){
				SipRingParallel.handleCancelRequest(sipRequest, callData);
				return;
			}

			if(lastAction!=null && lastAction.getActionType()==Action.ActionType.ACTION_CONNECT_SERIAL){
				SipRingSerial.handleCancelRequest(sipRequest, callData);
				return;
			}

			Event event=null;
			if (lastAction!=null && lastAction.getActionType() == Action.ActionType.ACTION_TRANSFER_CONNECT) {
				event = new Event(EventType.EVENT_TRANSFER_FAILURE,
						Protocol.SIP, CallDataAttribute.P_LEG1.name());
			} else {
				event = new Event(EventType.EVENT_DISCONNECT,
						Protocol.SIP, CallDataAttribute.P_LEG1.name());
			}

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);
		} catch (Exception ex) {
			logger.error(
					origLegCallId + ":: Failed to process CANCEL with Call Id "
							+ sipRequest.getCallId(), ex);

			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_CANCEL_HANDLING);
			legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
					.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);

		}
	}

	/**
	 * Invoked by the server (via the service method) to handle incoming INFO
	 * requests. It takes care of INFO - 200OK exchange during Media server
	 * interaction. Also forwards the received INFO SUS/RES from either end to
	 * the other end transparently . In case of an exception in handling INFO,
	 * call is dropped with CV=41.
	 * 
	 * @see javax.servlet.sip.SipServlet#doInfo(javax.servlet.sip.SipServletRequest)
	 * @param sipRequest
	 *            represents the incoming SIP INFO request
	 */
	public final void doInfo(SipServletRequest sipRequest)
			throws ServletException, IOException {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData legData = null;
		try {
			appSession = sipRequest.getApplicationSession();
			callData = SipProtocolUtil.getCallData(appSession);
			// Get LegData
			legData = SipProtocolUtil.getLegDataForSipSession(appSession,
					sipRequest.getSession(), callData);
			State state = (State) legData
					.get(LegDataAttributes.P_LEG_SIP_STATE);
			ConnectionType connType = (ConnectionType) legData
					.get(LegDataAttributes.P_CONNECTION_TYPE);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: INFO received with call id = "
						+ sipRequest.getCallId() + " from " + connType
						+ " at state " + state);
			}
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Send 200 OK for INFO received");
			}
			//			SipServletResponse sipResponse = sipRequest
			//					.createResponse(SipServletResponse.SC_OK);

			SipProtocolMessageCreator.createResponse(origLegCallId, sipRequest, SipServletResponse.SC_OK, callData).send();

			if (sipRequest.getContentLength() > 0
					&& sipRequest.getContentType().equals(PhConstants.APP_DTMF_CONTENT_TYPE)) {

				sipRequest.createResponse(SipServletResponse.SC_OK).send();

				if (logger.isDebugEnabled()) {
					logger.debug(":: DTMF received in INFO Message");
				}
				Object content=sipRequest.getContent();

				String digits=null;
				if(content instanceof byte[] ){
					digits=new String((byte[]) content);
				}else if(content instanceof String){
					digits=(String)content;
				}

				if (logger.isDebugEnabled()) {
					logger.debug(":: DTMF received is : "+digits);
				}
				callData.set(CallDataAttribute.P_COLLECTED_DIGITS,
						digits);
				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_DTMF,
						Protocol.SIP, null);

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			} else {
				//sipResponse.send();
				/*
				 * SBTM-UAT-1292: GSX can send INFO+CHG in early state of SIP
				 * session. So removed CONNECTED state validations.
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Forward it transparently to other leg");
				}

				LegData peerLegData = SipProtocolUtil.getPeerLegData(callData,
						connType);
				SipSession peerLegSipSession = SipProtocolUtil
						.getSipSessionFromSessionId(
								origLegCallId,
								appSession,
								(String) peerLegData
								.get(LegDataAttributes.P_SESSION_ID));

				// To fix "Dialog Terminated" exception observed in production
				if (peerLegSipSession == null
						|| peerLegSipSession.getState() == javax.servlet.sip.SipSession.State.TERMINATED) {
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ ":: Peer SIP session state is terminated, can not send INFO");
					}
					return;
				}
				//			SipServletRequest peerInfoRequest = peerLegSipSession
				//					.createRequest(PhConstants.INFO_REQUEST);

				SipServletRequest peerInfoRequest=SipProtocolMessageCreator.createRequest(origLegCallId, peerLegSipSession, PhConstants.INFO_REQUEST, callData);
				peerInfoRequest.setContent(sipRequest.getContent(),
						sipRequest.getContentType());
				peerInfoRequest.send();
			}
		} catch (Exception ex) {
			logger.error(
					origLegCallId + ":: Failed to process INFO with Call Id "
							+ sipRequest.getCallId(), ex);

			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_INFO_HANDLING);
			legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
					.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);

		}
	}

	public final void doPrack(SipServletRequest sipRequest)
			throws ServletException, IOException {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData legData = null;
		String reqCallId = sipRequest.getCallId();

		try {
			appSession = sipRequest.getApplicationSession();
			callData = SipProtocolUtil.getCallData(appSession);
			legData = SipProtocolUtil.getLegDataForSipSession(appSession,
					sipRequest.getSession(), callData);
			ConnectionType connType = (ConnectionType) legData
					.get(LegDataAttributes.P_CONNECTION_TYPE);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Received PRACK request with call id " + reqCallId
						+ " for connection " + connType);
			}
			boolean sdpExists=false;
			if(SipProtocolUtil.isSDPExists(sipRequest, origLegCallId)){
				sdpExists=true;
			}
			
			SipProtocolUtil.setReceivedSdp(sipRequest, callData);
			switch (connType) {
			case ORIG_CONNECTION: {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: PRACK received on orig leg");
				}
				State legState = (State) legData
						.get(LegDataAttributes.P_LEG_SIP_STATE);
				switch (legState) {
				case CONN_IN_PROGRESS:
				case MS_DISCONNECTED:{
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Orig state is "+ legState);
					}
					LegData termLegData = SipProtocolUtil.getPeerLegData(
							callData, connType);
					State termLegState = (State) termLegData
							.get(LegDataAttributes.P_LEG_SIP_STATE);
					if (termLegState == State.CONN_IN_PROGRESS) {
						// Handle PRACK in TermSipCallState
						// CONNECTION_IN_PROGRESS
						// Send pending PRACK on term leg
						SipServletRequest termPendingPrack = (SipServletRequest) termLegData
								.get(LegDataAttributes.NP_PENDING_PRACK);
						if (termPendingPrack != null) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ "::Send pending prack on term");
							}
							// Set SDP content in pending prack
							boolean isOfferRcvd = PhConstants.TRUE
									.equals(termLegData
											.get(LegDataAttributes.NP_IS_OFFER_RECEIVED));
							if (isOfferRcvd) {
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ ":: Set SDP in pending PRACK request");
								}

								MultipartBody sdpContentFromOrig = (MultipartBody) legData
										.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

								termPendingPrack.setContent(
										sdpContentFromOrig.getContent(),
										sdpContentFromOrig.getContentType());
								termLegData
								.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
							} else {

								/*
								 * need to check here if orig had updated sdp in
								 * update success of 200 ok and 183 was
								 * forwarded to orig ater update then we need to
								 * send sdp received in update success in prack
								 * to term
								 */

								if (sdpExists) {
									MultipartBody termLastSentSdp = (MultipartBody) termLegData
											.get(LegDataAttributes.P_LAST_SENT_SDP);
									MultipartBody origCurrentRcvdSdp = (MultipartBody) legData
											.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

//<<<<<<< HEAD
//								if (SipProtocolUtil.isSdpChanged(
//										origCurrentRcvdSdp, termLastSentSdp,
//										origLegCallId)) {
//
//									if(logger.isDebugEnabled()){
//										logger.debug(" Need to send updated sdp in prack to term");
//									}
//									if (origCurrentRcvdSdp != null) {
//										termPendingPrack
//										.setContent(
//												origCurrentRcvdSdp
//												.getContent(),
//												origCurrentRcvdSdp
//												.getContentType());
//=======
									if (SipProtocolUtil.isSdpChanged(
											origCurrentRcvdSdp,
											termLastSentSdp, origLegCallId)) {

										if (logger.isDebugEnabled()) {
											logger.debug(" Need to send updated sdp in prack to term");
										}
										if (origCurrentRcvdSdp != null) {
											termPendingPrack.setContent(
													origCurrentRcvdSdp
															.getContent(),
													origCurrentRcvdSdp
															.getContentType());
										}
										SipProtocolUtil.setSentSdp(
												termPendingPrack, termLegData,
												origLegCallId);
									}
								}else{
									
									if (logger.isDebugEnabled()) {
										logger.debug(" SDP donot exists so donot set");
									}
								}
							}
							
							SipProtocolUtil.setSentSdp(termPendingPrack, termLegData, origLegCallId);
							SipProtocolUtil.copyHeaders(callData, sipRequest, termPendingPrack);
							termPendingPrack.send();
							termLegData
							.remove(LegDataAttributes.NP_PENDING_PRACK);
						}
					}
					// Send 200 OK for Prack
					SipServletResponse prackRes=SipProtocolMessageCreator.createResponse(origLegCallId, sipRequest, SipServletResponse.SC_OK, callData);// 
					legData.set(LegDataAttributes.NP_PENDING_PRACK_SUCCESS, prackRes);//.send();

					break;
				}
				default: {
					
					// Send 200 OK for Prack
					if (legState != State.PROXY_CONNECT) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: PRACK received from orig. But orig state is "
									+ legState + ". so donot do anything");
						}
						SipProtocolMessageCreator.createResponse(origLegCallId,
								sipRequest, SipServletResponse.SC_OK, callData)
								.send();
					} else {
						logger.error(origLegCallId
								+ ":: PRACK received from orig. But orig state is "
								+ legState + ". Just send 200 OK for it");
					}
					break;
				}
				}
				break;
			}
			default: {
				logger.error(origLegCallId
						+ ":: PRACK received unexpected conn type " + connType
						+ ". Just send 200 OK for it");

				// Send 200 OK for Prack
				//sipRequest.createResponse(SipServletResponse.SC_OK).send();
				SipProtocolMessageCreator.createResponse(origLegCallId, sipRequest, SipServletResponse.SC_OK, callData).send();
				break;
			}
			}

		} catch (Exception ex) {
			logger.error(
					origLegCallId + ":: Failed to process PRACK with Call Id "
							+ sipRequest.getCallId(), ex);

			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_PRAK_HANDLING);
			legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
					.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);

		}
	}

	/**
	 * This is invoked when ACK is not received. Drops the call with CV=41.
	 * 
	 * @see javax.servlet.sip.SipErrorListener#noAckReceived(javax.servlet.sip.SipErrorEvent)
	 * @param arg0
	 *            represents SipErrorEvent Instance
	 */
	public void noAckReceived(SipErrorEvent arg0) {
		SipServletRequest sipRequest = arg0.getRequest();
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		// CallData callData = (CallData)
		// appSession.getAttribute(CallData.CALL_DATA);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		logger.error(origLegCallId + ":: ACK not received for callId "
				+ sipRequest.getCallId() + ", drop call");

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Drop the call with error response");
		}
		LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
				sipRequest.getSession(), callData);
		callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
				SipProtocolRelReasonCode.ACK_TIMED_OUT);
		legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
				.parseInt(SipProtocolUtil.getConfig(
						SipProtocolConfig.SIP_RES_CODE_IN_ERROR)));
		SipProtocolHelper.dropCall(appSession);
	}

	/**
	 * This is invoked when PRACK is not received. Drops the call with CV=41.
	 * 
	 * @see javax.servlet.sip.SipErrorListener#noPrackReceived(javax.servlet.sip.SipErrorEvent)
	 * @param arg0
	 *            represents SipErrorEvent Instance
	 */
	public void noPrackReceived(SipErrorEvent arg0) {
		SipServletRequest sipRequest = arg0.getRequest();
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		// CallData callData = (CallData)
		// appSession.getAttribute(CallData.CALL_DATA);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		logger.error(origLegCallId + ":: PRACK not received for callId "
				+ sipRequest.getCallId() + ", drop call");

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Drop the call with error response");
		}
		LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
				sipRequest.getSession(), callData);
		callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
				SipProtocolRelReasonCode.PRACK_TIMED_OUT);
		legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
				.parseInt(SipProtocolUtil.getConfig(
						SipProtocolConfig.SIP_RES_CODE_IN_ERROR)));
	}

	/**
	 * This call-back method is called by container on any timer timeout. In
	 * this method, service checks the timer information. And if timer started
	 * by service, it performs appropriate action. It performs following
	 * activities:
	 * <p>
	 * If appsession instance is null then does nothing and returns. Implemets
	 * the check to avoid un-necessary processing on timeout of a timer, which
	 * has been removed from appSession due to some call cleanup activity or
	 * successful handoff/assist handling.
	 * <ul>
	 * <li>For CHARGING_TIMER, Start session refresh timer on orig leg,Set orig
	 * call state, Send success response to orig leg and inform the service
	 * about completion of activity.
	 * <li>For CDR_TIMER, set App-session timeout to 24 hours and 5 minute and
	 * replicate tcap session.
	 * <li>For AT_ACK_TIMER, mark call state to terminated to avoid other
	 * actionand notify the service.
	 * <li>For CORRELATION_TIMER expiry, drop the call.
	 * <li>For ORIG_SESSION_EXPIRE_TIMER, notifying the service with connection
	 * disconnected event and drop the call with CV=41.
	 * <li>For TERM_SESSION_EXPIRE_TIMER, notifying the service with connection
	 * disconnected event and drop the call with CV=41.
	 * <li>For ACCESS_GATEWAY_TIMER, notify service with operationFailed for
	 * HTTP_GET_OPERATION_TYPE.
	 * <li>For NO_ANSWER_TIMER unlink the session, set release cause code and
	 * notify the service that term connection failed.
	 * <li>For A_LEG_REINVITE_TIMER drop the call with CV=41 if term leg invite
	 * is missing else Send reinvite to orig leg.
	 * <li>For B_LEG_REINVITE_TIMER, drop the call with CV=41 if
	 * initialOrigInviteReq is null. Else Send reinvite to term leg.
	 * </ul>
	 * 
	 * @see javax.servlet.sip.TimerListener#timeout(javax.servlet.sip.ServletTimer)
	 * @param timer
	 *            represents ServletTimer instance
	 */
	public final void timeout(ServletTimer timer) {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData origLegData = null;
		try {
			appSession = timer.getApplicationSession();
			/*
			 * Announcements towards A and B are in progress and B-Party's
			 * session refresh timer timeout. This timer is managed by MS-Sbb
			 * but PH received callback for this. And in this timeout event
			 * AppSession is null. so to handle this case adding null check for
			 * apSession. Added isValid check to fix exceptions observed in
			 * production
			 */
			if (appSession == null || !appSession.isValid()) {
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Do nothing as timer appsession is null or invalidated");
				}
				return;
			}
			callData = SipProtocolUtil.getCallData(appSession);
			if(callData == null) {
				if(logger.isDebugEnabled()) {
					logger.debug("Call Data is null, so returning");
				}
				return;
			}
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			PhTimerInfo timerInfo = (PhTimerInfo) timer.getInfo();
			String timerName = timerInfo.getTimerName();
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Received timeout for the timer " + timerName);
			}
			/*
			 * This check is to avoid un-necessary processing on timeout of a
			 * timer, which has been removed from appSession due to some call
			 * cleanup activity or successful handoff/assist handling.
			 */
			if (appSession.getAttribute(timerName) == null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Do nothing as the timer is not in appsession");
					logger.debug(origLegCallId
							+ ":: Cleanup has been performed");
				}
				return;
			}

			origLegData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);
			State origState = (State) origLegData
					.get(LegDataAttributes.P_LEG_SIP_STATE);

			LegData termLegData = SipProtocolUtil.getPeerLegData(callData,
					ConnectionType.ORIG_CONNECTION);

			Object sessionRefTimerOrig = origLegData
					.get(LegDataAttributes.P_SESSION_REFRESH_TIMER_NAME);
			Object pendingInvTranOrigTimer = origLegData
					.get(LegDataAttributes.P_INV_PENDING_TRANS_TIMER_NAME);

			Object relFailureRetryTimer=origLegData.get(
					LegDataAttributes.P_100_REL_RETRY_TIMER_NAME);

			Object sessionRefTimerTerm = null;
			Object pendingInvTransTermTimer = null;

			if (termLegData != null) {
				sessionRefTimerTerm = termLegData
						.get(LegDataAttributes.P_SESSION_REFRESH_TIMER_NAME);
				pendingInvTransTermTimer = termLegData
						.get(LegDataAttributes.P_INV_PENDING_TRANS_TIMER_NAME);
			}
			if (timerName.equals(PhConstants.CALL_TRANSFER_TIMER)) {

				String referTokey=(String)termLegData.get(LegDataAttributes.NP_TRANSFER_KEY);

				if(referTokey!=null){

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Remove Transfer key from attributes "+referTokey);
					}
					termLegData.remove(LegDataAttributes.NP_TRANSFER_KEY);
				}else{

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: No Transfer key found in peer legdata can not remove from attributes");
					}
				}

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Remove refer-to mapping from m_ReferDialogMap");
				}
				m_ReferDialogMap.remove(referTokey);

			}
			if (timerName.startsWith(PhConstants.APP_TIMER)) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Application timer timedout ");
					logger.debug(origLegCallId
							+ ":: Hence notifying the service for application timer timeout event");
				}

				timerName = timerName.substring(timerName
						.indexOf(PhConstants.APP_TIMER) + 9);

				if (timerName.equals("")) {
					timerName = PhConstants.APP_TIMER;
				}

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_APP_TIMER_TIMEOUT,
						Protocol.SIP, null);
				event.setTimerName(timerName);

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				return;

			} else if (timerName.equals(PhConstants.NEXT_CALL_TIMER)){ 

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Next Call timer expired, Notify application to start next call processing");
				}

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_NEXT_CALL_TIMER_TIMEOUT,
						Protocol.SIP,
						CallDataAttribute.P_LEG1.name());
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

				return;

			}else if (sessionRefTimerOrig != null
					&& timerName.equals(sessionRefTimerOrig)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Session Refresh Timer expired for Orig end");
					logger.debug(origLegCallId
							+ ":: Hence notifying the service with connection disconnected event");
				}

				logger.warn(origLegCallId + ":: Originating session expired");

				callData.set(
						CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.SESSION_EXPIRED_ORIG);

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_SESSION_EXPIRED,
						Protocol.SIP, CallDataAttribute.P_LEG1.name());

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			} else if (termLegData != null && sessionRefTimerTerm != null
					&& timerName.equals(sessionRefTimerTerm)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Session Refresh Timer expired for Term end");
					logger.debug(origLegCallId
							+ ":: Hence notifying the service with connection disconnected event");
				}

				logger.warn(origLegCallId + ":: Terminating session expired");

				callData.set(
						CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.SESSION_EXPIRED_TERM);
				SipSession termSipSession = SipProtocolUtil
						.getSipSessionFromSessionId(
								origLegCallId,
								appSession,
								(String) termLegData
								.get(LegDataAttributes.P_SESSION_ID));
				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_SESSION_EXPIRED,
						Protocol.SIP,
						(String) termSipSession
						.getAttribute(PhConstants.LEG_ID));

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			} else if (timerName.equals(PhConstants.NO_ANSWER_TIMER)) {

				/**
				 * remove this timer attribute name
				 */
				appSession.removeAttribute(timerName);

				Action lastAction = (Action) callData
						.get(CallDataAttribute.P_CURRENT_ACTION);

				if(lastAction.getActionType()==Action.ActionType.ACTION_CONNECT_PARALLEL){
					SipRingParallel.handleTimeout(timer, callData);
					return;
				}

				if(lastAction.getActionType()==Action.ActionType.ACTION_CONNECT_SERIAL){
					SipRingSerial.handleTimeout(timer, callData);
					return;
				}

				State termState = (State) termLegData
						.get(LegDataAttributes.P_LEG_SIP_STATE);

				if (termState != State.TERMINATED) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: No-Answer Timer expired for Term end");
						logger.debug(origLegCallId
								+ ":: Hence notifying the service with no-answer event");
					}
					SipServletRequest termInitInv = SipProtocolUtil
							.getInitialInvite(appSession, termLegData);
					SipSession termSipSession = termInitInv.getSession();
					// Set flag to differentiate this leg
					termLegData.set(
							LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND,
							PhConstants.TRUE);

					javax.servlet.sip.SipSession.State termSessionState = termSipSession
							.getState();
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Term SIP SessionState = "
								+ termSessionState.name());
					}

					if (termSessionState == javax.servlet.sip.SipSession.State.EARLY
							|| termSessionState == javax.servlet.sip.SipSession.State.INITIAL) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Send CANCEL on terminating leg");
						}
						// To fix IllegalStateException exceptions observed in
						// production
						try {
							SipProtocolMessageCreator.createCancelRequest(
									origLegCallId, termInitInv,
									PhConstants.NO_ANS_TIMEOUT_CV_TERM_LEG)
							.send();
						} catch (IllegalStateException ex) {
							if (logger.isInfoEnabled()) {
								logger.info(origLegCallId
										+ ":: Seems 200 OK came so send BYE",
										ex);
							}
							SipProtocolMessageCreator.createByeRequest(
									origLegCallId, termSipSession,
									PhConstants.NO_ANS_TIMEOUT_CV_TERM_LEG,
									false).send();
						}
					} else if (termSessionState == javax.servlet.sip.SipSession.State.CONFIRMED) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "::Send BYE on terminating leg");
						}
						SipProtocolMessageCreator.createByeRequest(
								origLegCallId, termSipSession,
								PhConstants.NO_ANS_TIMEOUT_CV_TERM_LEG, false)
						.send();
					}

					// Update Term Call States
					termLegData
					.set(
							LegDataAttributes.P_LEG_SIP_STATE,
							State.TERMINATED);

					// Unlink terminating session
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Unlink terminating session");
					}
					termInitInv.getB2buaHelper().unlinkSipSessions(
							termInitInv.getSession());
					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							SipProtocolRelReasonCode.NO_ANSWER_TIMEOUT);


					lastAction = (Action) callData
							.get(CallDataAttribute.P_CURRENT_ACTION);

					Event event = null;

					termLegData.set(
							LegDataAttributes.P_CAUSE_CODE,
							PhConstants.CAUSE_CODE_NOANSWER);

					ServiceInterface serviceHandler = PhUtilityServices
							.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

					Action lastLegAction = (Action) termLegData
							.get(LegDataAttributes.P_LEG_CURRENT_ACTION);

					if (lastAction.getActionType() == Action.ActionType.ACTION_TRY_REDIRECT_CONTACTS
							|| (lastLegAction != null && lastLegAction
							.getActionType() == Action.ActionType.ACTION_TRY_REDIRECT_CONTACTS)) {

						event = SipProtocolHelper.tryNextContactFromList(
								origLegCallId, callData, 408, termLegData,
								lastAction, appSession,
								termInitInv.getSession(), false);

						if (event != null) {
							ProtocolRouter.getInstance().execute(event,
									callData, serviceHandler);
							return;
						}

					} else {
						// Set reason as no-answer in
						// event//EVENT_SESSION_EXPIRED

						event = new Event(EventType.EVENT_FAILURE,
								Protocol.SIP,
								(String) termSipSession
								.getAttribute(PhConstants.LEG_ID));

						ProtocolRouter.getInstance().execute(event, callData,
								serviceHandler);
					}
				} else {
					logger.error(origLegCallId
							+ ":: Do nothing as timeout received in invalid call state");
					logger.error(origState + " " + termState);
				}

			} else if (pendingInvTranOrigTimer != null
					&& timerName.equals(pendingInvTranOrigTimer)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Invite pending transaction timer timout happened for orig Leg");
				}

				if (termLegData != null) {
					MultipartBody currentReceivedSdpBLeg = (MultipartBody) termLegData
							.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
					SipServletRequest origLegInitInv = SipProtocolUtil
							.getInitialInvite(appSession, origLegData);
					SipServletRequest origReInv = SipProtocolMessageCreator
							.createReinviteRequest(origLegInitInv,
									currentReceivedSdpBLeg);
					origLegData.set(
							LegDataAttributes.NP_IS_OFFER_SENT,
							PhConstants.TRUE);
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "Send reinvite to orig leg");
					}
					// Send reinvite
					origReInv.send();
					origLegData.set(
							LegDataAttributes.P_INVITE_PENDING_TRANS,
							PhConstants.TRUE);
				} else {
					// Drop the call here with new release reason code
					logger.error(origLegCallId
							+ ":: Drop the call as A_LEG_REINVITE_TIMER expirted and Term Leg initial INVITE is missing");
					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							SipProtocolRelReasonCode.MISSING_TERM_INVITE_REQ);
					origLegData.set(
							LegDataAttributes.P_CAUSE_CODE,
							Integer.parseInt(SipProtocolUtil.getConfig(

									SipProtocolConfig.SIP_RES_CODE_IN_ERROR)));
					SipProtocolHelper.dropCall(appSession);
				}

			} else if (pendingInvTransTermTimer != null && termLegData != null
					&& timerName.equals(pendingInvTransTermTimer)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Invite pending transaction timer timout happened for term Leg");
				}
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: B Leg Re-invite timer for 491 expired");
				}

				MultipartBody currentReceivedSdpALeg = null;
				if (PhConstants.TRUE
						.equals(termLegData
								.get(LegDataAttributes.NP_IS_OFFER_SENT))) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: ReInvite without SDP was send to B. So create and send ReInvite without SDP");
					}
					/*
					 * currentReceivedSdpALeg is already initialized with null.
					 */
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: ReInvite with SDP was send to B. So create and send ReInvite with SDP");
					}
					currentReceivedSdpALeg = (MultipartBody) origLegData
							.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
				}
				SipServletRequest termLegInitInv = SipProtocolUtil
						.getInitialInvite(appSession, termLegData);
				SipServletRequest peerLegReinvite = SipProtocolMessageCreator
						.createReinviteRequest(termLegInitInv,
								currentReceivedSdpALeg);
				// Send reinvite
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "Send reinvite to term leg");
				}
				peerLegReinvite.send();
				termLegData.set(
						LegDataAttributes.P_INVITE_PENDING_TRANS,
						PhConstants.TRUE);

			} else if (timerName
					.equals(PhConstants.CORRELATED_ORIG_CLEANUP_TIMER)) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Pending BYE for orig correlated call cleanup timer timout happened ");
				}

				if (origLegData != null) {

					origLegData
					.set(
							LegDataAttributes.P_CORRELATED_ORIG_LEG_CLEANUP_IND,
							PhConstants.TRUE);

					SipServletRequest origLegInitInv = SipProtocolUtil
							.getInitialInvite(appSession, origLegData);
					SipServletRequest origByeReq = SipProtocolMessageCreator
							.createByeRequest(origLegCallId,
									origLegInitInv.getSession(),
									PhConstants.NO_ANS_TIMEOUT_CV_TERM_LEG,
									false);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "Send ByeRequest to orig leg");
					}
					// Send bye request
					origByeReq.send();
				}

			} else if (relFailureRetryTimer != null
					&& timerName.equals(relFailureRetryTimer)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: 100 rel retry timer happened for orig Leg");
				}


				SipServletResponse provResponse=(SipServletResponse)origLegData.get(
						LegDataAttributes.REL_RETRY_PROV_RESPONSE);

				if (provResponse != null) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Retry to process rel provisional response from term");
					}
					doProvisionalResponse(provResponse);
				}

			} else if (timerName.startsWith(PhConstants.MAX_CALL_DURATION_TIMER)) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Max call duration timer timedout drop the call  ");
				}

				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.MAX_DURATION_REACHED);
				SipProtocolHelper.dropCall(appSession);
				return;

			}else {
				/*
				 * revering back this change for E911 will change in next patch
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Giving call back for the  timer to application  which is unknown to protocol handler");
				}

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_APP_TIMER_TIMEOUT,
						Protocol.SIP,
						CallDataAttribute.P_LEG1.name());
				event.setTimerName(timerName);
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);


				//				logger.warn(origLegCallId
				//						+ ":: Do nothing unexpected timeout received with info "
				//						+ timerName);
				//				return;
			}
		} catch (Exception ex) {
			logger.error(origLegCallId + ":: Failed to process timeout", ex);
			callData.set(
					CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_TIMEOUT_CLBK);
			origLegData.set(LegDataAttributes.P_CAUSE_CODE,
					Integer.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);
		}
	}

	/**
	 * This callback is implemented to increment the appsession timeout by 5
	 * minutes for the connected calls. This would avoid the invalidation of
	 * both appsession (notify, sip request) while call is in connected state.
	 * 
	 * @see javax.servlet.sip.SipApplicationSessionListener#sessionExpired(javax.servlet.sip.SipApplicationSessionEvent)
	 * @param sase
	 *            represents the SipApplicationSessionEvent
	 */
	public void sessionExpired(SipApplicationSessionEvent sase) {
		SipApplicationSession appSession = sase.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);

		if (callData == null) {
			logger.error("Calldata is null so do nothing in sessionExpired callback");
			// TODO: I think we should invalidate appsession here
			return;
		}

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside sessionExpired()");
		}
		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		State origState = (State) origLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);

		LegData termLegData = SipProtocolUtil.getPeerLegData(callData,
				ConnectionType.ORIG_CONNECTION);
		State termState = termLegData == null ? null : (State) termLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);
		
		State origDiameterState = origLegData == null ? null : (State) origLegData
				.get(LegDataAttributes.P_LEG_DIAMETER_STATE);
		

		State termDiameterState = termLegData == null ? null : (State) termLegData
				.get(LegDataAttributes.P_LEG_DIAMETER_STATE);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: OrigSipCallState is " + origState);
			logger.debug(origLegCallId + ":: TermSipCallState is " + termState);
		}

		if ((origState == State.CONNECTED && termState == State.CONNECTED)
				|| origState == State.MS_PLAY
				|| origState == State.MS_PLAY_COLLECT
				|| origState == State.MS_PLAY_RECORD
				|| termState == State.MS_PLAY
				|| termState == State.MS_PLAY_COLLECT
				|| termState == State.MS_PLAY_RECORD
				|| (origDiameterState != null
						&& origDiameterState != State.DIAMETER_REQUEST_COMPLETED || termDiameterState != null
						&& termDiameterState != State.DIAMETER_REQUEST_COMPLETED)) {

			logger.error(origLegCallId
					+ "::  Extend AppSession timeout for next 5 minutes");
		//	logger.error(origLegCallId + " " + origState + " " + termState);
			appSession.setExpires(5);
			return;
		}

		if (callData
				.get(CallDataAttribute.P_CALL_DISCONNECT_TIME) != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Call already dropped so do nothing");
			}
			return;
		}
		
		if (origState==State.PROXY_CONNECT) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Proxy mode return");
			}
			return;
		}

		logger.warn(origLegCallId + ":: Session " + appSession.getId()
		+ " expired so dropping the call");
		callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
				SipProtocolRelReasonCode.APPSESSION_EXPIRED);
		origLegData.set(LegDataAttributes.P_CAUSE_CODE, Integer
				.parseInt(SipProtocolUtil.getConfig(
						SipProtocolConfig.SIP_RES_CODE_IN_ERROR)));
		SipProtocolHelper.dropCall(appSession);
	}

	/**
	 * This callback method is invoked by the server when FT happens. Include
	 * the INAP call state check also because SAS gives sessionDidActivate()
	 * callback on the appsession of ASSIST in case REROUTING gets performed
	 * after ASSIST. For A and B leg start session refresh timer, Notify service
	 * if not done in tcap activate listener that FT happened and call is to be
	 * recovered or not. Set the call data in appsession as in case of ASSIST +
	 * HANDOFF call, if FT happens after DFC + CON and before receiving invite;
	 * sessionDidActivate is called on the appSession of ASSIST and in
	 * dropCall() tcap session gets invalidated.
	 * 
	 * @param appSessionEvent
	 *            represents an instance of SipApplicationSessionEvent
	 * @see javax.servlet.sip.SipApplicationSessionActivationListener#sessionDidActivate(javax.servlet.sip.SipApplicationSessionEvent)
	 */
	public void sessionDidActivate(SipApplicationSessionEvent appSessionEvent) {
		SipApplicationSession appSession = appSessionEvent
				.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		
		if(callData !=null ){
			
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside sessionDidActivate()");
		}
		callData.set(CallDataAttribute.NP_FT_CALL,
				PhConstants.TRUE);
		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		State origState = (State) origLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);
		
		State origDState = (State) origLegData
				.get(LegDataAttributes.P_LEG_DIAMETER_STATE);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Call Sip State is ..."+origState+" Diameter state is "+origDState);
		}
		
		if((origDState==State.DIAMETER_REQUEST_RECEIVED)|| (origDState==State.DIAMETER_RESPONSE_SENT)){
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: its Diameter call returning not taking any action...");
			}
			return;
		}

		LegData termLegData = SipProtocolUtil.getPeerLegData(callData,
				ConnectionType.ORIG_CONNECTION);
		State termState = termLegData == null ? null : (State) termLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);

		if ((origState == State.CONNECTED
				&& termState == State.CONNECTED)
				|| (origState == State.CALL_TRANSFERED && termState == State.CALL_TRANSFERED)) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Call is in CONNECTED/OR in Expected state. Doing nothing ...");
			}
			/*
			 * For A and B leg start session refresh timer
			 */
			SipProtocolUtil.startSessionRefreshTimerAfterFt(appSession,
					callData, origLegData, origLegCallId);
			SipProtocolUtil.startSessionRefreshTimerAfterFt(appSession,
					callData, termLegData, origLegCallId);

			try {
				/*
				 * Notify service if not done in tcap activate listener that FT
				 * happened and call is to be recovered.
				 */

				callData.set(CallDataAttribute.NP_TRANSIENT_CALL, PhConstants.FALSE);
				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_FAILOVER, Protocol.SIP,
						null);
				/*
				 * TODO: Ask DSR... How can we inform service and force service
				 * to return NONE action here
				 */
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return;
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Call is not in CONNECTED state");
			logger.debug(origLegCallId + ":: Drop the call and notify service");
		}
		callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
				SipProtocolRelReasonCode.TRANSIENT_CALL_FT);
		origLegData.set(LegDataAttributes.P_CAUSE_CODE, Integer
				.parseInt(SipProtocolUtil.getConfig(
						SipProtocolConfig.SIP_RES_CODE_AFTER_FT)));
		/*
		 * Notify service if not done in tcap activate listener that FT happened
		 * and call is not required to be recovered.
		 */
		/*
		 * TODO: Ask DSR... How can we inform service and force service to
		 * return NONE action here
		 */

		callData.set(CallDataAttribute.NP_TRANSIENT_CALL, PhConstants.TRUE);
		//			ServiceInterface serviceHandler = PhUtilityServices.getInstance()
		//					.getServiceHandler();
		//			Event event = new Event(EventType.EVENT_FAILOVER, Protocol.SIP,
		//					null);
		//			ProtocolRouter.getInstance().execute(event, callData,
		//					serviceHandler);
		//		} catch (Exception e) {
		//			// TODO Auto-generated catch block
		//			e.printStackTrace();
		//		}
		try {
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();
			Event event = new Event(EventType.EVENT_FAILOVER, Protocol.SIP,
					null);
			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		  SipProtocolHelper.dropCall(appSession);
		}else{
			if (logger.isDebugEnabled()) {
				logger.debug(":: CallData is null.");
			}
		}
		

	}

	/**
	 * This method is used to handle Redirect Response
	 * 
	 * @param sipResponse
	 */
	@SuppressWarnings("unchecked")
	public void doRedirectResponse(SipServletResponse sipResponse) {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		LegData legData = null;
		String resCallId = sipResponse.getCallId();

		try {
			
			appSession = sipResponse.getApplicationSession();
			logger.info("appSession inside doRedirectResponse"+appSession);
			callData = SipProtocolUtil.getCallData(appSession);
			logger.info("callData inside doRedirectResponse"+callData);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			logger.info("origLegCallId inside doRedirectResponse"+origLegCallId);
			logger.info("Inside doRedirectResponse sipSession:: "+sipResponse.getSession());
			if(sipResponse.getSession() != null){
				legData = SipProtocolUtil.getLegDataForSipSession(appSession,
						sipResponse.getSession(), callData);
			}else{
				logger.info("Inside doRedirect response LEG1 is:: "+callData.get(CallDataAttribute.P_LEG1));
				logger.info("Inside doRedirect response LEG2 is:: "+callData.get(CallDataAttribute.P_LEG2));
				//				callData.get(CallDataAttribute.P_LEG1);
				//				callData.get(CallDataAttribute.P_LEG1);
			}
			logger.info("legData inside doRedirectResponse"+legData);

			legData.set(LegDataAttributes.P_CAUSE_CODE, sipResponse.getStatus());
			
			SipProtocolUtil.incrementNetworkTransactions(callData, 1);

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Received Redirection Response with call id "
						+ resCallId + " for " + sipResponse.getMethod());
			}

			// Connection type from where response received
			ConnectionType connectionType = (ConnectionType) legData
					.get(LegDataAttributes.P_CONNECTION_TYPE);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Connection type is "
						+ connectionType);
				logger.debug(origLegCallId
						+ ":: State "
						+ legData
						.get(LegDataAttributes.P_LEG_SIP_STATE));
			}

			ListIterator<Address> contactHeaders = sipResponse
					.getAddressHeaders(PhConstants.CONTACT_HEADER);

			switch (sipResponse.getStatus()) {

			case 302:
				List<TermRedirectionContact> redirectContacts = new ArrayList<TermRedirectionContact>();

				while (contactHeaders.hasNext()) {
					Address contact = contactHeaders.next();

					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ "::  Redirect response received with Contact "
								+ contact);
					}

					SipURI contactUri  = (SipURI) contact.getURI();
					
					String urep = contactUri.getParameter(PhConstants.UREP_PARAM);
					String ucat = contactUri.getParameter(PhConstants.UCAT_PARAM);
				
				
					if (ucat != null) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched ucat from contact uri set in legdata "+ urep);
						}
						legData.set(LegDataAttributes.P_PSX_UCAT, ucat);
					}

					if (urep != null) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched urep from contact uri set in legdata "+ urep);
						}
						legData.set(LegDataAttributes.P_PSX_UREP, urep);
					}
					
					TermRedirectionContact trc = new TermRedirectionContact();
					trc.setRedirectContactUri(contactUri);
					trc.setQValue(contact.getQ());
					
					if (contactUri.getUser() != null) {
						
						trc.setUser(contactUri.getUser());

						String rn = SipProtocolUtil.parseParamFromUserPortion(
								origLegCallId, contactUri.getUser(),
								PhConstants.RN_PARAM);

						trc.setRn(rn);

						String npdi = SipProtocolUtil
								.parseParamFromUserPortion(origLegCallId,
										contactUri.getUser(),
										PhConstants.NPDI_PARAM);
						
						trc.setNpdi(npdi);

						String ssn = SipProtocolUtil.parseParamFromUserPortion(
								origLegCallId, contactUri.getUser(),
								PhConstants.SSN_PARAM);
						
						trc.setSsn(ssn);

						String spid = SipProtocolUtil
								.parseParamFromUserPortion(origLegCallId,
										contactUri.getUser(),
										PhConstants.SPID_PARAM);
						
						trc.setSpid(spid);

						String dpc = SipProtocolUtil.parseParamFromUserPortion(
								origLegCallId, contactUri.getUser(),
								PhConstants.DPC_PARAM);
						
						trc.setDpc(dpc);
					}
					
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: add TermRedirectionContact to RedirectionList " + trc);
					}
					redirectContacts.add(trc);
					
					String contactuser=contactUri.getUser();
					if(contactuser!=null && contactuser.indexOf(";")!=-1){
						
						contactuser = contactuser
								.substring(0,contactuser.indexOf(";"));
					}
					
					// In case of multiple PSX queries, need to remove older value of
					// NP_CONTACT_302_USER. 
					if(callData.get(CallDataAttribute.NP_CONTACT_302_USER) != null){
						callData.remove(CallDataAttribute.NP_CONTACT_302_USER);
						if(logger.isDebugEnabled()){
							logger.debug("NP_CONTACT_302_USER is already set, removing it");
						}
					}
					
					if (callData.get(CallDataAttribute.NP_CONTACT_302_USER) == null
							&& !PhConstants.NPDI_PARAM.equals(contactuser)
							&& !PhConstants.RN_PARAM.equals(contactuser)
							&& !PhConstants.SSN_PARAM.equals(contactuser)
							&& !PhConstants.DPC_PARAM.equals(contactuser)
							&& !PhConstants.SPID_PARAM.equals(contactuser)) {
						
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: set NP_CONTACT_302_USER --> " + contactuser);
						}
						callData.set(CallDataAttribute.NP_CONTACT_302_USER,
								contactuser);
					} else {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: donot set NP_CONTACT_302_USER already set OR not a valid user ");
						}
					}
				}

				Collections.sort(redirectContacts,new TermRedirectionContact());  
				callData.set(CallDataAttribute.NP_REDIRECT_ADDRESS_LIST, redirectContacts);
			
				break;
			case 300:
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Multiple choices received with call id "
							+ resCallId + " for " + sipResponse.getMethod());
				}

				String pSigInfo=sipResponse.getHeader(PhConstants.P_SIG_INFO_HEADER);
				if (pSigInfo != null) {
					callData.set(
							CallDataAttribute.NP_SIG_INFO, pSigInfo);
				}

				List<MultiChoiceContact> multiCContacts = new ArrayList<MultiChoiceContact>();

				while (contactHeaders.hasNext()) {
					Address contact = contactHeaders.next();
					SipURI contactUri = (SipURI)contact.getURI();

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Contact Address  is " + contact);
					}
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Contact URI  is " + contactUri);
					}

					String pAIHdr=null;

					String[] headers=SipProtocolUtil.parsePAIFromMCContact(origLegCallId, contactUri.toString());//contactUri.toString());
					//					String[] psxtag = SipProtocolUtil.parsePSXTagFrmContactHdr(callData, contactUri.toString());
					logger.info("Getting Header:: "+headers);
					if (headers != null && headers.length > 1) {

						contactUri = (SipURI) PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipFactory().createURI(headers[0]);
						pAIHdr = headers[1];
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Final Contact URI is " + contactUri);
						}

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + ":: PAI fetched is "
									+ pAIHdr);
						}
					}

					String tgrp = contactUri.getParameter(PhConstants.TGRP_PARAM);
					String trunkContext = contactUri.getParameter(PhConstants.TRUNK_CONTEXT);
					String destTrunkGroup = contactUri.getParameter(PhConstants.DTG_PARAM);
					String rn = contactUri.getParameter(PhConstants.RN_PARAM);
					String npdi = contactUri.getParameter(PhConstants.NPDI_PARAM);
					String ssn = contactUri.getParameter(PhConstants.SSN_PARAM);
					String spid = contactUri.getParameter(PhConstants.SPID_PARAM);
					String dpc = contactUri.getParameter(PhConstants.DPC_PARAM);
					String urep = contactUri.getParameter(PhConstants.UREP_PARAM);
					String ucat = contactUri.getParameter(PhConstants.UCAT_PARAM);

					MultiChoiceContact mcc = new MultiChoiceContact();
					logger.info("the contact URI user is::"+contactUri.getUser());

					if (contactUri.getUser() != null) {

						//psx tag start
						if (rn == null) {
							rn = SipProtocolUtil.parseParamFromUserPortion(
									origLegCallId, contactUri.getUser(),
									PhConstants.RN_PARAM);
						}

						if (npdi == null) {
							npdi = SipProtocolUtil.parseParamFromUserPortion(
									origLegCallId, contactUri.getUser(),
									PhConstants.NPDI_PARAM);
						}

						if (ssn == null) {
							ssn = SipProtocolUtil.parseParamFromUserPortion(
									origLegCallId, contactUri.getUser(),
									PhConstants.SSN_PARAM);
						}

						if (spid == null) {
							spid = SipProtocolUtil.parseParamFromUserPortion(
									origLegCallId, contactUri.getUser(),
									PhConstants.SPID_PARAM);
						}

						if (dpc == null) {
							dpc = SipProtocolUtil.parseParamFromUserPortion(
									origLegCallId, contactUri.getUser(),
									PhConstants.DPC_PARAM);
						}
						//end psx tag					
						if (tgrp == null
								&& contactUri.getUser().contains(
										PhConstants.TGRP_PARAM)) {
							tgrp = SipProtocolUtil.parseParamFromUserPortion(
									origLegCallId, contactUri.getUser(),
									PhConstants.TGRP_PARAM);
						}

						if (trunkContext == null
								&& contactUri.getUser().contains(
										PhConstants.TRUNK_CONTEXT)) {
							trunkContext = SipProtocolUtil
									.parseParamFromUserPortion(origLegCallId,
											contactUri.getUser(),
											PhConstants.TRUNK_CONTEXT);
						}

						String[] contactUserParts=contactUri.getUser().split(";");
						logger.info("The length of contactUserParts::"+contactUserParts.length);

                    if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Contact uri phone number is "+ contactUserParts[0]);
						}
					}
					mcc.setContactUri(contactUri);
					mcc.setTermPort(contactUri.getPort());
					mcc.setTermIp(contactUri.getHost());


					if(tgrp!=null && trunkContext!=null){

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched tgrp and trunkcontext from contact uri "+ tgrp +" " +trunkContext);
						}
						mcc.setTgrp(tgrp);
						mcc.setTrunkContext(trunkContext);
					}

					if(destTrunkGroup!=null){

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched DTG from contact uri "+ destTrunkGroup);
						}
						mcc.setDtg(destTrunkGroup);
					}

					if(pAIHdr!=null){

						mcc.setPAIHdr(pAIHdr);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched PAI from contact uri set in MCC "+ pAIHdr);
						}
					}
					if(rn != null){
						mcc.setPsx_rn(rn);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched rn from contact uri set in MCC "+ rn);
						}
					}
					if(dpc != null){
						mcc.setPsx_dpc(dpc);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched DPC from contact uri set in MCC "+ dpc);
						}
					}

					if(ssn != null){
						mcc.setPsx_ssn(ssn);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched SSN from contact uri set in MCC "+ ssn);
						}
					}

					if(npdi != null){
						mcc.setNpdi(npdi);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched npdi from contact uri set in MCC "+ npdi);
						}
					}

					if(spid != null){
						mcc.setPsxspid(spid);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched spid from contact uri set in MCC "+ spid);
						}
					}
					
					if(urep != null){
						mcc.setPsxUrep(urep);
						legData.set(LegDataAttributes.P_PSX_UREP, urep);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched urep from contact uri set in MCC "+ urep);
						}
					}
					
					if(ucat != null){
						mcc.setPsxUcat(ucat);
						legData.set(LegDataAttributes.P_PSX_UCAT, ucat);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: fetched ucat from contact uri set in MCC "+ ucat);
						}
					}

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Add multiple contact choice to list");
					}
					multiCContacts.add(mcc);
				}
				callData.set(
						CallDataAttribute.P_MC_CONTACTS_LIST, multiCContacts);
				break;
			default:
				logger.error("Not handling this redirect response ");

			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inform service that redirect reponse has been received on connection "+connectionType);
			}
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();
			Event event = null;

			Action lastAction=(Action) callData.get(CallDataAttribute.P_CURRENT_ACTION);
			Boolean dialoutCheck = (Boolean) callData.get(CallDataAttribute.P_DIALOUT);
			if (logger.isDebugEnabled()){
				logger.debug("Action Type for last action::"+lastAction.getActionType()+" and P_DIALOUT flag::"+dialoutCheck );
			}
			if(lastAction.getActionType() == ActionType.ACTION_PSX_ROUTING
					&& dialoutCheck != null && dialoutCheck.booleanValue() == true){

				logger.info("Setting Protocol as::"+lastAction.getProtocol()+" and event as :: Event Redirect");
				SipProtocolUtil.fetchPSXAttributes(callData,sipResponse.getStatus());
			

				if(lastAction!=null){

					Protocol protocol =lastAction.getProtocol();

					logger.info("Protocol in last action "+protocol);

					switch(protocol){

					case AIN_SCF:{
						if(legData.get(LegDataAttributes.P_LIDB_QUERY_TYPE)==null){
						legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);
						}
						event = new Event(EventType.EVENT_REDIRECT, Protocol.AIN_SCF,
								CallDataAttribute.P_LEG1.name());
					}
					break;
					case ITUINAPCS1_SCF:{
						legData.set(LegDataAttributes.P_LEG_SS7_STATE, InapCallStates.SERVICE_LOGIC);
						event = new Event(EventType.EVENT_REDIRECT, Protocol.ITUINAPCS1_SCF,
								CallDataAttribute.P_LEG1.name());
					}	
					break;
					case ITUINAPCS2_SCF:{
						legData.set(LegDataAttributes.P_LEG_SS7_STATE, InapCallStates.SERVICE_LOGIC);
						event = new Event(EventType.EVENT_REDIRECT, Protocol.ITUINAPCS2_SCF,
								CallDataAttribute.P_LEG1.name());
					}	
					break;
					case CAPV2_SCF:{
						legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.SERVICE_LOGIC);
						event = new Event(EventType.EVENT_REDIRECT, Protocol.CAPV2_SCF,
								CallDataAttribute.P_LEG1.name());
					}	
					break;
					case SIP:{
						if (connectionType != null
								&& connectionType.equals(ConnectionType.ORIG_CONNECTION)) {
							event = new Event(EventType.EVENT_REDIRECT, Protocol.SIP,
									CallDataAttribute.P_LEG1.name());

						} else if (connectionType != null
								&& connectionType.equals(ConnectionType.TERM_CONNECTION)) {
							event = new Event(EventType.EVENT_REDIRECT, Protocol.SIP,
									CallDataAttribute.P_LEG2.name());
						}
					}
					break;
					default:{

						logger.info("no valid protocol in last action");
					}
					}
				}
			}else {

				if (connectionType != null
						&& connectionType.equals(ConnectionType.ORIG_CONNECTION)) {
					event = new Event(EventType.EVENT_REDIRECT, Protocol.SIP,
							CallDataAttribute.P_LEG1.name());

				} else if (connectionType != null
						&& connectionType.equals(ConnectionType.TERM_CONNECTION)) {
					event = new Event(EventType.EVENT_REDIRECT, Protocol.SIP,
							CallDataAttribute.P_LEG2.name());
				}
			}

			if (event != null) {

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			}


			//			if(connectionType != null
			//					&& connectionType.equals(ConnectionType.ORIG_CONNECTION)
			//					&& legData.get(LegDataAttributes.P_LEG_SS7_STATE) == State.PSX_ROUTING ){//{/AinCallStates.PROXY_STATE){
			//				logger.info("Setting Protocol as::AIN_SCF and event as :: Event Redirect");
			//				SipProtocolUtil.fetchPSXAttributes(callData);
			//				legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);
			//				event = new Event(EventType.EVENT_REDIRECT, Protocol.AIN_SCF,
			//						CallDataAttribute.P_LEG1.name());
			//			}
			//			else if (connectionType != null
			//					&& connectionType.equals(ConnectionType.ORIG_CONNECTION)) {
			//				event = new Event(EventType.EVENT_REDIRECT, Protocol.SIP,
			//						CallDataAttribute.P_LEG1.name());
			//
			//			} else if (connectionType != null
			//					&& connectionType.equals(ConnectionType.TERM_CONNECTION)) {
			//				event = new Event(EventType.EVENT_REDIRECT, Protocol.SIP,
			//						CallDataAttribute.P_LEG2.name());
			//			}
			/**
			 * invalidate this sipSession it is not required further
			 */
			sipResponse.getSession().invalidate();


		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Excepton while processing doRedirectResponse()" +e.getMessage());

		}
	}

	/**
	 * This method is used to handle Refer method as per rfc 5589
	 * @param sipRequest
	 * @throws ServletException
	 * @throws IOException
	 */
	public final void doRefer(SipServletRequest sipRequest) throws ServletException,
	IOException {

		try {

			SipApplicationSession appSession = sipRequest.getApplicationSession();
			CallData callData = SipProtocolUtil.getCallData(appSession);
			String origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			String legCallId = sipRequest.getCallId();

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Entering doRefer with callId "+legCallId);
			}

			String targetDialogId=sipRequest.getHeader(PhConstants.TARGET_DIALOG);

			if(targetDialogId!=null){

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: out of dialog refer received not handling currently  ");

					//sipRequest.createResponse(603).send();
					SipProtocolMessageCreator.createResponse(origLegCallId, sipRequest, SipServletResponse.SC_DECLINE, callData).send();
				}

				/**
				 * Refer is received out of dialog need to get sipsessions for this dialog id received and send REFER on that
				 */
			}else{

				String transferKey= SipProtocolUtil.updateDialogMap(sipRequest, origLegCallId, m_ReferDialogMap, dialogManager);

				LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
						sipRequest.getSession(), callData);

				legData.set(LegDataAttributes.NP_TRANSFER_KEY, transferKey);
				// Connection type from where response received
				ConnectionType connectionType = (ConnectionType) legData
						.get(LegDataAttributes.P_CONNECTION_TYPE);

				String legId =(String)sipRequest.getSession().getAttribute(PhConstants.LEG_ID);

				/**
				 * Set legid for this event
				 */
				if (legId == null) {
					if (connectionType == ConnectionType.ORIG_CONNECTION) {
						legId = CallDataAttribute.P_LEG1.name();
					} else {
						legId = CallDataAttribute.P_LEG2.name();
					}
				}
				// Parse the message and fill leg data 

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: setting pending refer for this legid will send response on it   "+legId);
				}

				legData
				.set(LegDataAttributes.P_PENDING_REFER ,sipRequest);

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Notify service that transfer has been initiated by leg   "+legId);
				}

				Event event = new Event(EventType.EVENT_TRANSFER, Protocol.SIP,
						legId);

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			}

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Leaving doRefer");
			}
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("doRefer()" +e.getMessage());

		}
	}

	/**
	 * This method is used to handle Notify method as per rfc 5589
	 * @param sipRequest
	 * @throws ServletException
	 * @throws IOException
	 */
	public final void doNotify(SipServletRequest sipRequest) throws ServletException,
	IOException {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		String origLegCallId = sipRequest.getCallId();

		try {

			CallData callData = SipProtocolUtil.getCallData(appSession);
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Entering doNotify() send 200 ok for this notify with content type "
						+ sipRequest.getContentType() + " and content "
						+ sipRequest.getContent());
			}

			//sipRequest.createResponse(SipServletResponse.SC_OK).send();

			SipProtocolMessageCreator.createResponse(origLegCallId, sipRequest, SipServletResponse.SC_OK, callData).send();


			LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
					sipRequest.getSession(), callData);
			State state=(State)legData.get(LegDataAttributes.P_LEG_SIP_STATE);

			switch (state) {

			case CALL_TRANSFER_IN_PROGRESS: {
				// Connection type from where response received
				ConnectionType connType = (ConnectionType) legData
						.get(LegDataAttributes.P_CONNECTION_TYPE);

				LegData peerLegData = SipProtocolUtil.getPeerLegData(callData,
						connType);

				String content=null;
				if (sipRequest.getContentLength() > 0){
					//							&& sipRequest.getContentType().equals(
					//									PhConstants.NOTIFY_REFER_CONTENT_TYPE)) {

					content = sipRequest.getContent().toString();

					// if (content instanceof String) {
					String notifycontent = (String) content;

					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ "::Notify content recevied is "+notifycontent);
					}

					if (notifycontent.toUpperCase()
							.contains(PhConstants.REFER_SUCESS_CONTENT)) {
						/**
						 * Set the state on this leg as call is transfered so
						 * that when BYE is received from transferor i.e. ATF
						 * agent then we can notify application that call was
						 * transfered
						 */

						if (logger.isInfoEnabled()) {
							logger.info(origLegCallId
									+ "::Notify with 200 OK success is received , set leg state to CALL_TRANSFERED");
						}
						legData.set(
								LegDataAttributes.P_LEG_SIP_STATE,
								State.CALL_TRANSFERED);
						peerLegData.set(
								LegDataAttributes.P_LEG_SIP_STATE,
								State.CALL_TRANSFERED);
						callData.set(
								CallDataAttribute.P_TRANSFERED_CALL_ID,
								origLegCallId);
					}

				}
				SipSession peerLegSipSession = SipProtocolUtil
						.getSipSessionFromSessionId(
								origLegCallId,
								appSession,
								(String) peerLegData
								.get(LegDataAttributes.P_SESSION_ID));

				// To fix "Dialog Terminated" exception observed in production
				if (peerLegSipSession == null
						|| peerLegSipSession.getState() == javax.servlet.sip.SipSession.State.TERMINATED) {
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ ":: Peer SIP session state is terminated, can not send NOTIFY");
					}
					return;
				}
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: send NOTIFY on peer leg");
				}

				SipServletRequest peerNotifyRequest=SipProtocolMessageCreator.createRequest(origLegCallId, peerLegSipSession, PhConstants.NOTIFY_REQUEST, callData);

				peerNotifyRequest.addHeader(PhConstants.EVENT_HEADER, sipRequest.getHeader(PhConstants.EVENT_HEADER));
				peerNotifyRequest.addHeader(PhConstants.SUBSCRIPTION_HEADER,sipRequest.getHeader(PhConstants.SUBSCRIPTION_HEADER));

				if (content != null) {

					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId + ":: set notify content");
					}
					peerNotifyRequest.setContent(sipRequest.getContent(),
							PhConstants.NOTIFY_REFER_CONTENT_TYPE);
				} else {
					/**
					 * currently doing work around for mobile client as it donot send any content and ploycom sends 400 bad request
					 */
					if (sipRequest.getHeader(PhConstants.USER_AGENT_HEADER) != null
							&& sipRequest.getHeader(PhConstants.USER_AGENT_HEADER).contains(
									"antisip")) {
						peerNotifyRequest.setContent(
								PhConstants.REFER_TRYING_CONTENT.getBytes(),
								PhConstants.NOTIFY_REFER_CONTENT_TYPE);
					}
				}
				peerNotifyRequest.send();
			}
			break;

			default:

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Invalid notify received call transfer was not initiated !!!");
				}
				break;
			}

		} catch (Exception e) {
			e.printStackTrace();
			logger.error("doNotify()" +e.getMessage());

		}
	}

	/**
	 * This method is used to handle incoming options messages
	 * @param sipRequest
	 */
	public void doOptions(SipServletRequest sipRequest) {
		if (logger.isInfoEnabled()) {
			logger.info(sipRequest.getCallId()
					+ ":: doOptions called  sending 200 OK!!!");
		}	
		try {
			sipRequest.createResponse(200).send();
		} catch (IOException e) {
			logger.error("doOptions()" +e);
		}
	}

}
