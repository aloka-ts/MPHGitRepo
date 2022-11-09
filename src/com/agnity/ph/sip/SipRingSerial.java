/**
 *
 */
package com.agnity.ph.sip;

import java.io.IOException;
import java.util.List;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.InviteAttributes;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;

/**
 * This Action will send INVITE requests to multiple destination in serial for a
 * given type.
 *
 * @author Reeta Aggarwal
 */
public class SipRingSerial {

	private static final long serialVersionUID = 5114580199832371947L;
	private static Logger logger = Logger.getLogger(SipRingSerial.class);

	/**
	 * This method is used to connect serially
	 * 
	 * @param callData
	 * @param action
	 * @param appSession
	 * @throws Exception
	 */
	public static void connectSerial(CallData callData, Action action,
			SipApplicationSession appSession) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] Inside connectSerial()...");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		@SuppressWarnings("unchecked")
		List<InviteAttributes> destinations = (List<InviteAttributes>) legData
				.get(LegDataAttributes.FORKING_INVITE_ATTRIBUTES_LIST);

		InviteAttributes inviteAttribute = destinations.remove(0);

		legData.set(LegDataAttributes.INVITE_ATTRIBUTES, inviteAttribute);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] Sending request to destination: "
					+ inviteAttribute);
		}
		SipProtocolHelper.sendInitialInviteToTerm(appSession, true, callData,
				action);
		
		Event event = new Event(Event.EventType.EVENT_ROUTE_NEXT,
				Protocol.SIP, action.getLeg());

		ProtocolRouter.getInstance().execute(event, callData,
				PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());

		if (logger.isDebugEnabled()){
			logger.debug("[PH] Exiting connectSerial()...");
		}
	}

	/**
	 * This method is used to handle cancel request
	 * 
	 * @param request
	 * @param callData
	 */
	static void handleCancelRequest(SipServletRequest request, CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] handleCancelRequest() entered.");
		}
		try {
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
			SipServletRequest termInvite = SipProtocolUtil.getInitialInvite(
					request.getApplicationSession(), legData);
			if (termInvite != null) {
				termInvite.createCancel().send();
			}
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();

			Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.SIP,
					CallDataAttribute.P_LEG1.name());

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);

		} catch (Exception e) {
			logger.error("[PH] An Exception occured during creating cancel" + e);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] handleCancelRequest() exited.");
		}
	}

	/**
	 * This method is used to handle Error response from a destination
	 * 
	 * @param response
	 * @param callData
	 */
	protected static void handleErrorResponse(SipServletResponse response,
			CallData callData) {

		int responseCode = response.getStatus();

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] Inside handleErrorResponse() for method:"
					+ response.getMethod() + " response code" + responseCode);
		}

		SipServletRequest peerRequest = SipProtocolUtil.getOrigInitialInvite(
				response.getApplicationSession(), callData);
		try {
			Action action = (Action) callData
					.get(CallDataAttribute.P_CURRENT_ACTION);

			LegData legData = (LegData) callData.get(CallDataAttribute
					.valueOf(action.getLeg()));

			@SuppressWarnings("unchecked")
			List<InviteAttributes> destinations = (List<InviteAttributes>) legData
					.get(LegDataAttributes.FORKING_INVITE_ATTRIBUTES_LIST);

			 if (destinations != null && !destinations.isEmpty()) {

				tryNextDestination(peerRequest, response.getStatus(), callData,
						response.getApplicationSession());

			} else {
				// Default handling

				if (logger.isDebugEnabled()) {
					logger.debug("[PH] list is empty proceed send failure event to application");
				}
				SipServletResponse peerResponse = peerRequest
						.createResponse(response.getStatus());
				peerResponse.send();
				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

				Event event = new Event(EventType.EVENT_FAILURE, Protocol.SIP,
						CallDataAttribute.P_LEG2.name());

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			}
		} catch (IOException e) {
			logger.error("[PH] An IOException occured in handleErrorResponse(): "
					+ e);
		} catch (Exception e) {
			logger.error("[PH] An Exception occured in handleErrorResponse(): "
					+ e);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] Exitting handleErrorResponse()");
		}
	}

	/**
	 * This method is used to try nest available destination
	 * 
	 * @param request
	 * @param responseCode
	 * @param callData
	 * @param appSession
	 * @throws Exception
	 */
	private static void tryNextDestination(SipServletRequest request,
			int responseCode, CallData callData,
			SipApplicationSession appSession) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] tryNextDestination() entered");
		}

		Action action = (Action) callData
				.get(CallDataAttribute.P_CURRENT_ACTION);

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		@SuppressWarnings("unchecked")
		List<InviteAttributes> destinations = (List<InviteAttributes>) legData
				.get(LegDataAttributes.FORKING_INVITE_ATTRIBUTES_LIST);

		InviteAttributes nextDestinationDAO = !destinations.isEmpty() ? destinations
				.remove(0) : null;

		if (nextDestinationDAO != null) {

			request.createResponse(SipServletResponse.SC_CALL_BEING_FORWARDED)
					.send();
			legData.set(LegDataAttributes.INVITE_ATTRIBUTES, nextDestinationDAO);

			SipProtocolUtil.stopTimer(appSession, PhConstants.NO_ANSWER_TIMER);

			SipProtocolHelper.sendInitialInviteToTerm(appSession, true,
					callData, action);
			
			Event event = new Event(Event.EventType.EVENT_ROUTE_NEXT,
					Protocol.SIP, action.getLeg());

			ProtocolRouter.getInstance().execute(event, callData,
					PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());

		} else {

			if (logger.isDebugEnabled()) {
				logger.debug("[PH] no more destination found notify application()");
			}

			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();

			Event event = new Event(EventType.EVENT_FAILURE, Protocol.SIP,
					CallDataAttribute.P_LEG2.name());

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] tryNextDestination() exited");
		}

	}

	/**
	 * This method is used to handle timeout
	 * 
	 * @param timer
	 * @param callData
	 */
	public static void handleTimeout(ServletTimer timer, CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] handleTimeout() entered");
		}
		SipServletRequest origInvite = SipProtocolUtil.getOrigInitialInvite(
				timer.getApplicationSession(), callData);

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		SipServletRequest termInvite = SipProtocolUtil.getInitialInvite(
				timer.getApplicationSession(), legData);
		try {
			if (termInvite != null) {
				termInvite.createCancel().send();
			}
			tryNextDestination(origInvite, 408, callData,
					timer.getApplicationSession());
		} catch (Exception e) {
			logger.error("[PH] An Exception occured in handleTimeout()" + e);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] handleTimeout() exited");
		}
	}

}
