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
package com.agnity.ph.http;

import java.io.BufferedReader;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;

import org.apache.log4j.Logger;
import org.json.JSONObject;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.State;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolHandler;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.SipProtocolConfig;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.common.Registry;
import com.baypackets.ase.common.logging.LoggingHandler;
import com.baypackets.ase.ra.http.event.HttpResourceEvent;
import com.baypackets.ase.ra.http.message.HttpRequest;
import com.baypackets.ase.ra.http.message.HttpResponse;
import com.baypackets.ase.resource.Message;
import com.baypackets.ase.sipconnector.AseDialogManager;
import com.baypackets.ase.sipconnector.AseMessageLoggingInterface;
import com.baypackets.ase.util.Constants;

/**
 * This class is used to handle HTTP events. this class is called by
 * ProtocolHandlerHttpServlet to handle http events
 */
public class HttpProtocolHandler implements ProtocolHandler {

	private static final HttpProtocolHandler INSTANCE = new HttpProtocolHandler();

	static LoggingHandler m_loggingHandler = (LoggingHandler) Registry
			.lookup(Constants.NAME_LOGGING_HANDLER);

	public static final String GET_STATS = "getStatistics".intern();
	/** Logger element */
	private static Logger logger = Logger.getLogger(HttpProtocolHandler.class
			.getName());

	private AseDialogManager dialogManager;

	private HttpProtocolHandler() {
		dialogManager = (AseDialogManager) Registry
				.lookup(Constants.DIALOG_MGR);
	}

	public static HttpProtocolHandler getInstance() {
		return INSTANCE;
	}

	/**
	 * This method is used to execute as action returned by service. this method
	 * is called by Protocol Router
	 */
	public void executeAction(CallData callData, Action action)
			throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		SipApplicationSession appSession = SipProtocolUtil.getAppSession(
				(String) callData.get(CallDataAttribute.P_APP_SESSION_ID),
				(String) callData.get(CallDataAttribute.SERVICE_ID));
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside executeAction HttpProtocolHandler");
		}

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

		case ACTION_SEND_HTTP_RES: {

			HttpProtocolHelper.sendHTTPResponse(appSession,callData, action);

			break;
		}
		case ACTION_SEND_HTTP_ERROR: {

			int httpError = HttpServletResponse.SC_NOT_FOUND;

			String errorCode = (String) callData
					.get(CallDataAttribute.NP_HTTP_RESP_CODE);

			if (errorCode != null) {
				httpError = Integer.parseInt(errorCode);
			}

			String httpErrorMsg = "Not Found";
			String errorMsg = (String) callData
					.get(CallDataAttribute.NP_HTTP_RESP_MSG);

			if (errorMsg != null) {
				httpErrorMsg = errorMsg;
			}
			// sendHttpError(callData,
			// (HttpServletResponse) callData
			// .get(CallDataAttribute.NP_HTTP_RES),
			// httpError, httpErrorMsg, origLegCallId);

			sendHttpError(callData, action.getHttpResSendEvent(), httpError,
					httpErrorMsg, origLegCallId);

			break;
		}
		case ACTION_SEND_HTTP_REQ: {

			sendHttpRequest(appSession, callData, action);
		}
			break;

		case ACTION_START_TIMER: {
			HttpProtocolHelper.startApplicationTimer(appSession, callData,
					action);
			break;
		}
		case ACTION_STOP_TIMER: {
			HttpProtocolHelper.stopApplicationTimer(appSession, callData,
					action);
			break;
		}

		default: {
			logger.error(origLegCallId + ":: Incorrect action type received "
					+ action.getActionType());
			break;
		}
		}

		String traceFlag = (String) callData
				.get(CallDataAttribute.P_TRACE_FLAG);
		if (PhConstants.TRUE.equals(traceFlag)) {
			StringBuilder traceMsg = (StringBuilder) callData
					.get(CallDataAttribute.P_TRACE_MESSAGE);

			if (traceMsg.length() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Trace message is "
							+ traceMsg);
				}
				HttpServletRequest httpRequest = (HttpServletRequest) callData
						.get(CallDataAttribute.NP_HTTP_REQ);
			}
		}

	}

	/**
	 * This method is used to handle HTTP POST request
	 * 
	 * @param request
	 * @param response
	 * @throws ServletException
	 * @throws IOException
	 */
	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		if (logger.isInfoEnabled()) {
			logger.info("Entering doPost of HttpProtocolHandler");
		}

		CallData calldata = new CallData();
		calldata.set(CallDataAttribute.NP_HTTP_RES, response);
		calldata.set(CallDataAttribute.NP_HTTP_REQ, request);

		if (request.getParameter(GET_STATS) != null) {
			String serviceName = request
					.getParameter(HttpProtocolHandler.GET_STATS);
			HttpProtocolHelper.fetchAnDisplayStats(calldata, serviceName);
			return;
		}

		Enumeration<String> paramE = request.getParameterNames();

		while (paramE.hasMoreElements()) {

			String nextElem = paramE.nextElement();
			String value = request.getParameter(nextElem);

			if (logger.isInfoEnabled()) {
				logger.info("Save value of HTTP Param " + nextElem + " as "
						+ value + " In call Data");
				// FIXME: Commented by Ankit for the timebeing!
				// calldata.setNonpersistableData(nextElem, value);
			}
		}

		Event httpevent = new Event(EventType.EVENT_HTTP_REQ_RECEIVED,
				Protocol.HTTP, null);
		calldata.set(CallDataAttribute.P_PROTOCOL, Protocol.HTTP);
		try {
			ProtocolRouter.getInstance().execute(
					httpevent,
					calldata,
					PhUtilityServices
							.getInstance(
									(String) calldata
											.get(CallDataAttribute.SERVICE_ID))
							.getServiceHandler());
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

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
	@Override
	public final void timeout(ServletTimer timer) {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";
		// LegData origLegData = null;
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

			if (timerName.startsWith(PhConstants.HTTP_OPERATION_RESP_TIMER)) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Access gatway timer timedout  ");
				}

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();
				Event event = new Event(
						EventType.EVENT_HTTP_RESP_TIMER_TIMEOUT, Protocol.HTTP,
						null);
				event.setTimerName(timerName);
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				return;

			} else if (timerName.startsWith(PhConstants.APP_TIMER)) {

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
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();
				Event event = new Event(EventType.EVENT_APP_TIMER_TIMEOUT,
						Protocol.HTTP, null);
				event.setTimerName(timerName);

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				return;

			} else if (timerName.startsWith(PhConstants.HTTP_RESP_SENT_TIMER)) {

				HttpEvent httpEvent=(HttpEvent)timerInfo.getData();
				
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Http Resp is sent raise Event ");
					logger.debug(origLegCallId
							+ ":: Hence notifying the service for http res sent "+ httpEvent.getLeg());
				}
				
				Event event = new Event(EventType.EVENT_HTTP_RESP_SENT,
						Protocol.HTTP, httpEvent.getLeg());
				event.setHttpEvent(httpEvent);

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				return;

			} else {
				/*
				 * revering back this change for E911 will change in next patch
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Giving call back for the  timer to application  which is unknown to protocol handler");
				}

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();
				Event event = new Event(EventType.EVENT_APP_TIMER_TIMEOUT,
						Protocol.HTTP, CallDataAttribute.P_LEG1.name());
				event.setTimerName(timerName);
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);

			}
		} catch (Exception ex) {
			logger.error(origLegCallId + ":: Failed to process timeout", ex);
		}
	}

	/**
	 * This method is used to process HTTP response received through http RA
	 * 
	 * @param callData
	 * @param message
	 */
	public void processResponse(CallData callData, Message message) {

		SipApplicationSession appSession = null;
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (message instanceof HttpResponse) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: processResponse for http");
			}
			String httpheaderfield = "";
			HttpResponse httpResponse = (HttpResponse) message;

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: HTTP Response code is "
						+ httpResponse.getResponseCode());
			}

			HttpEvent httpEvent = new HttpEvent();
			appSession = httpResponse.getApplicationSession();

			httpEvent.setRespContent(httpResponse.getData());
			httpEvent.setRespCode(httpResponse.getResponseCode());
			httpEvent.setHeadersMap(httpResponse.getHeaderFields());
			httpEvent.setRespTimeStamp(new Date());

			// Stop the timer
			SipProtocolUtil.stopTimer(appSession,
					PhConstants.HTTP_OPERATION_RESP_TIMER);

			// callData.set(CallDataAttribute.NP_HTTP_RES_CONTENT,
			// httpResponse.getData());
			// callData.set(CallDataAttribute.NP_HTTP_RES_HDRS,
			// httpResponse.getHeaderFields());
			//
			// callData.set(CallDataAttribute.NP_HTTP_RESP_CODE,
			// httpResponse.getResponseCode());

			Map<String, List<String>> headerFields = httpResponse
					.getHeaderFields();

			// callData.set(CallDataAttribute.NP_HTTP_RES_HDRS, headerFields);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Received HTTP response header is " + headerFields);
				logger.debug(origLegCallId + ":: Received HTTP response is "
						+ httpResponse.getData());
				logger.debug(origLegCallId
						+ ":: Notify service that HTTP operation successfull");
			}

			// callData.set(CallDataAttribute.P_HTTP_RES_RECIEVED_TIME, new
			// Date());

			// callData.set(CallDataAttribute.NP_INCOMING_HTTP_RES,
			// httpResponse.get());

			Event httpevent = new Event(EventType.EVENT_HTTP_RESP_RECEIVED,
					Protocol.HTTP, null);
			httpevent.setHttpEvent(httpEvent);
			try {
				ProtocolRouter.getInstance().execute(
						httpevent,
						callData,
						PhUtilityServices.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
								.getServiceHandler());
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}

	}

	/**
	 * String urlRemote =
	 * "http://"+fipRemote+"/2/httpsbb?aai=2&numType=destType"; String urlLocal
	 * = "http://"+fipLocal+"/2/httpsbb?aai=2&numType=destType"; This method
	 * sends the HTTP request to outside world for the isup call. In case of an
	 * exception in sending http request service is notified with operation
	 * failed event and reason for release as EXCEP_SEND_HTTP_REQ.
	 * 
	 * @param appSession
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	private void sendHttpRequest(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = "0";

		String svcId = (String) callData.get(CallDataAttribute.SERVICE_ID);

		try {

			// callData = SipProtocolUtil.getCallData(appSession);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inside sendHttpRequest with appSession");
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Creating HTTP GET request for for action "
						+ action);
			}

			if (action.isNewHttpSession()) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Creating New App session for new request");
				}
				appSession = PhUtilityServices.getInstance(svcId)
						.getSipFactory().createApplicationSession();
				appSession.setAttribute(CallData.CALL_DATA, callData);
			}

			HttpEvent httpEvent = action.getHttpReqSendEvent();

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Creating Http Request from httpEvent "
						+ httpEvent);
			}

			HttpRequest httpRequest = PhUtilityServices
					.getInstance(svcId)
					.getHttpResFactory()
					.createRequest(appSession, httpEvent.getReqURI(),
							httpEvent.getEventId());
			

			if (httpEvent.getContent() != null) {

				String data = httpEvent.getContent();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Do UTF-8 encoding of content " + data);
				}

				byte[] postdata = data.getBytes("UTF-8");

				httpRequest.setData(postdata);
			}

			if (httpEvent.getContentType() != null) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Set contentType as "
							+ httpEvent.getContentType());
				}

				httpRequest.setContentType(httpEvent.getContentType());
			}

			if (httpEvent.getHeadersMap() != null) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: set Http headers "
							+ httpEvent.getHeadersMap());
				}

				@SuppressWarnings("unchecked")
				LinkedHashMap<String, List<String>> httpHeaders = (LinkedHashMap<String, List<String>>) httpEvent
						.getHeadersMap();

				Set<Entry<String, List<String>>> enteries = httpHeaders
						.entrySet();
				if (enteries != null) {

					Iterator<Entry<String, List<String>>> iterator = enteries
							.iterator();

					while (iterator.hasNext()) {

						Entry<String, List<String>> entry = iterator.next();
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "Add custom header  Name: "
									+ entry.getKey() + " Value :"
									+ entry.getValue());
						}
						for (String e : entry.getValue()) {
							httpRequest.setHeader(entry.getKey(), e);
						}
					}
				}
			}

			@SuppressWarnings("unchecked")
			Map<String, String> httpParams = (Map<String, String>) httpEvent
					.getParameterMap();

			if (httpParams != null && !httpParams.isEmpty()) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Set http params");
				}
				httpRequest.setParams(httpParams);
			} else {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: No http params to set");
				}
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Sending HTTP request to requested URL");
			}

			httpRequest.send();

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Starting Http Response Timer if enabled");
			}

			if (SipProtocolUtil
					.getConfig(SipProtocolConfig.HTTP_OP_RESPONSE_TIME) != null) {
				long sAgResponseTimerValue = Long.parseLong(SipProtocolUtil
						.getConfig(SipProtocolConfig.HTTP_OP_RESPONSE_TIME));

				// This timer should be non-persistable to avoid data
				// replication
				SipProtocolUtil.startTimer(appSession, sAgResponseTimerValue,
						false, PhConstants.HTTP_OPERATION_RESP_TIMER);
			}

			httpEvent.setReqTimeStamp(new Date());
			callData.set(CallDataAttribute.P_HTTP_REQ_SENT_TIME, new Date());

			// callData.set(CallDataAttribute.NP_OUTGOING_HTTP_REQ,httpRequest.get());
			//
			// Event httpevent = new Event(EventType.EVENT_HTTP_REQ_SENT,
			// Protocol.HTTP, null);
			// try {
			// ProtocolRouter.getInstance().execute(
			// httpevent,
			// callData,
			// PhUtilityServices
			// .getInstance(
			// (String) callData
			// .get(CallDataAttribute.SERVICE_ID))
			// .getServiceHandler());
			// } catch (Exception e) {
			// logger.error(origLegCallId
			// + ":: Exception in raising event EVENT_HTTP_REQ_SENT to App. "
			// + e.getMessage());
			// }

		} catch (IOException ex) {
			logger.error(origLegCallId
					+ ":: Exception in sending HTTP request. "
					+ ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Error sending HTTP request", ex);
				logger.info(origLegCallId
						+ ":: Notify service that HTTP GET operation failed");
			}

			Event httpevent = new Event(EventType.EVENT_HTTP_REQ_FAILED,
					Protocol.HTTP, null);
			try {
				ProtocolRouter.getInstance().execute(
						httpevent,
						callData,
						PhUtilityServices.getInstance(svcId)
								.getServiceHandler());
			} catch (Exception e) {
				logger.error(origLegCallId
						+ ":: Exception in raising EVENT_HTTP_REQ_FAILED. "
						+ ex.getMessage());
			}
		}
	}

	/**
	 * This method is for handling http get request scoming to the server. Used
	 * for ASR and VXML processing. In case of an exception send 404
	 * SC_NOT_FOUND response.
	 * 
	 * @param httpEvent
	 *            represents an instance of httpEvent
	 * @param callData2
	 * @throws Exception
	 */
	public void handleHttpGet(HttpEvent httpEvent, CallData callData,SipApplicationSession appSession)
			throws Exception {

		HttpServletResponse httpResponse = httpEvent.getHttpResponse();

		try {

			if (logger.isDebugEnabled()) {
				logger.debug(" Inside handleHttpGet method");
			}

			handleHttpPost(httpEvent, callData,appSession);
		} catch (Exception e) {
			logger.error("Exception in handling HTTP GET. Error is "
					+ e.getMessage());
//			sendHttpError(callData, httpEvent,
//					HttpServletResponse.SC_NOT_FOUND, e.getMessage(), "0");
		}
	}

	/**
	 * This method is used to send HTTP Error
	 * 
	 * @param callData
	 * 
	 * @param httpResponse
	 * @param msgCode
	 * @param message
	 * @param callId
	 */
	private void sendHttpError(CallData callData,

	HttpEvent httpEvent, int msgCode, String message, String callId) {
		try {

			String svcId = (String) callData.get(CallDataAttribute.SERVICE_ID);
			if (logger.isDebugEnabled()) {
				logger.debug(" Inside sendHttpError method");
			}

			httpEvent.getHttpResponse().sendError(msgCode, message);

			 HttpServletRequest httpReq=(HttpServletRequest)
			 callData.get(CallDataAttribute.NP_HTTP_REQ);

			if (m_loggingHandler.isHttpLoggingEnabled()) {

				String errOut = HttpProtocolHandler.m_loggingHandler
						.getHttpLoggingInterface().logResponse(0,
								httpEvent.getHttpResponse(), message,
								httpReq.getRemoteHost(),
								"" + httpReq.getRemotePort());
				callData.set(CallDataAttribute.NP_OUTGOING_HTTP_RES, errOut);
			}
			// else {
			//
			// String errOut = HttpProtocolHandler.m_loggingHandler
			// .getHttpLoggingInterface()
			// .outgoingHttpResponseToString(httpResponse, message);
			// callData.set(CallDataAttribute.NP_OUTGOING_HTTP_RES, errOut);
			// }

			httpEvent.setRespTimeStamp(new Date());
			// callData.set(CallDataAttribute.P_HTTP_RES_SENT_TIME, new Date());

			Event httpevent = new Event(EventType.EVENT_HTTP_ERR_RES_SENT,
					Protocol.HTTP, null);

			httpevent.setHttpEvent(httpEvent);
			try {
				ProtocolRouter.getInstance().execute(
						httpevent,
						callData,
						PhUtilityServices.getInstance(svcId)
								.getServiceHandler());
			} catch (Exception e) {
				logger.error(" Exception in raising EVENT_HTTP_ERR_RES_SENT. "
						+ e.getMessage());
			}

		} catch (Exception e1) {
			logger.warn(callId
					+ ":: Exception in sending http error. Error is "
					+ e1.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(callId + ":: Error in sending http response.", e1);
			}
		}
	}

	/**
	 * This method is for handling http post request soming to the server. In
	 * case of HTTP POST if httpResponse.status is not set in that case, HTTP RA
	 * itself sends 200 OK response for that HTTP request.In case of an
	 * exception send 404 SC_NOT_FOUND response.
	 * 
	 * @param httpEvent
	 *            represents an instance of httpEvent
	 * @throws Exception
	 */
	public void handleHttpPost(HttpEvent httpEvent, CallData callData,SipApplicationSession appSession)
			throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleHttpPost method");
		}

		HttpServletRequest httpRequest = httpEvent.getHttpRequest();
		HttpServletResponse httpResponse = httpEvent.getHttpResponse();

		if (httpRequest.getParameter(GET_STATS) != null) {
			String serviceName = httpRequest
					.getParameter(HttpProtocolHandler.GET_STATS);
			HttpProtocolHelper.fetchAnDisplayStats(callData, serviceName);
			return;
		}

		String content=httpEvent.getContent();

		 String incomingReq=null;

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: if Http logging is enabled.. log request..."+ m_loggingHandler);
		}
		try {
			if (m_loggingHandler.isHttpLoggingEnabled()) {
				incomingReq = m_loggingHandler.getHttpLoggingInterface()
						.logRequest(AseMessageLoggingInterface.DIRECTION_IN,
								httpRequest, content);
				callData.set(CallDataAttribute.NP_INCOMING_HTTP_REQ,
						incomingReq);
			}
		} catch (Exception e) {
			logger.error(origLegCallId
					+ ":: if Http logging is enabled.. could not log request..."
					+ e);

		}
		// else{
		//
		// incomingReq
		// =m_loggingHandler.getHttpLoggingInterface().incominghttpRequestToString(httpRequest,
		// content);
		// callData.set(CallDataAttribute.NP_INCOMING_HTTP_REQ,incomingReq);
		// }
		LegData leg1Data=null;
		try {

			// callData.set(CallDataAttribute.NP_HTTP_RES, httpResponse);
			// callData.set(CallDataAttribute.NP_HTTP_REQ_CONTENT,
			// content);
			// callData.set(CallDataAttribute.NP_HTTP_REQ_METHOD,
			// httpRequest.getMethod());
			// callData.set(CallDataAttribute.NP_HTTP_REQ_URL,
			// httpRequest.getRequestURI());
			// callData.set(CallDataAttribute.NP_HTTP_REQ,
			// httpRequest);

			Enumeration<String> hdrNames = httpRequest.getHeaderNames();

			HashMap<String, List<String>> httpHeadersMap = new HashMap<String, List<String>>();

			if (hdrNames != null) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Read header names and values");
				}

				while (hdrNames.hasMoreElements()) {

					String hdrName = hdrNames.nextElement();

					Enumeration<String> httpHdrs = httpRequest
							.getHeaders(hdrName);

					List<String> hdrsValues = new ArrayList<String>();

					while (httpHdrs.hasMoreElements()) {

						String httphdrVal = httpHdrs.nextElement();
						hdrsValues.add(httphdrVal);
					}

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Header name: "
								+ hdrName + " Header Value: " + hdrsValues);
					}
					httpHeadersMap.put(hdrName, hdrsValues);
				}

				httpEvent.setHeadersMap(httpHeadersMap);
			}
			
			leg1Data=(LegData) callData.get(CallDataAttribute.P_LEG1);
			
			if(leg1Data==null){
				leg1Data= new LegData();		
				callData.set(CallDataAttribute.P_LEG1,leg1Data);
				leg1Data.set(LegDataAttributes.P_LEG_SIP_STATE,State.INIT);
			}
			fetchContent(httpEvent, leg1Data,callData);
			
			 SimpleDateFormat sdfDate = new SimpleDateFormat("yyyyMMddHHmmssSSS");//dd/MM/yyyy
			    Date now = new Date();
			    String strDate = sdfDate.format(now);
			    
			// logger.error("HTTP_REQ:" +appSession.getId()+","+ httpRequest.getSession().getId()+","+strDate);
			callData.set(CallDataAttribute.P_CALL_START_TIME, new Date());
			callData.set(CallDataAttribute.P_LEG1,leg1Data);
			callData.set(CallDataAttribute.NP_HTTP_REQ_CONTENT, httpEvent.getContent());
			callData.set(CallDataAttribute.NP_HTTP_REQ_CONTENT_TYPE, httpEvent.getContentType());

			// callData.set(CallDataAttribute.NP_HTTP_REQ_HDRS, httpHeadersMap);
			//
			// Map<String, String> httpParams = new HashMap<String, String>();
			//
			// Enumeration<String> paramNames = httpRequest.getParameterNames();
			//
			// while (paramNames.hasMoreElements()) {
			// String paramName = paramNames.nextElement();
			// String paramValue = httpRequest
			// .getParameter(paramName);
			// // List<String> list = Arrays.asList(paramValues);
			// httpParams.put(paramName, paramValue);
			// }
			//
			// callData.set(CallDataAttribute.NP_HTTP_REQ_PARAMS, httpParams);

			Event httpReqevent = new Event(EventType.EVENT_HTTP_REQ_RECEIVED,
					Protocol.HTTP, null);
			
			callData.set(CallDataAttribute.P_PROTOCOL, Protocol.HTTP);

			httpReqevent.setHttpEvent(httpEvent);
			ProtocolRouter.getInstance().execute(
					httpReqevent,
					callData,
					PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getServiceHandler());
		} catch (Exception e) {
			logger.error(origLegCallId
					+ ":: Exception in handling HTTP GET. Error is " + e);

//			if (!httpResponse.isCommitted()) {
//				sendHttpError(callData, httpEvent,
//						HttpServletResponse.SC_NOT_FOUND, e.getMessage(), "0");
//			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ "get response information");
		}
		int sAgResponseTimerValue = -1;

		if (SipProtocolUtil.getConfig(SipProtocolConfig.HTTP_OP_RESPONSE_TIME) != null) {
			sAgResponseTimerValue = Integer.parseInt(SipProtocolUtil
					.getConfig(SipProtocolConfig.HTTP_OP_RESPONSE_TIME));
			if (sAgResponseTimerValue / 1000 <1) {
				sAgResponseTimerValue = sAgResponseTimerValue *1000;
			}
		}
		if (sAgResponseTimerValue < 0) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "send response now donot wait ");
			}
			sendHttpResponse(origLegCallId, leg1Data, appSession, callData,
					httpRequest, httpResponse);
		} else {
			
			waitForSomeTimeForSendingResponse(sAgResponseTimerValue, httpResponse,leg1Data);
			
			sendHttpResponse(origLegCallId, leg1Data, appSession, callData,
					httpRequest, httpResponse);
			Date conn=(Date) callData.get(CallDataAttribute.P_CALL_START_TIME);
			Date dis=(Date)callData.get(CallDataAttribute.P_CALL_DISCONNECT_TIME);
			
			long time=dis.getTime()-conn.getTime();
//			logger.error("HTTP_RES:" + appSession.getId() +  "," + httpRequest.getSession().getId()	+ "," + leg1Data.get(LegDataAttributes.MAP_ATI_MSISDN) + ","
//					+ time);

		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "handleHttpPost-->Leaving");
		}
	}
	
	
	private void sendHttpResponse(String origLegCallId, LegData leg1Data,
			SipApplicationSession appSession, CallData callData,
			HttpServletRequest httpRequest, HttpServletResponse httpResponse) throws IOException {
		String rsContent = (String) leg1Data
				.get(LegDataAttributes.NP_HTTP_RESPONSE_CONTENT);

		String resContentType = (String) leg1Data
				.get(LegDataAttributes.NP_HTTP_RESPONSE_CONTENT_TYPE);

		String respCode = (String) leg1Data
				.get(LegDataAttributes.NP_HTTP_RESPONSE_CODE);

		int resCInt = 200;
		if (rsContent != null || resContentType != null || respCode != null) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "send http response  for content sent");
			}
			Action sendHttpRes = new Action(ActionType.ACTION_SEND_HTTP_RES);
			HttpEvent event = new HttpEvent();

			if (respCode != null) {
				resCInt = Integer.parseInt(respCode);
			}
			event.setRespCode(resCInt);
			event.setRespContentType(resContentType);
			event.setRespContent(rsContent);
			event.setHttpRequest(httpRequest);
			event.setHttpResponse(httpResponse);
			sendHttpRes.setHttpResSendEvent(event);

			HttpProtocolHelper.sendHTTPResponse(appSession, callData,
					sendHttpRes);

		}
	}
	
	
	public void waitForSomeTimeForSendingResponse(int TIMEOUT,HttpServletResponse httpResponse,LegData leg1Data) throws Exception {
		   // String result = null;
		if (logger.isDebugEnabled()) {
			logger.debug("waitForSomeTimeForSendingResponse--> for "+TIMEOUT);
		}
		
		TIMEOUT=(TIMEOUT/1000)*2;
		
		if (logger.isDebugEnabled()) {
			logger.debug("waitForSomeTimeForSendingResponse--> retries "+TIMEOUT);
		}
		    int i = 0;
		    while (i < TIMEOUT) {
		       
		        if ( leg1Data
						.get(LegDataAttributes.NP_HTTP_RESPONSE_CONTENT)!=null) {
		        	if (logger.isDebugEnabled()) {
		    			logger.debug("break content is set after timeout (ms) "+i*500);
		    		}
		            break;
		        } else {
		            TimeUnit.MILLISECONDS.sleep(500);
		            ++i;
		            if (i == TIMEOUT) {
		            	if (logger.isDebugEnabled()) {
		    				logger.debug( "handleHttpPost-->Leaving timedout ");
		    			}
		            	//httpResponse.setStatus(200);
		               // throw new TimeoutException("Timed out after waiting for " + i + " seconds");
		            }
		        }
		    }
		   // return result;
		}
	
	void fetchContent(HttpEvent event,LegData leg1,CallData callData) {
		
		if (logger.isDebugEnabled()) {
			logger.debug("fetchContent----->");
		}
		if (event.getContentLength() != -1) {

			if (logger.isDebugEnabled()) {
				logger.debug("fetchContent----->" +event.getContent());
			}
			
			callData.set(CallDataAttribute.NP_HTTP_REQ_CONTENT, event.getContent());
			callData.set(CallDataAttribute.NP_HTTP_REQ_CONTENT_TYPE, event.getContentType());
			
			if (event.getContentType().equals(
					PhConstants.HTTP_CONTENT_TYPE_JSON)
					|| event.getContentType().equals(
							PhConstants.HTTP_CONTENT_TYPE_JSON_CHARSET)) {

				if (logger.isDebugEnabled()) {
					logger.debug("get callId from JSON if it exists");
				}

				JSONObject jsonContent = new JSONObject(event.getContent());
				String callId = null;
				
				Iterator keys=jsonContent.keys();
				Map jsonkeyvalue=new HashMap<String,String>();

				while( keys.hasNext()){
					String key =(String) keys.next();
					Object value=jsonContent.get(key);
					jsonkeyvalue.put(key, value);
					
					if(key.equalsIgnoreCase("callingParty")){
						
						PhoneNumber calling=new PhoneNumber();
						calling.setAddress((String)value);
						leg1.set(LegDataAttributes.P_CALLING_PARTY, calling);
					}
					if(key.equalsIgnoreCase("calledParty")){
						PhoneNumber called=new PhoneNumber();
						called.setAddress((String)value);
						leg1.set(LegDataAttributes.P_CALLED_PARTY, called);
					}
				}
				if (logger.isDebugEnabled()) {
					logger.debug("content parsed map is----->" +jsonkeyvalue);
				}
				leg1.set(LegDataAttributes.NP_HTTP_JSON_CONTENT_RECEIVED, jsonkeyvalue);
				callData.set(CallDataAttribute.P_LEG1, leg1);
				
			}
		}
	}

	public void handleHttpPut(HttpEvent httpEvent, CallData callData) {
		// TODO Auto-generated method stub

	}

	// /**
	// * This method is used to read data from the incoming httpRequest
	// *
	// * @param request
	// * @return
	// * @throws IOException
	// */
	// private String readContent(HttpServletRequest request) throws IOException
	// {
	//
	// String body = null;
	// BufferedReader reader = null;
	//
	// if (logger.isDebugEnabled()) {
	// logger.debug("Inside readContent() ...");
	// }
	//
	// try {
	// // Read from request
	// StringBuilder buffer = new StringBuilder();
	// reader = request.getReader();
	// String line;
	// while ((line = reader.readLine()) != null) {
	// buffer.append(line + "\n");
	// }
	//
	// // if (body != null) {
	// body = buffer.toString();
	// body.trim();
	// // }
	//
	// } catch (IOException ex) {
	// throw ex;
	// } finally {
	// if (reader != null) {
	// try {
	// reader.close();
	// } catch (IOException ex) {
	// throw ex;
	// }
	// }
	// }
	//
	// if (logger.isDebugEnabled()) {
	// logger.debug("Leaving readContent() ..." + body);
	// }
	// return body;
	//
	// }

	public void processResourceEvent(HttpResourceEvent resEvent) {

		String origLegCallId = null;

		if (resEvent.getMessage() instanceof HttpRequest) {

			HttpRequest httpRequest = (HttpRequest) resEvent.getMessage();
			SipApplicationSession appSession = httpRequest
					.getApplicationSession();
			CallData callData = SipProtocolUtil.getCallData(appSession);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			if (origLegCallId == null) {
				origLegCallId = appSession.getId();
			}

			// callData.set(CallDataAttribute.NP_OUTGOING_HTTP_REQ,
			// httpRequest.get());

			Event httpevent = new Event(EventType.EVENT_HTTP_REQ_SENT,
					Protocol.HTTP, null);
			try {
				ProtocolRouter.getInstance().execute(
						httpevent,
						callData,
						PhUtilityServices.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
								.getServiceHandler());
			} catch (Exception e) {
				logger.error(origLegCallId
						+ ":: Exception in raising event EVENT_HTTP_REQ_SENT to App. "
						+ e.getMessage());
			}

		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Sent HTTP request header is ");
		}

	}

}
