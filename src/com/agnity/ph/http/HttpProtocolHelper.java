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

import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.sip.SipApplicationSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.AppStatsProviderInterface;
import com.agnity.ph.common.AppStatsRegistery;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.common.logging.AseHttpMsgLoggingInterface;
import com.baypackets.ase.sipconnector.AseMessageLoggingInterface;

/**
 * This class is helper class used by HttpProtocolHandler to send HTTP respone
 * and process incoming HTTP request
 *
 */
public class HttpProtocolHelper {

	private static Logger logger = Logger.getLogger(HttpProtocolHelper.class);
	
	

	private static final String TEXT_HTML_CONTENT = "text/html";

	public static void sendHTTPResponse(SipApplicationSession appSession,
			CallData calldata, Action action) throws IOException {

		String origLegCallId = (String) calldata
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside sendHTTPResponse...for event "
					+ action.getHttpResSendEvent());
		}
		HttpEvent httpEvent = action.getHttpResSendEvent();
		if(httpEvent==null){
			httpEvent=setHttpEvent(calldata);
		}

		PrintWriter out = null;

		try {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inside sendHTTPResponse...for httpRes "
						+ httpEvent.getHttpResponse()+" For content type "+httpEvent.getRespContentType());
			}
			

			httpEvent.setRespTimeStamp(new Date());

			if (httpEvent.getRespContent() != null) {
				
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Write the conten ton output stream");
				}
				
				out = new PrintWriter(httpEvent.getHttpResponse().getOutputStream());
				
			}

			if (httpEvent.getRespContentType() != null) {
				
				httpEvent.getHttpResponse().setContentType(
						httpEvent.getRespContentType());
			}
			
			
			sendResponse(calldata, httpEvent, out);

			// calldata.set(CallDataAttribute.P_HTTP_RES_SENT_TIME, new Date());

			HttpServletRequest httpReq = httpEvent.getHttpRequest();

			if (HttpProtocolHandler.m_loggingHandler.isHttpLoggingEnabled()) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: http logging is enabled ...logResponse");
				}

				String response = HttpProtocolHandler.m_loggingHandler
						.getHttpLoggingInterface().logResponse(
								AseHttpMsgLoggingInterface.DIRECTION_OUT,
								httpEvent.getHttpResponse(),
								httpEvent.getRespContent(),
								httpReq.getRemoteHost(),
								"" + httpReq.getRemotePort());

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Response is " + response);
				}

				calldata.set(CallDataAttribute.NP_OUTGOING_HTTP_RES, response);
			}

//			HttpProtocolUtil.startTimer(appSession, 0, false,
//					PhConstants.HTTP_RESP_SENT_TIMER, httpEvent);

		} catch (Exception e) {
			out = new PrintWriter(httpEvent.getHttpResponse().getOutputStream());
			logger.error(origLegCallId + ":: " + e.toString(), e);
			displayErrorPage(out, e);
		}
		
		if (httpEvent.getRespCode() != -1) {
			httpEvent.getHttpResponse().setStatus(httpEvent.getRespCode());
		} else {
			httpEvent.getHttpResponse().setStatus(HttpServletResponse.SC_OK);
		}

		 SimpleDateFormat sdfDate = new SimpleDateFormat("yyyy-MM-dd-HH:mm:ss.SSS");//dd/MM/yyyy
		    Date now = new Date();
		    String strDate = sdfDate.format(now);
		calldata.set(CallDataAttribute.P_CALL_DISCONNECT_TIME,new Date());
		
		Event event = new Event(EventType.EVENT_HTTP_RESP_SENT,
				Protocol.HTTP, httpEvent.getLeg());
		event.setHttpEvent(httpEvent);

		ServiceInterface serviceHandler = PhUtilityServices
				.getInstance(
						(String) calldata
								.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();

		try {
			ProtocolRouter.getInstance().execute(event, calldata,
					serviceHandler);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		HttpProtocolUtil.writeServiceCdr(calldata,null);
		HttpProtocolUtil.setAppSessionTimeout(appSession, 1, origLegCallId);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside sendHTTPResponse...Leaving");
		}

	}
	
	
	/**
	 * this method is used to set http veent details for outgoing response
	 * @param callData
	 * @return
	 */
	private static HttpEvent setHttpEvent(CallData callData) {
		
		LegData leg1Data=(LegData)callData.get(CallDataAttribute.P_LEG1);
		String rsContent = (String) leg1Data
				.get(LegDataAttributes.NP_HTTP_RESPONSE_CONTENT);

		String resContentType = (String) leg1Data
				.get(LegDataAttributes.NP_HTTP_RESPONSE_CONTENT_TYPE);

		String respCode = (String) leg1Data
				.get(LegDataAttributes.NP_HTTP_RESPONSE_CODE);

		HttpServletResponse httpResponse = (HttpServletResponse) callData
				.get(CallDataAttribute.NP_HTTP_RES);
		HttpServletRequest httpRequest = (HttpServletRequest) callData
				.get(CallDataAttribute.NP_HTTP_REQ);

		if (logger.isDebugEnabled()) {
			logger.debug("setHttpEvent");
		}
		HttpEvent event = new HttpEvent();
		int resCInt = 200;
		if (rsContent != null || resContentType != null || respCode != null) {

			if (respCode != null) {
				resCInt = Integer.parseInt(respCode);
			}
			event.setRespCode(resCInt);
			event.setRespContentType(resContentType);
			event.setRespContent(rsContent);

		}

		event.setHttpRequest(httpRequest);
		event.setHttpResponse(httpResponse);
		return event;
		//sendHttpRes.setHttpResSendEvent(event);
	}

	/**
	 * Displays an error page to the client.
	 * 
	 * @param out
	 *            object of PrintWriter
	 * @param e
	 *            object of Exception
	 */
	private static void displayErrorPage(PrintWriter out, Exception e) {
		if (logger.isDebugEnabled()) {
			logger.debug("Displaying error page...");
		}
		try {
			if (out != null) {
				out.println("<html>");
				out.println("The following error occurred: " + e.toString());
				out.println("</html>");
				out.close();
			}
		} catch (Exception e2) {
			logger.error(e2.toString(), e2);
		}
	}

	/**
	 * This method is used to fetch and display statistics for a specific
	 * servicename
	 * 
	 * @param calldata
	 * @param serviceName
	 */
	public static void fetchAnDisplayStats(CallData calldata, String serviceName) {

		if (logger.isDebugEnabled()) {
			logger.debug("Inside fetchAnDisplayStats...");
		}

		PrintWriter out = null;

		try {
			HttpServletResponse response = (HttpServletResponse) calldata
					.get(CallDataAttribute.NP_HTTP_RES);

			out = new PrintWriter(response.getOutputStream());

			if (logger.isDebugEnabled()) {
				logger.debug("Inside fetchAnDisplayStats show stats");
			}
			response.setContentType(TEXT_HTML_CONTENT);

			if (logger.isDebugEnabled()) {
				logger.debug("Handling HTTP POST request..Service Name is ."
						+ serviceName);
			}
			AppStatsProviderInterface statsProvider = AppStatsRegistery.APP_STATS_REGISTRY
					.getStatsProviders(serviceName);

			if (logger.isDebugEnabled()) {
				logger.debug("Stats Provider returned by registery is  ."
						+ statsProvider);
			}

			displaySuccessPage(out, serviceName, statsProvider);
		} catch (Exception e) {
			logger.error(e.toString(), e);
			displayErrorPage(out, e);
		}

	}

	/**
	 * Displays a success message to the client after posting the request.
	 * 
	 * @param out
	 *            object of PrintWriter
	 * @param appSessionId
	 *            id of Application Session
	 * @throws Exception
	 */
	private static void displaySuccessPage(PrintWriter out, String serviceName,
			AppStatsProviderInterface statsProvider) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("Displaying success page to the client...");
		}
		out.println("<html>");
		out.println("<b>Statistics provided by service " + serviceName
				+ " are here : </b><br/><br/>");
		out.println("" + statsProvider.provideStats() + " <br/><br/>");
		out.println("<br/><hr/>");
		out.println("</html>");
		out.close();
	}

	/**
	 * This method is used to return Form in HTTP response
	 * @param out 
	 * 
	 * @param response
	 * @param calldata
	 * @param out
	 * @throws Exception
	 */
	static void sendResponse(CallData callData,HttpEvent httpEvent, PrintWriter out) throws Exception {
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId +":: sendResponse :sendResponse sent by application to the client...");
		}

		HttpServletResponse httpResponse = httpEvent.getHttpResponse();
//		if (callData.get(CallDataAttribute.NP_HTTP_RES_HDRS) != null) {
//
//			@SuppressWarnings("unchecked")
//			Map<String, List<String>> httpHeaders = (Map<String, List<String>>) callData
//					.get(CallDataAttribute.NP_HTTP_RES_HDRS);
//
//			Set<Entry<String, List<String>>> enteries = httpHeaders.entrySet();
//
//			Iterator<Entry<String, List<String>>> iterator = enteries
//					.iterator();
//
//			while (iterator.hasNext()) {
//
//				Entry<String, List<String>> entry = iterator.next();
//				if (logger.isDebugEnabled()) {
//					logger.debug(origLegCallId + ":: Add header  Name: " + entry.getKey()
//							+ " Value :" + entry.getValue());
//				}
//				for (String e : entry.getValue()) {
//					httpResponse.setHeader(entry.getKey(), e);
//				}
//			}
//		}
		//PrintWriter out = new PrintWriter(httpResponse.getOutputStream());
		
		String content = httpEvent.getRespContent();

		String contentType = httpEvent.getRespContentType();
//		String url = (String) callData
//				.get(CallDataAttribute.NP_HTTP_REQ_URL);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId +":: Http content and content type provided by app is ..."
					+ content + " type " + contentType);
		}
		
		if (content != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId +":: set content length "+content.length());
			}
			httpResponse.setContentLength(content.length());
		}else{
			httpResponse.setContentLength(0);
		}
		
		if (contentType != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId +":: set content type "+ contentType);
			}
			httpResponse.setContentType(contentType);
		}
		
	//	PrintWriter out = httpResponse.getWriter();
		
//		httpResponse.setHeader("Server", null);
//		httpResponse.setHeader("Connection", null);
		
		
	//	httpEvent.getHttpResponse().setStatus(200);
		
		if (content != null) {
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId +":: write http content to out stream and close it");
			}
			
			out.write(content);
			out.close();
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId +"::  http response sent");
		}
		
		//  public void logResponse(int reason, int direction, byte[] response, int statusCode, int method, String url) {

	//	byte[] bytestr=content.getBytes();
		
		HttpServletRequest httpReq=(HttpServletRequest) callData.get(CallDataAttribute.NP_HTTP_REQ);
//		
//		if (logger.isDebugEnabled()) {
//			logger.debug(origLegCallId +":: Http Request received was  "+httpReq);
//		}

		
		if (HttpProtocolHandler.m_loggingHandler.isHttpLoggingEnabled()) {
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId +":: http logging is enabled ...logResponse");
			}

			String response = HttpProtocolHandler.m_loggingHandler
					.getHttpLoggingInterface()
					.logResponse(
							AseHttpMsgLoggingInterface.DIRECTION_OUT,
							httpResponse, content, httpReq.getRemoteHost(),
							"" + httpReq.getRemotePort());
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId +":: Response is "+ response);
			}

			callData.set(CallDataAttribute.NP_OUTGOING_HTTP_RES, response);
		} 
//		else {
//
//			
//			if (logger.isDebugEnabled()) {
//				logger.debug(origLegCallId +":: set NP_OUTGOING_HTTP_RES ");
//			}
//			String response = HttpProtocolHandler.m_loggingHandler
//					.getHttpLoggingInterface().outgoingHttpResponseToString(
//							httpResponse, content);
//			callData.set(CallDataAttribute.NP_OUTGOING_HTTP_RES, response);
//		}
	}
	
	
	/**
	 * This method is used to start a timer requested by application
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void startApplicationTimer(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception{
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside startApplicationTimer"
					+ action.getTimerName());
		}

		String timerName = PhConstants.APP_TIMER;

		if (action.getTimerName() != null) {
			timerName = timerName + action.getTimerName();
		}
		HttpProtocolUtil.startTimer(appSession, action.getTimerTimeInMills(),
				false, timerName,null);
		
		ServiceInterface serviceHandler = PhUtilityServices
				.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		Event appEvent = new Event(EventType.EVENT_APP_TIMER_STARTED, Protocol.HTTP, action.getLeg());

		ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
	}

	/**
	 * This method is used to stop a timer requested by application
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void stopApplicationTimer(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception{
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside stopApplicationTimer"
					+ action.getTimerName());
		}

		String timerName = PhConstants.APP_TIMER;

		if (action.getTimerName() != null) {
			timerName = timerName + action.getTimerName();
		}
		HttpProtocolUtil.stopTimer(appSession, timerName);

		try{
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event appEvent = new Event(EventType.EVENT_APP_TIMER_STOPPED, Protocol.HTTP, action.getLeg());

			ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
		}catch(Exception ex){
			logger.error(origLegCallId +"Error occured while stopping application timer : " + action.getTimerName() );
			logger.error(origLegCallId +"Error occured : " + ex);
			throw ex;
		}
	}

}
