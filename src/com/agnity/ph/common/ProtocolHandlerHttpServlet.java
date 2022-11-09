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
package com.agnity.ph.common;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.Date;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipFactory;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;
import org.json.JSONObject;

import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.threadpool.EventThreadPool;
import com.agnity.ph.http.HttpEvent;
import com.agnity.ph.http.HttpProtocolHandler;
import com.agnity.ph.http.HttpWorkHandler;
import com.agnity.ph.sip.SipProtocolUtil;
import com.agnity.service.fee.FeeServiceServlet;
import com.baypackets.ase.cdr.CDRImpl;
import com.baypackets.ase.common.Registry;
import com.baypackets.ase.container.AseApplicationSession;
import com.baypackets.ase.container.AseContext;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sipconnector.AseDialogManager;
import com.baypackets.ase.sipconnector.AseSipSession;
import com.baypackets.ase.util.AseStrings;
import com.baypackets.ase.util.Constants;

/**
 * This class is the HTTP Protocol handler servlet class . This class is the
 * abstract class which is extended by services to handle HTTP events.This HTTP
 * interface can help services to trigger some service specific actions from
 * HTTP interface.
 */
public abstract class ProtocolHandlerHttpServlet extends HttpServlet {

	private static final long serialVersionUID = 7155196461444876225L;
	private static Logger logger = Logger
			.getLogger(ProtocolHandlerHttpServlet.class);

	public abstract ServiceInterface getServiceInstance();

	private AseDialogManager dialogManager;
	private EventThreadPool pool;

	/**
	 * Constructor
	 */
	public ProtocolHandlerHttpServlet() {
	}

	/**
	 * This method is the init method used to initialise Servlet configuration
	 */
	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		if (logger.isDebugEnabled()) {
			logger.debug("ProtocolHandlerHttpServlet() called...");
		}

		dialogManager = (AseDialogManager) Registry
				.lookup(Constants.DIALOG_MGR);

		if (PhUtilityServices.getInstance(
				getServiceInstance().getApplicationName()).getServiceHandler() == null) {
			PhUtilityServices.getInstance(
					getServiceInstance().getApplicationName())
					.setServiceHandler(getServiceInstance());
		}

		int poolSize = 0;
		if (logger.isDebugEnabled()) {
			logger.debug("V ==> Thread pool size = " + poolSize);
		}

		int threadTimeoutTime = 100;
		if (poolSize != 0) {

			pool = new EventThreadPool(poolSize, null, threadTimeoutTime);
			pool.initialize();

			if (logger.isDebugEnabled()) {
				logger.debug("Thread pool created with pool size " + poolSize);
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug("Not creating any Thread pool as poolsize is "
						+ poolSize);
			}
		}

	}

	// /**
	// * @param request
	// * @throws javax.servlet.ServletException
	// * @throws java.io.IOException
	// */
	// public void doGet(HttpServletRequest request, HttpServletResponse
	// response)
	// throws ServletException, IOException {
	// this.doPost(request, response);
	// if (logger.isInfoEnabled()) {
	// logger.info("Entering doGet of ProtocolHandlerHttpServlet");
	// }
	// }
	//
	// /**
	// * @param request
	// * @throws javax.servlet.ServletException
	// * @throws java.io.IOException
	// */
	// public void doPost(HttpServletRequest request, HttpServletResponse
	// response)
	// throws ServletException, IOException {
	// if (logger.isInfoEnabled()) {
	// logger.info("Entering doPost of ProtocolHandlerHttpServlet");
	// }
	//
	// if (logger.isDebugEnabled()) {
	// logger.debug("doPost() called");
	// }
	//
	// HttpProtocolHandler.getInstance().doPost(request, response);
	// }

	private static String OP_TYPE = "OP_TYPE";
	
	/**
	 * 
	 */
	@Override
	public void doOptions(HttpServletRequest req, HttpServletResponse resp)
	        throws IOException {
	    //The following are CORS headers. Max age informs the 
		if (logger.isInfoEnabled()) {
			logger.info("Entering doOptions()");
		}
	    //browser to keep the results of this call for 1 day.
		 resp.setHeader("Access-Control-Allow-Origin", "*");
		    
		    resp.setHeader("Access-Control-Allow-Origin", "*");
		    resp.setHeader("Access-Control-Allow-Headers",
	                "CSRF-Token, X-Requested-By, Authorization, Content-Type,x-agnity-commit-mode");
		    resp.setHeader("Access-Control-Allow-Credentials", "true");
		    resp.setHeader("Access-Control-Allow-Methods",
	                "GET, POST, PUT, DELETE, OPTIONS, HEAD");
		    resp.setHeader("Allow", "GET, HEAD, POST, TRACE, OPTIONS");
		    
		    if (logger.isInfoEnabled()) {
		      logger.info("Leaving doOptions()");
		    }
	}

	/**
	 * @param request
	 * @throws javax.servlet.ServletException
	 * @throws java.io.IOException
	 */
	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		try {
			
			if (logger.isInfoEnabled()) {
				logger.info("Entering doPost()");
			}
			
			 response.setHeader("Access-Control-Allow-Origin", "*");
			 response.setHeader("Access-Control-Allow-Origin", "*");
				response.setHeader("Access-Control-Allow-Headers",
			                "CSRF-Token, X-Requested-By, Authorization, Content-Type, x-agnity-commit-mode");
				response.setHeader("Access-Control-Allow-Credentials", "true");
				response.setHeader("Access-Control-Allow-Methods",
			                "GET, POST, PUT, DELETE, OPTIONS, HEAD");
				response.setHeader("Allow", "GET, HEAD, POST, TRACE, OPTIONS");
			HttpEvent event = new HttpEvent();
			event.setHttpRequest(request);
			event.setHttpResponse(response);
			event.setsContext(getServletContext());
			event.setRemoteHost(request.getRemoteHost());
			event.setRemotePort(request.getRemotePort());
			event.setReqTimeStamp(new Date());
			event.setRequestURL(request.getRequestURI());
			if (request.getAttribute(OP_TYPE) != null) {
				event.setEventId(HttpEvent.EVENT_HTTP_GET);
			} else {
				event.setEventId(HttpEvent.EVENT_HTTP_POST);
				event.loadData();
			}
			if (logger.isInfoEnabled()) {
				logger.info("Inside doPost() before fireEvent");
			}

			fireEvent(event);
			if (logger.isInfoEnabled()) {
				logger.info("Leaving doPost() after fireEvent");
			}
			
		} catch (Exception e) {
			logger.error("Leaving doPost() after Exception " + e);
		}
	}

	/**
	 * @param request
	 * @throws javax.servlet.ServletException
	 * @throws java.io.IOException
	 */
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		if (logger.isInfoEnabled()) {
			logger.info("TESTV Inside do Get ");
		}
		request.setAttribute(OP_TYPE, "1");
		this.doPost(request, response);
	}

	public void doPut(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {

		 response.setHeader("Access-Control-Allow-Origin", "*");
		HttpEvent event = new HttpEvent();
		event.setEventId(HttpEvent.EVENT_HTTP_PUT);
		// String eventId = EVENT_HTTP_PUT;
		fireEvent(event);

	}

	public void fireEvent(HttpEvent event) {

		// MergedServletContext mergedContext = (MergedServletContext)
		// getServletContext().get;
		//
		// AseContext aseContext = mergedContext.getAseContext();
		//
		// String appid=mergedContext.getAseContext().getId();
		//
		//
		// if (logger.isDebugEnabled()) {
		// logger.debug("<AseHttpServlet>APPID from Merged context is " +
		// appid);
		// }

		SipApplicationSession appSession = getExistingApplicationSession(event);

		if (appSession == null) {

			String appid = null;

			HttpServletRequest request = event.getHttpRequest();

			String aai = request.getParameter(AseStrings.PARAM_AAI);

			if (aai != null && aai.indexOf("aai=") != -1) {
				appid = aai.substring(aai.indexOf("aai=") + 4);
			}

			if (appid != null) {
				if (logger.isDebugEnabled()) {
					logger.debug("<AseHttpServlet> doPost() Know whom to invoke App id found to be "
							+ appid);
				}
				try {
					appid = URLDecoder.decode(appid,
							AseStrings.XML_ENCODING_UTF8);
				} catch (UnsupportedEncodingException e) {
					logger.error("could not decode Application Session ID", e);
					return;
				}

				if (logger.isDebugEnabled()) {
					logger.debug("AseHttpServlet  App id After Decoding is  is"
							+ appid);
				}

				appSession = (SipApplicationSession) getServletContext()
						.getAttribute(appid);

			}

			if (appSession == null) {

				SipFactory factory = (SipFactory) getServletContext()
						.getAttribute("javax.servlet.sip.SipFactory");
				appSession = factory.createApplicationSession();

				if (logger.isDebugEnabled()) {
					logger.debug("<AseHttpServlet> doPost() created new App session "
							+ appSession);
				}
			} else {

				if (logger.isDebugEnabled()) {
					logger.debug("<AseHttpServlet> doPost() App Session found "
							+ appSession);
				}
			}

		}

		if (appSession != null) {

			event.setAppSession(appSession);

			handleHttpEvents(event);
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug("<AseHttpServlet> doPost() Donot know whom to invoke App id found to be null");
			}
		}
	}

	public void handleHttpEvents(HttpEvent aseHttpEvent) {
		String httpEventid = aseHttpEvent.getEventId();

		String origLegCallId = "";
		try {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::handleHttpEvents");
			}

			CallData callData = null;

			SipApplicationSession appSession = aseHttpEvent.getAppSession();

			if (appSession != null) {
				callData = SipProtocolUtil.getCallData(appSession);

				origLegCallId = appSession.getId();

			}

			if (callData == null) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: CallData is null seems like new HTTP request update appsessionId "
							+ aseHttpEvent.getAppSession() + " and  ServiceId "
							+ getServiceInstance().getApplicationName()
							+ " in calldata ");
				}

				callData = new CallData();
				callData.set(CallDataAttribute.SERVICE_ID, getServiceInstance()
						.getApplicationName());
				callData.set(CallDataAttribute.P_APP_SESSION_ID, aseHttpEvent
						.getAppSession().getId());
//				callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
//						origLegCallId);
				
				callData.set(CallDataAttribute.P_CDR_REF, getCDR((AseApplicationSession)appSession, aseHttpEvent.getHttpRequest().getSession().getId()));
				appSession.setAttribute(CallData.CALL_DATA, callData);
				callData.set(CallDataAttribute.P_NETWORK_TRANSACTION,new MutableInt(0));

			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::P_HTTP_REMOTE_HOST ... "
						+ aseHttpEvent.getRemoteHost());
				logger.debug(origLegCallId + "::P_HTTP_REMOTE_PORT ... "
						+ aseHttpEvent.getRemotePort());
			}
			
			callData.set(CallDataAttribute.NP_HTTP_RES, aseHttpEvent.getHttpResponse());
			callData.set(CallDataAttribute.NP_HTTP_REQ, aseHttpEvent.getHttpRequest());

			if (pool != null) {

				/**
				 * Assign work to queue for sequntial request handling for
				 * multiple requests case
				 */
				assignHttpWorkToQueue(appSession, aseHttpEvent, callData);

			} else {

				HttpProtocolHandler hph = (HttpProtocolHandler) ProtocolHandlerFactory
						.getProtocolHandler(Protocol.HTTP);

				if (httpEventid.equals(HttpEvent.EVENT_HTTP_GET)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Handle HTTP GET");
					}
					hph.handleHttpGet(aseHttpEvent, callData,appSession);
				} else if (httpEventid.equals(HttpEvent.EVENT_HTTP_POST)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Handle HTTP POST");
					}
					hph.handleHttpPost(aseHttpEvent, callData,appSession);
				} else if (httpEventid.equals(HttpEvent.EVENT_HTTP_PUT)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Handle HTTP PUT");
					}
					hph.handleHttpPut(aseHttpEvent, callData);
				} else {
					throw new Exception(origLegCallId
							+ ":: Unsupported HTTP Event");
				}
			}
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Http Event processing failed for event Id "
					+ httpEventid + ". Error is " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(origLegCallId
						+ ":: Error in handleHttpEvents for event id "
						+ httpEventid, ex);
			}
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::handleHttpEvents Leaving");
		}
	}

	/**
	 * This method is used to get existing application session from http event
	 * 
	 * @param event
	 * @return
	 */
	SipApplicationSession getExistingApplicationSession(HttpEvent event) {

		if (logger.isDebugEnabled()) {
			logger.debug("getExistingApplicationSession Entering ");
		}
		SipApplicationSession appSession = null;

		if (event.getContentLength() != -1 && event.getContentType()!=null) {

			if (event.getContentType().equals(
					PhConstants.HTTP_CONTENT_TYPE_JSON)
					|| event.getContentType().equals(
							PhConstants.HTTP_CONTENT_TYPE_JSON_CHARSET)) {

				if (logger.isDebugEnabled()) {
					logger.debug("get callId from JSON if it exists");
				}

				JSONObject jsonContent = new JSONObject(event.getContent());
				String callId = null;

				 if (!jsonContent.isNull(PhConstants.HTTP_JSON_CALLID)) {

					callId = jsonContent.getString(PhConstants.HTTP_JSON_CALLID);
					
					if (logger.isDebugEnabled()) {
						logger.debug("callId from JSON CALLID is " + callId);
					}
				}if (!jsonContent.isNull(PhConstants.HTTP_JSON_SESSID)) {
					callId = jsonContent.getString(PhConstants.HTTP_JSON_SESSID);

					if (logger.isDebugEnabled()) {
						logger.debug("callId from JSON SESSIONID  is " + callId);
					}
				}
				if (callId != null) {
					AseSipSession sipSession = dialogManager.getSession(callId);

					if (logger.isDebugEnabled()) {
						logger.debug("AseSipSession from callid is  "
								+ sipSession);
					}
					if (sipSession != null) {
						appSession = sipSession.getApplicationSession();
						String legId = (String) sipSession
								.getAttribute(PhConstants.LEG_ID);
						event.setLeg(legId);
						event.setSessionId(callId);

					}
//						else{
//						
//						if (logger.isDebugEnabled()) {
//							logger.debug("getExistingApplicationSession find application session from existing active appsessions");
//						}
//						
//						List<String> actionsipSessions = PhUtilityServices
//								.getInstance(
//										getServiceInstance()
//												.getApplicationName())
//								.getActiveAppSessionIdsList();
//
//						SipSessionsUtil util = PhUtilityServices.getInstance(
//								getServiceInstance().getApplicationName())
//								.getSipSessionsUtil();
//						
//						if (!actionsipSessions.isEmpty()) {
//							appSession = util
//									.getApplicationSessionById(actionsipSessions
//											.get(0));
//							
//							if (logger.isDebugEnabled()) {
//								logger.debug("getExistingApplicationSession  application session from existing active appsessions" + appSession);
//							}
//							
//							if (appSession!=null && !jsonContent
//									.isNull(PhConstants.HTTP_JSON_MSGTYPE)) {
//
//								String msgType = jsonContent
//										.getString(PhConstants.HTTP_JSON_MSGTYPE);
//
//								CallData callData =SipProtocolUtil.getCallData(appSession);
//								
//
//								if (PhConstants.HTTP_JSON_MSGTYPE_CUSTOMER
//										.equals(msgType)) {
//
//									if (logger.isDebugEnabled()) {
//										logger.debug("createKWDReqJson for msgType "
//												+ msgType);
//									}
//
//									LegData leg1Data = (LegData) callData
//											.get(CallDataAttribute.P_LEG1);
//									callId = (String) leg1Data
//											.get(LegDataAttributes.P_CALL_ID);
//
//									event.setLeg(CallDataAttribute.P_LEG1
//											.name());
//									event.setSessionId(callId);
//
//								} else {
//
//									LegData leg2Data = (LegData) callData
//											.get(CallDataAttribute.P_LEG2);
//									callId = (String) leg2Data
//											.get(LegDataAttributes.P_CALL_ID);
//
//									event.setLeg(CallDataAttribute.P_LEG2
//											.name());
//									event.setSessionId(callId);
//
//								}
//							} else
//
//							     if (logger.isDebugEnabled()) {
//								logger.debug("getExistingApplicationSession appsession found but can not match leg ");
//							    }
//
//						}	else{
//							if (logger.isDebugEnabled()) {
//							logger.debug("getExistingApplicationSession active appsession not found in map");
//						}
//					}
//				
//			}
		   }
		 }
		}

		if (logger.isDebugEnabled()) {
			logger.debug("getExistingApplicationSession Leaving  with appession "
					+ appSession);
		}
		return appSession;

	}

	private void assignHttpWorkToQueue(SipApplicationSession appSession,
			HttpEvent event, CallData callData) {

		if (logger.isDebugEnabled()) {
			logger.debug("Entering...assignHttpWorkToQueue");
		}
		if (pool != null) {
			try {
				int idx = (appSession.getId().hashCode()) % pool.getSize();
				idx = (idx < 0) ? (idx * (-1)) : idx;
				HttpWorkHandler work = new HttpWorkHandler(event, callData,
						appSession, idx);
				pool.assign(work);
			} catch (Exception ex) {
				logger.error("Exception:" + ex.getMessage());
			}
		}
	}

	@Override
	/**
	 * This method will be called when the servlet is destroyed/undeployed.
	 * Need to stop the running refresher thread and close all the DB Connections
	 */
	public void destroy() {
		String methodname = "destroy()";
		if (logger.isDebugEnabled()) {
			logger.debug("Entering..." + methodname);
		}

		if (pool != null) {
			pool.shutdown();
		}

		super.destroy();

		if (logger.isDebugEnabled()) {
			logger.debug("Leaving..." + methodname);
		}
	}
	
public static  CDR getCDR(AseApplicationSession appSession, String sessionId) {
		
		if(logger.isDebugEnabled())
			logger.info("getCDR() called to get CDR ref");
		// Get the CDRContext of the app that this session is associated with...
		if(appSession==null){
			if(logger.isDebugEnabled())
				logger.info("getCDR() called appsesion is currently null will add cdr ref later on");
			return null;
		}
		//AseApplicationSession appSession = (AseApplicationSession)this.getApplicationSession();
		AseContext app = appSession.getContext();
		CDR cdr = app.getCDRContext(sessionId).createCDR();

		// Populate the CDR with the initial values...
		//Marking it as default CDR
		cdr.set(CDR.DEFAULT_CDR,CDR.DEFAULT_CDR);
	//	cdr.set(CDR.CORRELATION_ID, appSession.getAttribute(Constants.CORRELATION_ID));
		cdr.set(CDR.SESSION_ID, sessionId);
///		cdr.set(CDR.ORIGINATING_NUMBER, ((SipURI)m_localParty.getURI()).getUser()); /coomenting for axtel it is coming as null for tcap
//		cdr.set(CDR.TERMINATING_NUMBER, ((SipURI)m_remoteParty.getURI()).getUser());
		//cdr.set(CDR.CALL_START_TIMESTAMP, String.valueOf(this.getSession().getCreationTime()));
//		cdr.set(CDR.BILL_TO_NUMBER, ((SipURI)m_localParty.getURI()).getUser());

		// Special case for handling the default CDR implementation:
		// We associate the host and app name with the CDR here so that the 
		// CDRContext can be looked up and re-associated with the CDR after 
		// it has been replicated.
		// We also set a flag indicating if the CDR is associated with a
		// distributable app or not.  If it is distributable, only Serializable
		// attribute values will be allowed to be set on it.
		if (cdr instanceof CDRImpl) {
			((CDRImpl)cdr).setHostName(app.getParent().getName());
			((CDRImpl)cdr).setAppName(app.getName());
			((CDRImpl)cdr).setDistributable(app.isDistributable());
		}
		if(logger.isDebugEnabled())
			logger.info("getCDR() Leaving with ref "+cdr);
		return cdr;
	}

}
