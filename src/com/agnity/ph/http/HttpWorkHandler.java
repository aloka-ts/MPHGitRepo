package com.agnity.ph.http;

import javax.servlet.sip.SipApplicationSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.ProtocolHandlerFactory;
import com.agnity.ph.common.threadpool.EventThreadPool;
import com.agnity.ph.common.threadpool.EventWork;

public class HttpWorkHandler implements EventWork {

	private SipApplicationSession appSession;
	private int queueIndx;
	private CallData callData;
	private HttpEvent aseHttpEvent;
	private String httpEventid;
	private String appSesssionId;
	
	public static Logger logger = Logger.getLogger(HttpWorkHandler.class);

	public HttpWorkHandler(HttpEvent event, CallData callData,
			SipApplicationSession appSession, int idx) {
		this.appSession = appSession;
		this.appSesssionId=appSession.getId();
		this.callData = callData;
		this.aseHttpEvent = event;
		this.httpEventid=event.getEventId();
		queueIndx = idx;

	}

	@Override
	public int getQueueIndex() {
		return queueIndx;
	}

	@Override
	public boolean isValid() {
		if (appSession.isValid()) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public void finishIt() throws Exception {
		
		if (logger.isDebugEnabled()) {
			logger.debug(appSesssionId + "::finishIt ");
		}
		HttpProtocolHandler hph = (HttpProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.HTTP);


	if (httpEventid.equals(HttpEvent.EVENT_HTTP_GET)) {
		if (logger.isDebugEnabled()) {
			logger.debug(appSesssionId + ":: Handle HTTP GET");
		}
		hph.handleHttpGet(aseHttpEvent, callData,appSession);
	} else if (httpEventid.equals(HttpEvent.EVENT_HTTP_POST)) {
		if (logger.isDebugEnabled()) {
			logger.debug(appSesssionId + ":: Handle HTTP POST");
		}
		hph.handleHttpPost(aseHttpEvent, callData,appSession);
	} else if (httpEventid.equals(HttpEvent.EVENT_HTTP_PUT)) {
		if (logger.isDebugEnabled()) {
			logger.debug(appSesssionId + ":: Handle HTTP PUT");
		}
		hph.handleHttpPut(aseHttpEvent, callData);
	} else {
		throw new Exception(appSesssionId + ":: Unsupported HTTP Event");
	}

	}

	@Override
	public SipApplicationSession getAppSession() {
		// TODO Auto-generated method stub
		return this.appSession;
	}

}
