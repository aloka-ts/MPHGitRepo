package com.agnity.ph.Enum;

import java.util.List;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.ph.common.ProtocolHandlerFactory;
import com.agnity.ph.common.ProtocolRouter;
import com.baypackets.ase.enumclient.EnumListener;
import com.baypackets.ase.ra.enumserver.receiver.EnumReceiver;

public class EnumResponseListener implements EnumListener {
	
	transient private static Logger logger = Logger
		
			.getLogger(EnumResponseListener.class);
	
	private CallData calldata=null;

	public  EnumResponseListener(CallData callData){
		this.calldata= callData;
	}
	
	@Override
	public void handleError(int arg0) {
		if (logger.isInfoEnabled()) {
			logger.info(":: Inside handleError" + arg0);
		}
		try {

			EnumProtocolHandler ph = (EnumProtocolHandler) ProtocolHandlerFactory
					.getProtocolHandler(Protocol.ENUM);
			ph.handleResponse(this.getCallData(),null);
		} catch(Exception e){
			logger.error(" xception e "+e);
		}
		
	}

	@Override
	public void handleException(Exception arg0) {
		// TODO Auto-generated method stub
		try {

			EnumProtocolHandler ph = (EnumProtocolHandler) ProtocolHandlerFactory
					.getProtocolHandler(Protocol.ENUM);
			ph.handleResponse(this.getCallData(),null);
		} catch(Exception e){
			logger.error(" xception e "+e);
		}
	}

	@Override
	public void receiveUriList(List list) {
		if (logger.isInfoEnabled()) {
			logger.info(":: Inside receiveUriList " + list);
		}

		try {

			EnumProtocolHandler ph = (EnumProtocolHandler) ProtocolHandlerFactory
					.getProtocolHandler(Protocol.ENUM);
			ph.handleResponse(this.getCallData(),list);
		} catch(Exception e){
			logger.error(" xception e "+e);
		}
		
	
	}
	
	public CallData getCallData(){
		return calldata;
	}
}
