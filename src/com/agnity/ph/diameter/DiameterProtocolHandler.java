package com.agnity.ph.diameter;

import java.io.UnsupportedEncodingException;
import java.util.Date;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.ProtocolHandler;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.SipProtocolUtil;
//import com.baypackets.ase.ra.diameter.rf.RfAccountingRequest;
//import com.baypackets.ase.ra.diameter.rf.RfResourceFactory;
import com.baypackets.ase.ra.diameter.ro.CreditControlAnswer;
import com.baypackets.ase.ra.diameter.ro.CreditControlRequest;
import com.baypackets.ase.ra.diameter.ro.RoResourceException;
import com.baypackets.ase.ra.diameter.ro.enums.CCRequestTypeEnum;
import com.baypackets.ase.resource.Message;
import com.baypackets.ase.resource.ResourceException;
import com.agnity.mphdata.common.State;

public class DiameterProtocolHandler implements ProtocolHandler,
DiameterConstants {

	private static final DiameterProtocolHandler INSTANCE = new DiameterProtocolHandler();
	private static Logger logger = Logger
			.getLogger(DiameterProtocolHandler.class);

	private DiameterProtocolHandler() {
	}

	/**
	 * This method is used to process/send diameter RO/RF protocol requests
	 */
	@Override
	public void executeAction(CallData callData, Action action)
			throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("::executeAction:" + action.getActionType().name());
		}

		String serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		//		RfResourceFactory rffactory = PhUtilityServices.getInstance(serviceId)
		//				.getRfResFactory();

		SipApplicationSession appSession = SipProtocolUtil.getAppSession(
				(String) callData.get(CallDataAttribute.P_APP_SESSION_ID),
				(String) callData.get(CallDataAttribute.SERVICE_ID));

		if (logger.isDebugEnabled()) {
			logger.debug("::AppSession is :" + appSession);
		}
		switch (action.getActionType()) {

		case ACTION_SEND_CCR_EVENT: {

			if (logger.isDebugEnabled()) {
				logger.debug("::send CCR Event Request:");
			}
			DiameterProtocolHelper.sendDiameterRequest(appSession, callData, action,CCR_OUT_EVENT_REQUEST);
			break;
		}
		case ACTION_SEND_CCR_INITIAL: {

			if (logger.isDebugEnabled()) {
				logger.debug("::send CCR Initial Request:");
			}
			DiameterProtocolHelper.sendDiameterRequest(appSession, callData, action,CCR_OUT_INITIAL_REQUEST);
			break;
		}

		case ACTION_SEND_CCR_UPDATE: {

			if (logger.isDebugEnabled()) {
				logger.debug("::send CCR Update Request:");
			}
			DiameterProtocolHelper.sendDiameterRequest(appSession, callData, action,CCR_OUT_UPDATE_REQUEST);

		}
		break;
		case ACTION_SEND_CCR_TERMINATE: {

			if (logger.isDebugEnabled()) {
				logger.debug("::send CCR Termnate Request:");
			}
			DiameterProtocolHelper.sendDiameterRequest(appSession, callData, action,CCR_OUT_TERMINATION_REQUEST);
		}
		break;
		case ACTION_SEND_CCR_ANSWER: {

			DiameterProtocolHelper.sendAnswer(appSession, callData, action);
		}
		break;
		//		case ACTION_SEND_AR_EVENT_BASED_REQ: {
		//
		//			RfAccountingRequest ar = rffactory.createRequest(appSession,
		//					AR_EVENT);
		//			ar.send();
		//		}
		//			break;
		//
		//		case ACTION_SEND_AR_SESSION_BASED_REQ: {
		//
		//			RfAccountingRequest ar = rffactory.createRequest(appSession,
		//					AR_SESSION);
		//			ar.send();
		//		}
		//			break;
		}

	}

	@Override
	public void timeout(ServletTimer timer) {
		// TODO Auto-generated method stub
	}

	public static ProtocolHandler getInstance() {
		return INSTANCE;
	}

	public void handleRequest(CallData callData,
			CreditControlRequest ccRequest, int reqType,
			ServiceInterface serviceHandler) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("::handleCCR Request:" + ccRequest.getType() + ccRequest.getAvps());
		}

		callData.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(0));
		DiameterAVPParser.fetchCCRAVPs(ccRequest, callData, VENDOR_ID_3GPP);
		
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
	      legData
				.set(LegDataAttributes.P_LEG_DIAMETER_STATE,State.DIAMETER_REQUEST_RECEIVED);

		if(logger.isDebugEnabled()){
			logger.debug("handleRequest: RequestType:"+ reqType);
		}

		switch (reqType) {

		case CCR_IN_EVENT: {

			//CCRAVPAttributes avps=CCRParser.getParser().parseCCR(ccRequest);
			
			callData.set(CallDataAttribute.P_CALL_START_TIME, new Date());
			 
			Event event = new Event(EventType.EVENT_CCR_EVENT,
					Protocol.DIAMETER, CallDataAttribute.P_LEG1.name());

			//event.setAVPAttributes(avps);

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);
		}
		break;

		case CCR_IN_INITIAL: {

			if(logger.isDebugEnabled()){
				logger.debug("handleRequest: Initial set start time:");
			}
			callData.set(CallDataAttribute.P_CALL_START_TIME, new Date());
			//	CCRAVPAttributes avps=CCRParser.getParser().parseCCR(ccRequest);

			Event event = new Event(EventType.EVENT_CCR_INITIAL,
					Protocol.DIAMETER, CallDataAttribute.P_LEG1.name());
			//event.setAVPAttributes(avps);
			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);
		}
		break;
		case CCR_IN_INERIM: {
			callData.set(CallDataAttribute.P_CALL_CONNECT_TIME, new Date());
			CCRAVPAttributes_NotUsed avps=CCRParser_NotUsed.getParser().parseCCR(ccRequest);
			Event event = new Event(EventType.EVENT_CCR_INTERIM,
					Protocol.DIAMETER, CallDataAttribute.P_LEG1.name());
			//	event.setAVPAttributes(avps);
			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);
		}
		break;
		case CCR_IN_TERMINATION: {

			 callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());
			CCRAVPAttributes_NotUsed avps=CCRParser_NotUsed.getParser().parseCCR(ccRequest);
			Event event = new Event(EventType.EVENT_CCR_TERMINATION,
					Protocol.DIAMETER, CallDataAttribute.P_LEG1.name());
			//	event.setAVPAttributes(avps);
			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);
		}
		break;

		default:
			logger.error("handleRequest: invalid request Type:" + reqType);
		}
	}

	//	public void handleRequest(CallData callData,
	//			RfAccountingRequest accountingReq, int arEventRecord,
	//			ServiceInterface serviceHandler) throws Exception {
	//
	//		if (logger.isDebugEnabled()) {
	//			logger.debug("::handleAccounting Request:"
	//					+ accountingReq.getType());
	//		}
	//		switch (arEventRecord) {
	//
	//		case AR_EVENT_RECORD: {
	//
	//			ARAVPAttributes_NotUSed avps=ARParser_NotUsed.getParser().parseAR(accountingReq);
	//			
	//			Event event = new Event(EventType.EVENT_AR_EVENT,
	//					Protocol.DIAMETER, CallDataAttribute.P_LEG1.name());
	//			event.setAVPAttributes(avps);
	//			ProtocolRouter.getInstance().execute(event, callData,
	//					serviceHandler);
	//		}
	//			break;
	//
	//		case AR_START_RECORD: {
	//
	//			ARAVPAttributes_NotUSed avps=ARParser_NotUsed.getParser().parseAR(accountingReq);
	//			Event event = new Event(EventType.EVENT_AR_START_RECORD,
	//					Protocol.DIAMETER, CallDataAttribute.P_LEG1.name());
	//			event.setAVPAttributes(avps);
	//			ProtocolRouter.getInstance().execute(event, callData,
	//					serviceHandler);
	//		}
	//			break;
	//		case AR_INERIM_RECORD: {
	//			ARAVPAttributes_NotUSed avps=ARParser_NotUsed.getParser().parseAR(accountingReq);
	//			Event event = new Event(EventType.EVENT_AT_INTERIM_RECORD,
	//					Protocol.DIAMETER, CallDataAttribute.P_LEG1.name());
	//			event.setAVPAttributes(avps);
	//			ProtocolRouter.getInstance().execute(event, callData,
	//					serviceHandler);
	//		}
	//			break;
	//		case AR_STOP_RECORD: {
	//			ARAVPAttributes_NotUSed avps=ARParser_NotUsed.getParser().parseAR(accountingReq);
	//			Event event = new Event(EventType.EVENT_CCR_TERMINATION,
	//					Protocol.DIAMETER, CallDataAttribute.P_LEG1.name());
	//			event.setAVPAttributes(avps);
	//			ProtocolRouter.getInstance().execute(event, callData,
	//					serviceHandler);
	//		}
	//			break;
	//		}

	//}

	/**
	 * This method is sued to ahndle diameter responses
	 * @param message
	 * @param callData
	 * @param serviceHandler
	 */
	public void handleResponse(Message message,CallData callData,ServiceInterface serviceHandler) {

		if (logger.isDebugEnabled()) {
			logger.debug("::handleResponse...");
		}

		if(message instanceof CreditControlAnswer){

			CreditControlAnswer cca= (CreditControlAnswer)message;

			try {
				DiameterAVPParser.fetchCCAAVPs(cca, callData);
			} catch (UnsupportedEncodingException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (ResourceException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}

			if (logger.isDebugEnabled()) {
				logger.debug("::CCA received...");
			}

			Event event = new Event(EventType.EVENT_CCA_RECEIVED,
					Protocol.DIAMETER, CallDataAttribute.P_LEG2.name());

			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
			
			if(legData==null){
				legData= new LegData();
				if (logger.isDebugEnabled()) {
					logger.debug("::craete Leg2ata..");
				}
			}
			legData.set(LegDataAttributes.DIAMETER_IN_CCR_RES, cca);
			
			try {
				if (cca.getCCRequestType() == CCRequestTypeEnum
						.getCode(CCRequestTypeEnum.TERMINATION_REQUEST)) {
					legData.set(LegDataAttributes.P_LEG_DIAMETER_STATE,
							State.DIAMETER_REQUEST_COMPLETED);
				}
			} catch (RoResourceException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}

			callData.set(CallDataAttribute.P_LEG2, legData);
			
			try {
				legData.set(LegDataAttributes.DIAMETER_IN_CCA_REQ_TYPE, cca.getCCRequestType());
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		// TODO Auto-generated method stub

	}

}
