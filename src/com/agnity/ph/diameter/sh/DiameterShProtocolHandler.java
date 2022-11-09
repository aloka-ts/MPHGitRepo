package com.agnity.ph.diameter.sh;

import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.State;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolHandler;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.common.Registry;
import com.baypackets.ase.container.AseApplicationSession;
import com.baypackets.ase.container.AseContext;
import com.baypackets.ase.container.AseHost;
import com.baypackets.ase.ra.diameter.sh.ShResourceException;
import com.baypackets.ase.ra.diameter.sh.ShResponse;
import com.baypackets.ase.resource.Message;
import com.baypackets.ase.util.Constants;
import com.genband.tcap.provider.TcapSession;

import fr.marben.diameter.DiameterAVP;
import fr.marben.diameter.DiameterOctetStringAVP;

public class DiameterShProtocolHandler implements ProtocolHandler {

	private static final DiameterShProtocolHandler INSTANCE = new DiameterShProtocolHandler();
	private static Logger logger = Logger.getLogger(DiameterShProtocolHandler.class);

	private DiameterShProtocolHandler() {
	}

	@Override
	public void executeAction(CallData callData, Action action) throws Exception {
		// TODO Auto-generated method stub

		if (logger.isDebugEnabled()) {
			logger.debug("::executeAction:" + action.getActionType().name());
		}
		String serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		
		Object dialogIdObj = (Object) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug( "[PH]:: executeAction() Enter with callData and action " +dialogIdObj);
		}
		SipApplicationSession appSession=null;
		if (dialogIdObj != null) {
			
			int dialogId=(Integer)dialogIdObj;
			
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + "[PH]:: executeAction() Enter with callData and getappsession from tcapsession ");
			}
			TcapSession tcapSession = PhUtilityServices
					.getInstance(
							(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getTcapProvider().getTcapSession(dialogId);

			appSession = SipProtocolUtil.getAppSession(tcapSession);
			AseHost Host= (AseHost) Registry.lookup(Constants.NAME_HOST);
			Iterator contexts =Host.findContextByNamePrefix(serviceId);
			AseContext actxt=null;
			while (contexts!=null && contexts.hasNext()) {
				 actxt = (AseContext) contexts.next();

				if (logger.isDebugEnabled()) {
					logger.debug(" Asecontext is: " + actxt);
				}
				
				break;
			}
			if (actxt != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(" Update Asecontext for Diameter : " + actxt);
				}
				((AseApplicationSession) appSession).setContext(actxt);

				actxt.addApplicationSession((AseApplicationSession) appSession);
			}
			
		} else {

			appSession = SipProtocolUtil.getAppSession(
					(String) callData.get(CallDataAttribute.P_APP_SESSION_ID),
					serviceId);
		}

		switch (action.getActionType()) {

		case ACTION_SH_SEND_USER_DATA_REQUEST: {
          DiameterShProtocolHelper.sendShUserDataRequest(appSession, callData, action);
			break;
		}
		case ACTION_START_TIMER: {
	          DiameterShProtocolUtil.startApplicationTimer(appSession, callData, action);
				break;
			}
		case ACTION_STOP_TIMER: {
	          DiameterShProtocolUtil.stopApplicationTimer(appSession, callData, action);
				break;
			}

		default: {
			if (logger.isDebugEnabled()) {
				logger.debug("::executeAction: unexpected action");
			}
		}
			break;

		}
	}

	@Override
	public void timeout(ServletTimer timer) {
		// TODO Auto-generated method stub

	}

	public static ProtocolHandler getInstance() {
		return INSTANCE;
	}

	/**
	 * This method is used to handle diameter sh responses
	 * 
	 * @param message
	 * @param callData
	 * @param serviceHandler
	 */
	public void handleResponse(Message message, CallData callData, ServiceInterface serviceHandler) {

		if (logger.isDebugEnabled()) {
			logger.debug("::DiameterSh :- handleResponse...");
		}

		if (message instanceof ShResponse) {

			
			ShResponse shRes = (ShResponse) message;
			
			if (callData.get(CallDataAttribute.P_APP_SESSION_ID) == null) {
				callData.set(CallDataAttribute.P_APP_SESSION_ID, shRes
						.getApplicationSession().getId());
			}
			
			if (logger.isDebugEnabled()) {
				try {
					logger.debug("-------resultcode is ------" +shRes.getResultCode(false));
				} catch (ShResourceException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}//+cca.g);
			}
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			
			legData
			.set(LegDataAttributes.P_LEG_DIAMETER_STATE,State.DIAMETER_REQUEST_COMPLETED);
			
			 SimpleDateFormat sdfDate = new SimpleDateFormat("yyyyMMddHHmmssSSS");//dd/MM/yyyy
			    Date now = new Date();
			    String strDate = sdfDate.format(now);
			legData
			.set(LegDataAttributes.DIAMETER_UDA_RECEIVED_TIMESTAMP,strDate);
			callData
			.set(CallDataAttribute.P_CALL_DISCONNECT_TIME,new Date());
			
			 String sent= (String) legData.get(LegDataAttributes.DIAMETER_UDR_SENT_TIMESTAMP);
			  
			  long sent1= Long.parseLong(sent);
			  long rec=Long.parseLong(strDate);
			
//			logger.error("SH_UDA:"+ shRes
//					.getApplicationSession().getId()+","+callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID)+","+strDate+" ,"+(rec-sent1));
			
			fetchUserDataFromShResponse(shRes,legData);
			Event event = new Event(EventType.EVENT_SH_USER_DATA_RESPONSE_RECEIVED, Protocol.DIAMETER_SH,
					CallDataAttribute.P_LEG1.name());

			if (legData == null) {
				legData = new LegData();
				if (logger.isDebugEnabled()) {
					logger.debug("::create Leg2ata..");
				}
			}
         
			try {
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			DiameterShProtocolUtil.writeServiceCdr(callData, null);

		}

	}
	
	/**
	 *  fetch User data from sh response
	 * @param shResponse
	 * @param legdata
	 */
	private void fetchUserDataFromShResponse(ShResponse shResponse,LegData legdata){
		
		if (logger.isDebugEnabled()) {
			logger.debug("::fetchUserDataFromShResponse ..");
		}
		
		// LTE NETWORK
//		NP_UDA_EUTRAN_CELL_GLOBAL_ID(NON_PERSISTABLE), 
//		NP_UDA_TRACKING_AREA_ID(NON_PERSISTABLE),
//		NP_UDA_MME_NAME(NON_PERSISTABLE), 
//		NP_UDA_AGE_OF_LOCATION_INFORMATION(NON_PERSISTABLE),
//		NP_UDA_RESULT_CODE(NON_PERSISTABLE),
		
		try {
			boolean isExperimental=false;
			String responseCode=shResponse.getResultCode(isExperimental);
			int returnCode=ShResultCodes.getReturnCode(responseCode,false);
			
			
			String expResponseCode=shResponse.getResultCode(true);
			int extReturnCode=ShResultCodes.getReturnCode(expResponseCode,true);
			
			if (logger.isDebugEnabled()) {
				logger.debug("::ExperimentalResultCode is..>" +extReturnCode);
			}
			
			if (logger.isDebugEnabled()) {
				logger.debug("::ExperimentalResultCode  desc is..>" +expResponseCode);
			}
			
			//String userData=shResponse.getUserData();
			List<DiameterAVP> avpList=shResponse.getAvp(702);
			
			if(avpList!=null && avpList.isEmpty()){
				
				if (logger.isDebugEnabled()) {
					logger.debug("::fetchUserDataFromShResponse ..avpList is empty ");
				}
				legdata.set(LegDataAttributes.NP_UDA_RESULT_CODE, returnCode);
				legdata.set(LegDataAttributes.NP_UDA_RESULT_DESC, responseCode);
				legdata.set(LegDataAttributes.NP_UDA_EXP_RESULT_CODE, extReturnCode);
				legdata.set(LegDataAttributes.NP_UDA_EXP_RESULT_DESC, expResponseCode);
				return;
			}
			
			DiameterAVP userdata=shResponse.getAvp(702).get(0);
			
			
			DiameterOctetStringAVP rsuAvpTime = (DiameterOctetStringAVP) userdata;
			byte[] time = rsuAvpTime.getValue();
			String userdataStr = new String(time, StandardCharsets.US_ASCII);
			
			if (logger.isDebugEnabled()) {
				logger.debug("::userdata .."+ userdataStr);
			}
			

			if (logger.isDebugEnabled()) {
				logger.debug("::ReturnCode .. .."+ returnCode);
			}
			
			legdata.set(LegDataAttributes.NP_UDA_RESULT_CODE, returnCode);
			legdata.set(LegDataAttributes.NP_UDA_RESULT_DESC, responseCode);
			
			DiameterShUserDataParser.parseUserData(userdataStr,legdata);
			
			
		} catch (ShResourceException e) {
			logger.error("ShResourceException "+e);
		}
		
		
		
	}
	
	
	
//	<?xml version="1.0" encoding="UTF-8"?>
//	<Sh-Data>
//	   <Extension>
//	      <Extension>
//	         <Extension>
//	            <Extension>
//	               <EPSLocationInformation>
//	                  <E-UTRANCellGlobalId>AwIWCG1nFg==</E-UTRANCellGlobalId>
//	                  <TrackingAreaId>AwIW4pM=</TrackingAreaId>
//	                  <MMEName>04.mmecda.mmegidac0.mme.epc.mnc610.mcc302.3gppnetwork.org</MMEName>
//	                  <AgeOfLocationInformation>0</AgeOfLocationInformation>
//	                  <Extension>
//	                     <VisitedPLMNID>302610</VisitedPLMNID>
//	                  </Extension>
//	               </EPSLocationInformation>
//	            </Extension>
//	         </Extension>
//	      </Extension>
//	   </Extension>
//	</Sh-Data>
}
