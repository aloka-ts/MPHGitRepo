package com.agnity.ph.diameter.sh;

import java.text.SimpleDateFormat;
import java.util.Date;

import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.State;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.diameter.DiameterAVPCodes;
import com.agnity.ph.diameter.DiameterAVPNames;
import com.agnity.ph.diameter.DiameterConstants;
import com.agnity.ph.sip.SipProtocolConfig;
import com.baypackets.ase.ra.diameter.base.avp.AvpDiameterGrouped;
import com.baypackets.ase.ra.diameter.sh.ShResourceException;
import com.baypackets.ase.ra.diameter.sh.ShResourceFactory;
import com.baypackets.ase.ra.diameter.sh.ShUserDataRequest;

/**
 * This class is helper class for Smpp ProtocolHandler
 * @author bahul
 *
 */
public class DiameterShProtocolHelper {


	private static Logger logger = Logger.getLogger(DiameterShProtocolHelper.class);

	/**
	 * This method is used to send Dismeter sh query
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 *
	 */
	public static void sendShUserDataRequest(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::send diameter sh user data request");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(action.getLeg()));
		ShResourceFactory resFactory = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID)).getShResourceFactory();
		ServiceInterface serviceHandler = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();
   	try {
   		
			String realm = (String) legData
					.get(LegDataAttributes.MAP_UDR_DESTINATION_REALM);
			if (realm == null || realm.equals("")) {
				realm = SipProtocolConfig
						.getConfigData(SipProtocolConfig.DEFAULT_DIAMETER_SH_DEST_REALM);
			}
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::send diameter sh user data request using realm "+realm);
			}
			
			String msisdn=(String)legData.get(LegDataAttributes.MAP_UDR_MSISDN);
			ShUserDataRequest shUserDataRequest = resFactory
					.createUserDataRequest(appSession, realm,msisdn);
			
			
			int currenLocationEnum= getCurrentLocationEnum((String)legData.get(LegDataAttributes.MAP_UDR_CURRENT_LOCATION));
			
			shUserDataRequest.addDiameterInteger32AVP("Current-Location",  currenLocationEnum,DiameterConstants.VENDOR_NAME_3GPP);
			
			//if((Boolean)legData.get(LegDataAttributes.MAP_UDR_MSISDN)) {
				if (logger.isDebugEnabled()) {
					logger.debug("MAP_UDR_MSISDN is set to true so adding DataReferenceEnum.MSISDN in the request ");
				}
				//shUserDataRequest.addDiameterInteger32AVP("Data-Reference" , 14, DiameterConstants.VENDOR_NAME_3GPP); //shUserDataRequest.addDataReference(DataReferenceEnum.MSISDN);
		//	}
			
			//shUserDataRequest.addCurrentLocation(currenLocationEnum);
			
			int reqdomain=getRequestedDomain((String)legData.get(LegDataAttributes.MAP_UDR_DOMAIN_TYPE));
			
			shUserDataRequest.addDiameterInteger32AVP("Requested-Domain", reqdomain, DiameterConstants.VENDOR_NAME_3GPP); 
			
			if (logger.isDebugEnabled()) {
				logger.debug("set Vendor_Specific_Application_Id");
			}
			
			AvpDiameterGrouped grouped=shUserDataRequest.addDiameterGroupedAVP(DiameterAVPNames.Vendor_Specific_Application_Id, DiameterConstants.VENDOR_NAME_BASE);
			
			grouped.addDiameterUnsigned32AVP(DiameterAVPNames.Vendor_id, DiameterConstants.VENDOR_ID_3GPP);
			grouped.addDiameterUnsigned32AVP(DiameterAVPNames.Auth_Application_Id, DiameterConstants.AUTH_APPLICATION_ID_SH);
			shUserDataRequest.addDiameterUnsigned32AVP("Requested-Nodes", 1, DiameterConstants.VENDOR_NAME_3GPP);
			
			if (logger.isDebugEnabled()) {
				logger.debug("send SH UDR request  ");
			}
			shUserDataRequest.send();
			
            callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID, shUserDataRequest.getSessionId());
			
			LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
			
			 SimpleDateFormat sdfDate = new SimpleDateFormat("yyyyMMddHHmmssSSS");//dd/MM/yyyy
			    Date now = new Date();
			    String strDate = sdfDate.format(now);
			leg1Data.set(LegDataAttributes.DIAMETER_UDR_SENT_TIMESTAMP,strDate);
			legData
			.set(LegDataAttributes.P_LEG_DIAMETER_STATE,State.DIAMETER_REQUEST_SENT);
			
//			logger.error("SH_UDR:"+ shUserDataRequest
//					.getApplicationSession().getId()+","+callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID)+","+strDate);
			
		} catch (Exception e) {

			logger.error("Diameter sh exception received raising failuyre event " +e);
			Event event = new Event(EventType.EVENT_SH_SEND_USER_REQUEST_FAILURE,
					Protocol.DIAMETER_SH, action.getLeg());

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);		
			}
	}
	
	private static int getCurrentLocationEnum(String currentLocation) {
		if (PhConstants.FALSE.equalsIgnoreCase(currentLocation)) {// currentLocation
																	// ==
																	// "DoNotNeedInitiateActiveLocationRetrieval")
																	// {
			return 0;// CurrentLocationEnum.DoNotNeedInitiateActiveLocationRetrieval;
		}
		return 1;// CurrentLocationEnum.InitiateActiveLocationRetrieval;
	}
	
	/**
	 * for sh domain will always by PS
	 * @param domainType
	 * @return
	 * @throws Exception
	 */
    private static int getRequestedDomain(String domainType) throws Exception {
//    	if(StringUtils.equalsIgnoreCase(domainType,"CSDomain")) {
//    		if (logger.isDebugEnabled()) {
//				logger.debug("CSDomain is added as domainType in the sh user data request ");
//			}
//    		return 0;//RequestedDomainEnum.CSDomain;
//    	}
//    	else if (StringUtils.equalsIgnoreCase(domainType,"EPSDomain")) {
//    		if (logger.isDebugEnabled()) {
//				logger.debug("EPSDomain is added as domainType in the sh user data request ");
//			}
//    		return RequestedDomainEnum.EPSDomain;
//    	}
    	 if(StringUtils.equalsIgnoreCase(domainType,"PSDomain")){
    		if (logger.isDebugEnabled()) {
				logger.debug("PSDomain is added as domainType in the sh user data request ");
			}
    		return 1;//RequestedDomainEnum.PSDomain;
    	}
    	else {
    		
    		return 1;
		//	logger.error("Invalid value of domainType found in request throwing exception for value :- "+domainType);
    	//	throw new Exception("Invalid Domain type for User data sh request");
    	}
    }
}
