package com.agnity.ph.diameter;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.SipProtocolHelper;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.ra.diameter.base.avp.AvpDiameterGrouped;
import com.baypackets.ase.ra.diameter.ro.CreditControlAnswer;
import com.baypackets.ase.ra.diameter.ro.CreditControlRequest;
import com.baypackets.ase.ra.diameter.ro.RoResourceException;
import com.baypackets.ase.ra.diameter.ro.RoResourceFactory;
import com.baypackets.ase.ra.diameter.ro.enums.CCRequestTypeEnum;
import com.baypackets.ase.ra.diameter.ro.enums.RequestedActionEnum;
import com.baypackets.ase.resource.ResourceException;

import fr.marben.diameter.DiameterAVP;
import fr.marben.diameter.DiameterException;
import fr.marben.diameter.DiameterInvalidArgumentException;
import fr.marben.diameter._3gpp.ro.DiameterRoMessageFactory;

public class DiameterProtocolHelper {

	private static Logger logger = Logger
			.getLogger(DiameterProtocolHelper.class);

	/**
	 * This method is used to send Enum response out
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendAnswer(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::sendAnswer ");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		String retrunCode = (String) legData
				.get(LegDataAttributes.DIAMETER_CCA_OUT_RETURN_CODE);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::sendAnswer CCR retrunCode is "
					+ retrunCode);
		}
		
		LegData leg1= (LegData) callData.get(CallDataAttribute.valueOf(CallDataAttribute.P_LEG1.name()));
		
		leg1
		.set(LegDataAttributes.P_LEG_DIAMETER_STATE,State.DIAMETER_RESPONSE_SENT);
		
		CreditControlRequest request = (CreditControlRequest) leg1
				.get(LegDataAttributes.DIAMETER_IN_CCR_REQ);

		CreditControlAnswer answer = request.createAnswer(retrunCode);

		List<DiameterAVPAttribute> avpAttrbutes = (List<DiameterAVPAttribute>) legData
				.get(LegDataAttributes.DIAMETER_RES_AVP_LIST);
		
		DiameterRoMessageFactory roMsgfactory = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID)).getDiameterRoMesageFactory();
		

		if (avpAttrbutes != null) {
			addAVPs(avpAttrbutes, answer,
					(String) callData.get(CallDataAttribute.SERVICE_ID),
					legData,getReturnCode(retrunCode));
		}
		
		
		
		boolean sendterminateCCA=false;
		if(request.getCCRequestType() == CCRequestTypeEnum.getCode(CCRequestTypeEnum.TERMINATION_REQUEST)){
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::send CCA for CCR Terminate ");
			}
			sendterminateCCA=true;
			 leg1
				.set(LegDataAttributes.P_LEG_DIAMETER_STATE,State.DIAMETER_REQUEST_COMPLETED);
		}
		
				
		addAVPSToCCASetInLeg(legData,answer,roMsgfactory,sendterminateCCA,(String) callData.get(CallDataAttribute.SERVICE_ID),getReturnCode(retrunCode));
		answer.send();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::Answer is sent as " + retrunCode);
		}

		try {

			ServiceInterface serviceHandler = PhUtilityServices.getInstance(
					(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();

			Event event = new Event(EventType.EVENT_CCA_SENT,
					Protocol.DIAMETER, action.getLeg());

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);
			
			if(sendterminateCCA){
			    SipProtocolHelper.dropCall(appSession);
			}

		} catch (ResourceException e) {
			logger.error("ResourceException received " + e);
			e.printStackTrace();

		} catch (IOException e) {
			logger.error("IOException received " + e);

			e.printStackTrace();
		}

	}

	/**
	 * Ths method s used to send Diameter Request out
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @param requestType
	 * @throws ResourceException
	 * @throws IOException
	 */
	public static void sendDiameterRequest(SipApplicationSession appSession,
			CallData callData, Action action, int requestType)
			throws ResourceException, IOException {

		String serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		RequestedActionEnum reqAction = (RequestedActionEnum) legData
				.get(LegDataAttributes.DIAMETER_CCR_REQUESTED_ACTION);

		String remoteRealm = (String) legData
				.get(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM);

		if (logger.isDebugEnabled()) {
			logger.debug("::sendDiameterRequest -->Remote Realm is  "
					+ remoteRealm);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("::-->Requested Action is  " + reqAction);
		}

		RoResourceFactory rofactory = PhUtilityServices.getInstance(serviceId)
				.getRoResFactory();

		String sesionId=null;
		if(requestType==CCRequestTypeEnum
				.getCode(CCRequestTypeEnum.UPDATE_REQUEST)|| requestType==CCRequestTypeEnum
						.getCode(CCRequestTypeEnum.TERMINATION_REQUEST)){
			LegData leg1= (LegData) callData.get(CallDataAttribute.valueOf(CallDataAttribute.P_LEG1.name()));
			sesionId = (String) leg1
					.get(LegDataAttributes.DIAMETER_CCR_SESSION_ID);
			if (logger.isDebugEnabled()) {
				logger.debug("Pass Existing Session ID " + sesionId);
			}
		}
		
		boolean terminate = false;
		if (requestType == CCRequestTypeEnum
				.getCode(CCRequestTypeEnum.TERMINATION_REQUEST)) {
			terminate = true;

		}
		CreditControlRequest ccr = rofactory.createRequest(appSession,sesionId,requestType, remoteRealm);
		
		if (reqAction != null) {
			ccr.addRequestedAction(RequestedActionEnum.getCode(reqAction));// RequestedActionEnum.CHECK_BALANCE));
		}

		List<DiameterAVPAttribute> avpAttrbutes = (List<DiameterAVPAttribute>) legData
				.get(LegDataAttributes.DIAMETER_REQ_AVP_LIST);

		if (avpAttrbutes != null) {
			try {
				addAVPs(avpAttrbutes, ccr, serviceId, legData);
			} catch (DiameterInvalidArgumentException e) {
				logger.error("DiameterInvalidArgumentException" + e);
			} catch (DiameterException e) {
				logger.error("DiameterException" + e);
			}
		}

		addAVPSToCCRSetInLeg(legData, ccr, serviceId, terminate);
		ccr.send();
		
		 legData
			.set(LegDataAttributes.P_LEG_DIAMETER_STATE,State.DIAMETER_REQUEST_SENT);
	}

	/**
	 * This methd is used to add AVPs to CCR
	 * 
	 * @param avpAttrbutes
	 * @param ccr
	 * @throws DiameterException
	 * @throws DiameterInvalidArgumentException
	 */
	private static void addAVPs(List<DiameterAVPAttribute> avpAttrbutes,
			CreditControlRequest ccr, String serviceId, LegData legData)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::addAVPs set by applcaton to CCR");
		}

		for (DiameterAVPAttribute avp : avpAttrbutes) {

			if(avp==null){
				continue;
			}
			if (logger.isDebugEnabled()) {
				logger.debug("::addAVPs Code " + avp.getAvpCode() + "Name  "
						+ avp.getName() + " Value " + avp.getValue());
			}
			
			if (DiameterAVPNames.Mutil_Service_Credit_Control.equals(avp
					.getName())
					|| DiameterAVPCodes.Mutil_Service_Credit_Control == avp
							.getAvpCode()) {
				

			if (avp.getValue() instanceof List<?>) {
				
				/*
				 * Value should contain list of grouped avp code contained by MSCC
				 */
				List<String> avpCodes= (List<String>)avp.getValue();
//
//					if (logger.isDebugEnabled()) {
//						logger.debug("::addAVP Mutil_Service_Credit_Control");
//					}
//					// ccr.addDiameterAVPs((ArrayList<DiameterAVP>) avp
//					// .getValue());
//					ccr.addDiameterGroupedAVP(avp.getName(),
//							DiameterConstants.VENDOR_NAME_BASE,
//							(ArrayList<DiameterAVP>) avp.getValue());
//				} else {

					if (logger.isDebugEnabled()) {
						logger.debug("::addAVP Mutil_Service_Credit_Control by reading individual attributes");
					}
					List<DiameterAVP> msccAvpist = DiameterGroupedAVPBuilder
							.createMultiServiceCreditControlToCCR(avpCodes,serviceId,
									legData);
					ccr.addDiameterGroupedAVP(avp.getName(),
							DiameterConstants.VENDOR_NAME_BASE,
							msccAvpist);
				}
				
				continue;
			}
			
			//Code added by OCS team
			
//			if (DiameterAVPNames.Subscription_Id.equals(avp
//					.getName())
//					|| DiameterAVPCodes.Subscription_Id == avp
//							.getAvpCode()) {
//				
//
//			if (avp.getValue() instanceof List<?>) {
//				
//				/*
//				 * Value should contain list of grouped avp code contained by MSCC
//				 */
//				List<String> avpCodes= (List<String>)avp.getValue();
//
//					if (logger.isDebugEnabled()) {
//						logger.debug("::addAVP Subscription_Id by reading individual attributes");
//					}
//					List<DiameterAVP> subscriptionIdAvpist = DiameterGroupedAVPBuilder
//							.createSubscriptionIdToCCR(avpCodes,serviceId,
//									legData);
//					ccr.addDiameterGroupedAVP(avp.getName(),
//							DiameterConstants.VENDOR_NAME_BASE,
//							subscriptionIdAvpist);
//				}
//				continue;
//			}
			if (DiameterAVPNames.Destination_Host.equals(avp
					.getName())) {
				
				String destHost=(String) avp.getValue();
				if(destHost==null){
					destHost = (String) legData
						.get(LegDataAttributes.DIAMETER_DEST_HOST);
				}

				if (logger.isDebugEnabled()) {
					logger.debug("::Dest Host : " +destHost);
				}
				ccr.setDestinationHost(destHost);
					
					continue;
				}
			
			if (DiameterAVPNames.Service_Context_Id.equals(avp
					.getName())) {
				
				String ctxId=(String) avp.getValue();
				if(ctxId==null){
					ctxId = (String) legData
						.get(LegDataAttributes.DIAMETER_SERVICE_CONTEXT_ID);
				}

				if (logger.isDebugEnabled()) {
					logger.debug("::Dest Host : " +ctxId);
				}
					ccr.addDiameterOctetStringAVP(DiameterAVPNames.Service_Context_Id, DiameterConstants.VENDOR_NAME_BASE,ctxId);
					
					continue;
			}
			
			if (DiameterAVPNames.User_Name.equals(avp
					.getName())) {
				
				String ctxId=(String) avp.getValue();
				if(ctxId==null){
					ctxId = (String) legData
						.get(LegDataAttributes.DIAMETER_USER_NAME);
				}

				if (logger.isDebugEnabled()) {
					logger.debug("::User_Name : " +ctxId);
				}
					ccr.addDiameterOctetStringAVP(DiameterAVPNames.User_Name, DiameterConstants.VENDOR_NAME_BASE,ctxId);
					
					continue;
			}
			if (DiameterAVPNames.Origin_State_Id.equals(avp
					.getName())) {
				
				String stateid=(String) avp.getValue();
				if(stateid==null){
					stateid = (String) legData
						.get(LegDataAttributes.DIAMETER_ORIG_STATE_ID);
				}

				if (logger.isDebugEnabled()) {
					logger.debug("::Service_Context_Id : " +stateid);
				}
					ccr.addDiameterOctetStringAVP(DiameterAVPNames.Origin_State_Id,DiameterConstants.VENDOR_NAME_BASE,stateid);
					
					continue;
			}
			
//			if (DiameterAVPNames.Service_Information.equals(avp
//					.getName())
//					|| DiameterAVPCodes.Service_Information == avp
//							.getAvpCode()) {
//			if (avp.getValue() instanceof List<?>) {
//				
//				/*
//				 * Value should contain list of grouped avp code contained by MSCC
//				 */
//				List<String> avpCodes= (List<String>)avp.getValue();
//
//					if (logger.isDebugEnabled()) {
//						logger.debug("::addAVP Service_Information by reading individual attributes");
//					}
//					List<DiameterAVP> serviceInfoAvpList = DiameterGroupedAVPBuilder
//							.createServiceInformationToCCR(avpCodes, serviceId, legData);
//					ccr.addDiameterGroupedAVP(avp.getName(),
//							DiameterConstants.VENDOR_NAME_3GPP,
//							serviceInfoAvpList);
//				}
//				
//				continue;
//			}
			
			// code end

			switch (avp.getType()) {
			case Integer32: {
				ccr.addDiameterInteger32AVP(avp.getName(),
						(Integer)avp.getValue(), avp.getVendorName());
				break;
			}
			case Integer64: {
				ccr.addDiameterInteger64AVP(avp.getName(),
						(Long) avp.getValue(), avp.getVendorName());
				break;
			}
			case Float32: {
				ccr.addDiameterFloat32AVP(avp.getName(), avp.getVendorName(),
						(Float) avp.getValue());
				break;
			}
			case Float64: {
				ccr.addDiameterFloat64AVP(avp.getName(), avp.getVendorName(),
						(Double) avp.getValue());
				break;
			}
			case Unsigned32: {
				ccr.addDiameterUnsigned32AVP(avp.getName(),
						(Long) avp.getValue(), avp.getVendorName());
				break;
			}
			case Unsigned64: {
				ccr.addDiameterUnsigned64AVP(avp.getName(),
						(BigInteger) avp.getValue(), avp.getVendorName());
				break;
			}
			case OctetString: {
				ccr.addDiameterOctetStringAVP(avp.getName(),
						avp.getVendorName(), (String) avp.getValue());
				break;
			}
			case OctetString_byte: {
				ccr.addDiameterOctetStringAVP(avp.getName(),
						avp.getVendorName(), (byte[]) avp.getValue());
				break;
			}
			case Generic: {
				ccr.addDiameterGenericAVP(avp.getAvpCode(), avp.getVendorid(),
						(byte[]) avp.getValue());
				break;
			}
			case Grouped: {
                  {

					if (logger.isDebugEnabled()) {
						logger.debug("::addDiameterGroupedAVP");
					}
					AvpDiameterGrouped groupedAvp = ccr.addDiameterGroupedAVP(
							avp.getName(), avp.getVendorName());
					addAvpsToGrouped(
							(List<DiameterAVPAttribute>) avp.getValue(),
							groupedAvp);
				}
				break;

			}
			}

		}
	}

	/**
	 * This method is used to add avps to a grouped avp
	 * 
	 * @param avpAttrbutes
	 * @param groupedAvp
	 */
	private static void addAvpsToGrouped(
			List<DiameterAVPAttribute> avpAttrbutes,
			AvpDiameterGrouped groupedAvp) {

		if (logger.isDebugEnabled()) {
			logger.debug("::addAvpsToGrouped : "+ avpAttrbutes +" To Grouped AVP "+ groupedAvp);
		}
		
		for (DiameterAVPAttribute avp : avpAttrbutes) {

			if (logger.isDebugEnabled()) {
				logger.debug("::addAVP---> "+ avp.getName() + " value "+avp.getValue() );
			}
			
			switch (avp.getType()) {
			case Integer32: {
				groupedAvp.addDiameterInteger32AVP(avp.getName(),
						(Long) avp.getValue());
				break;
			}
			case Integer64: {
				groupedAvp.addDiameterInteger64AVP(avp.getName(),
						(Long) avp.getValue());
				break;
			}
			case Float32: {
				groupedAvp.addDiameterFloat32AVP(avp.getName(),
						(Float) avp.getValue());
				break;
			}
			case Float64: {
				groupedAvp.addDiameterFloat64AVP(avp.getName(),
						(Double) avp.getValue());
				break;
			}
			case Unsigned32: {
				groupedAvp.addDiameterUnsigned32AVP(avp.getName(),
						(Long) avp.getValue());
				break;
			}
			case Unsigned64: {
				groupedAvp.addDiameterUnsigned64AVP(avp.getName(),
						(BigInteger) avp.getValue());
				break;
			}
			case OctetString: {
				groupedAvp.addDiameterOctetStringAVP(avp.getName(),
						(String) avp.getValue());
				break;
			}
			case OctetString_byte: {
				groupedAvp.addDiameterOctetStringAVP(avp.getName(),
						(byte[]) avp.getValue());
				break;
			}
			case Generic: {
				groupedAvp.addDiameterGenericAVP(avp.getAvpCode(),
						avp.getVendorid(), (byte[]) avp.getValue());
				break;
			}

			}
		}
	}

	/**
	 * This method is used to add aVPS to CCA
	 * 
	 * @param avpAttrbutes
	 * @param cca
	 * @param resultCode 
	 * @throws DiameterException
	 * @throws DiameterInvalidArgumentException
	 */
	private static void addAVPs(List<DiameterAVPAttribute> avpAttrbutes,
			CreditControlAnswer cca, String serviceId, LegData legData, long resultCode)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::addAVPs set by applcaton to CCA");
		}
		
		DiameterRoMessageFactory roMsgfactory = PhUtilityServices.getInstance(
				serviceId).getDiameterRoMesageFactory();

		for (DiameterAVPAttribute avp : avpAttrbutes) {

			if (logger.isDebugEnabled()) {
				logger.debug("::addAVPs ..Code " + avp.getAvpCode() + " name "
						+ avp.getName() + " Value " + avp.getValue());
			}
			if(avp==null){
				continue;
			}

			if (avp.getName() == DiameterAVPNames.Direct_Debiting_Failure_Handling
					|| DiameterAVPCodes.Direct_Debiting_Failure_Handling == avp
					.getAvpCode()) {

				if (logger.isDebugEnabled()) {
					logger.debug("::addAVPs Direct_Debiting_Failure_Handling to CCA "
							+ avp.getValue());
				}
				
				String handling = (String) avp.getValue();

				if (handling == null) {
					handling = (String) legData
							.get(LegDataAttributes.DIAMETER_DEBIT_CTRL_FAILURE_HANDLING);

				}

				try {
					cca.addDirectDebitingFailureHandling(handling);
				} catch (RoResourceException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

				continue;

			}

			if (avp.getName() == DiameterAVPNames.Credit_Control_Failure_Handling
					|| DiameterAVPCodes.Credit_Control_Failure_Handling == avp
					.getAvpCode()) {
				if (logger.isDebugEnabled()) {
					logger.debug("::addAVPs Credit_Control_Failure_Handling to CCA "
							+ avp.getValue());
				}
				
				String handling = (String) avp.getValue();

				if (handling == null) {
					handling = (String) legData
							.get(LegDataAttributes.DIAMETER_CREDIT_CTRL_FAILURE_HANDLING);

				}
					try {
						cca.addCreditControlFailureHandling(handling);
					} catch (RoResourceException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					continue;
			}
			
			if (avp.getName() == DiameterAVPNames.CC_Session_Failover
					|| DiameterAVPCodes.CC_Session_Failover == avp
					.getAvpCode()) {
				if (logger.isDebugEnabled()) {
					logger.debug("::addAVPs CC_Session_Failover to CCA "
							+ avp.getValue());
				}
				
				String failover = (String) avp.getValue();

				if (failover == null) {
					failover = (String) legData
							.get(LegDataAttributes.DIAMETER_CC_SESSION_FAILOVER);
				}
				
				int sessionFail=-1;
				if(failover!=null){
					sessionFail=Integer.parseInt(failover);
				
					try {
						cca.addCCSessionFailover(sessionFail);
					} catch (RoResourceException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					continue;
				}
			}
			
			if (DiameterAVPNames.Service_Information.equals(avp
					.getName())
					|| DiameterAVPCodes.Service_Information == avp
							.getAvpCode()) {
			if (avp.getValue() instanceof List<?>) {
				
				/*
				 * Value should contain list of grouped avp code contained by MSCC
				 */
				List<String> avpCodes= (List<String>)avp.getValue();

					if (logger.isDebugEnabled()) {
						logger.debug("::addAVP Service_Information by reading individual attributes");
					}
					List<DiameterAVP> serviceInfoAvpList = DiameterGroupedAVPBuilder
							.createServiceInformationToCCA(avpCodes, roMsgfactory, legData);
					cca.addDiameterGroupedAVP(avp.getName(),
							DiameterConstants.VENDOR_NAME_3GPP,
							serviceInfoAvpList);
				}
				
				continue;
			}
			
			
			if (DiameterAVPNames.Mutil_Service_Credit_Control.equals(avp
					.getName())
					|| DiameterAVPCodes.Mutil_Service_Credit_Control == avp
							.getAvpCode()) {

				if (logger.isDebugEnabled()) {
					logger.debug("::addAVP Mutil_Service_Credit_Control will be added by set in attributes");
					
//				if (avp.getValue() instanceof List<?>) {
//					
//					List<String> avpCodes= (List<String>)avp.getValue();
//
//					if (logger.isDebugEnabled()) {
//						logger.debug("::addAVP Mutil_Service_Credit_Control by reading individual attributes");
//					}
//					List<DiameterAVP> msccAvpist = DiameterGroupedAVPBuilder
//							.createMultiServiceCreditControlToCCA(avpCodes,serviceId,
//									legData,resultCode);
//					// ccr.addDiameterAVPs((ArrayList<DiameterAVP>)
//					// msccAvpist);
//					cca.addDiameterGroupedAVP(avp.getName(),
//							DiameterConstants.VENDOR_NAME_BASE,
//							msccAvpist);
			}
				
				continue;
			} 
			
			if (DiameterAVPNames.Cost_Information.equals(avp
					.getName())
					|| DiameterAVPCodes.Cost_Information == avp
							.getAvpCode()) {

				if (avp.getValue() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("::addAVP Cost_Information");
					}
					// ccr.addDiameterAVPs((ArrayList<DiameterAVP>) avp
					// .getValue());
					cca.addDiameterGroupedAVP(avp.getName(),
							DiameterConstants.VENDOR_NAME_BASE,
							(ArrayList<DiameterAVP>) avp.getValue());
				} else {

					if (logger.isDebugEnabled()) {
						logger.debug("::addAVP Cost_Information by reading individual attributes");
					}
					List<DiameterAVP> msccAvpist = DiameterGroupedAVPBuilder
							.createCostInformation(serviceId, legData);
					// ccr.addDiameterAVPs((ArrayList<DiameterAVP>)
					// msccAvpist);
					cca.addDiameterGroupedAVP(avp.getName(),
							DiameterConstants.VENDOR_NAME_BASE,
							msccAvpist);
				}
				
				continue;
			} 

			switch (avp.getType()) {
			case Integer32: {
				cca.addDiameterInteger32AVP(avp.getName(),
						(Integer) avp.getValue(), avp.getVendorName());
				break;
			}
			case Integer64: {
				cca.addDiameterInteger64AVP(avp.getName(),
						(Long) avp.getValue(), avp.getVendorName());
				break;
			}
			case Float32: {
				cca.addDiameterFloat32AVP(avp.getName(), avp.getVendorName(),
						(Float) avp.getValue());
				break;
			}
			case Float64: {
				cca.addDiameterFloat64AVP(avp.getName(), avp.getVendorName(),
						(Double) avp.getValue());
				break;
			}
			case Unsigned32: {
				cca.addDiameterUnsigned32AVP(avp.getName(),
						(Long) avp.getValue(), avp.getVendorName());
				break;
			}
			case Unsigned64: {
				cca.addDiameterUnsigned64AVP(avp.getName(),
						(BigInteger) avp.getValue(), avp.getVendorName());
				break;
			}
			case OctetString: {
				cca.addDiameterOctetStringAVP(avp.getName(),
						avp.getVendorName(), (String) avp.getValue());
				break;
			}
			case OctetString_byte: {
				cca.addDiameterOctetStringAVP(avp.getName(),
						avp.getVendorName(), (byte[]) avp.getValue());
				break;
			}
			case Generic: {
				cca.addDiameterGenericAVP(avp.getAvpCode(), avp.getVendorid(),
						(byte[]) avp.getValue());
				break;
			}
			case Grouped: {
				{

					if (logger.isDebugEnabled()) {
						logger.debug("::addDiameterGroupedAVP");
					}
					AvpDiameterGrouped groupedAvp = cca.addDiameterGroupedAVP(
							avp.getName(), avp.getVendorName());
					addAvpsToGrouped(
							(List<DiameterAVPAttribute>) avp.getValue(),
							groupedAvp);
				}
				break;

			}
			}

		}
	}
	
	/**
	 * This method is used to add avps if set in leg
	 * @param legData
	 * @param cca
	 * @param sendterminateCCA 
	 */
		private static void addAVPSToCCASetInLeg(LegData legData, CreditControlAnswer cca,
				DiameterRoMessageFactory roMsgfactory, 
				boolean sendterminateCCA,String serviceId,
				long resultCode) {
	
			if (logger.isDebugEnabled()) {
				logger.debug("::addAVPSToCCAIfSetInLeg...");
			}
	
//			String handling = (String) legData
//					.get(LegDataAttributes.DIAMETER_DEBIT_CTRL_FAILURE_HANDLING);
//	
//			if (handling != null) {
//	
//				if (logger.isDebugEnabled()) {
//					logger.debug("::add DIAMETER_DEBIT_CTRL_FAILURE_HANDLING to CCA: ");
//				}
//				try {
//					cca.addDirectDebitingFailureHandling(handling);
//				} catch (RoResourceException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				}
//	
//			}
//	
//			handling = (String) legData
//					.get(LegDataAttributes.DIAMETER_CREDIT_CTRL_FAILURE_HANDLING);
//	
//			if (handling != null) {
//				
//				if (logger.isDebugEnabled()) {
//					logger.debug("::add DIAMETER_CREDIT_CTRL_FAILURE_HANDLING to CCA: ");
//				}
//				try {
//					cca.addCreditControlFailureHandling(handling);
//				} catch (RoResourceException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				}
//			}
			String failover = (String) legData
					.get(LegDataAttributes.DIAMETER_CC_SESSION_FAILOVER);
	
			int sessionFail = -1;
			if (failover != null) {
				sessionFail = Integer.parseInt(failover);

				if (logger.isDebugEnabled()) {
					logger.debug("::add DIAMETER_CC_SESSION_FAILOVER to CCA: ");
				}
				try {
					cca.addCCSessionFailover(sessionFail);
				} catch (RoResourceException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			
			String destHost = (String) legData
					.get(LegDataAttributes.DIAMETER_DEST_HOST);
			if(destHost!=null){
				if (logger.isDebugEnabled()) {
					logger.debug("::Dest Host : " +destHost);
				}
				cca.setDestinationHost(destHost);	
			}
			
	//	if (legData.get(LegDataAttributes.DIAMETER_MSCC_SERVICE_IDENTIFIER) != null) {
			if (logger.isDebugEnabled()) {
				logger.debug("::addAVP Mutil_Service_Credit_Control by reading individual attributes");
			}
			
			/**
			 * check MSCC AVP
			 */
			DiameterAVPAttribute mscc=new DiameterAVPAttribute(DiameterAVPNames.Mutil_Service_Credit_Control, DiameterConstants.VENDOR_NAME_BASE, DiameterAVPType.Grouped);
			List<String> avpcodes=new ArrayList<String>();
			
			if(legData.get(LegDataAttributes.DIAMETER_RSU_CC_TIME)!=null 
					||legData.get(LegDataAttributes.DIAMETER_RSU_CC_INPUT_OCTETS)!=null
					||legData.get(LegDataAttributes.DIAMETER_RSU_CC_OUTPUT_OCTETS)!=null
					||legData.get(LegDataAttributes.DIAMETER_RSU_CC_MONEY_VALUE_DIGITS)!=null
					||legData.get(LegDataAttributes.DIAMETER_RSU_CC_SERVICE_SPECIFIC_UNITS)!=null){
				avpcodes.add(DiameterAVPNames.Requested_Service_Unit);
				
			}
			if(legData.get(LegDataAttributes.DIAMETER_GSU_TARIFF_TIME_CHANGE)!=null
					||legData.get(LegDataAttributes.DIAMETER_GSU_CC_TIME)!=null 
					||legData.get(LegDataAttributes.DIAMETER_GSU_CC_INPUT_OCTETS)!=null
					||legData.get(LegDataAttributes.DIAMETER_GSU_CC_MONEY_VALUE_DIGITS)!=null
					||legData.get(LegDataAttributes.DIAMETER_GSU_CC_TOTAL_OCTETS)!=null){
				 avpcodes.add(DiameterAVPNames.Granted_Service_Unit);			
			}
			
			if(legData.get(LegDataAttributes.DIAMETER_USU_TARIFF_CHANGE_USAGE)!=null
					||legData.get(LegDataAttributes.DIAMETER_USU_CC_TIME)!=null 
					||legData.get(LegDataAttributes.DIAMETER_USU_REPORTING_REASON)!=null
					||legData.get(LegDataAttributes.DIAMETER_USU_CC_INPUT_OCTETS)!=null
					||legData.get(LegDataAttributes.DIAMETER_USU_CC_OUTPUT_OCTETS)!=null
					||legData.get(LegDataAttributes.DIAMETER_USU_CC_TOTAL_OCTETS)!=null
					||legData.get(LegDataAttributes.DIAMETER_USU_CC_SERVICE_SPECIFIC_UNITS)!=null){
				avpcodes.add(DiameterAVPNames.Used_Service_Unit);			
			}
			
			if(legData.get(LegDataAttributes.DIAMETER_FUI_FINAL_UNIT_ACTION)!=null){
					
				avpcodes.add(DiameterAVPNames.Final_Unit_Indication);			
			}
			
			if (!avpcodes.isEmpty()) {
				mscc.setValue(avpcodes);
			}
			List<DiameterAVP> msccAvpist;
			try {
				msccAvpist = DiameterGroupedAVPBuilder
						.createMultiServiceCreditControlToCCA(avpcodes, serviceId,
								legData,resultCode);
				cca.addDiameterGroupedAVP(DiameterAVPNames.Mutil_Service_Credit_Control,
						DiameterConstants.VENDOR_NAME_BASE, msccAvpist);
			} catch (DiameterInvalidArgumentException e) {
				logger.error(" could not create MSCC 1"+ e);
				e.printStackTrace();
			} catch (DiameterException e) {
				logger.error(" could not create MSCC 2"+ e);
				e.printStackTrace();
			}
			// ccr.addDiameterAVPs((ArrayList<DiameterAVP>)
			// msccAvpist);
		

	//	}
	
//			String serviceIdn = (String) legData
//					.get(LegDataAttributes.DIAMETER_SERVICE_IDENTIFIER);
//	
//			int svcId = -1;
//			if (serviceIdn != null) {
//				svcId = Integer.parseInt(serviceIdn);
//	
//				if (logger.isDebugEnabled()) {
//					logger.debug("::add Service_Identifier to CCA: ");
//				}
//				cca.addDiameterInteger32AVP(DiameterAVPNames.Service_Identifier,
//						svcId, DiameterConstants.VENDOR_NAME_BASE);
//			}
	
			/*
			 * Value should contain list of grouped avp code contained by MSCC
			 */
			if (sendterminateCCA) {
				
				if (logger.isDebugEnabled()) {
					logger.debug("::addAVP send AOC for CCA terminate");
				}
				List avpCodes = new ArrayList<String>();
				avpCodes.add(DiameterAVPNames.AoC_Information);
	
				if (logger.isDebugEnabled()) {
					logger.debug("::addAVP ServiceInformation by reading individual attributes");
				}
				List<DiameterAVP> serviceInfoAvpList = null;
				try {
					serviceInfoAvpList = DiameterGroupedAVPBuilder
							.createServiceInformationToCCA(avpCodes, roMsgfactory,
									legData);
					cca.addDiameterGroupedAVP(DiameterAVPNames.Service_Information,
							DiameterConstants.VENDOR_NAME_3GPP, serviceInfoAvpList);
				} catch (DiameterInvalidArgumentException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (DiameterException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			
		//addAnnouncemenetInstructions(legData);
	
		}
		
//	private static void addAnnouncemenetInstructions(LegData legData) {
//		// TODO Auto-generated method stub
//		
//		String annNumber = (String) legData
//				.get(LegDataAttributes.DIAMETER_ANN_INST_ANN_NUMBER);
//		String order = (String) legData
//				.get(LegDataAttributes.DIAMETER_ANN_INST_ANN_ORDER);
//
//		String annType = (String) legData
//				.get(LegDataAttributes.DIAMETER_ANN_INST_ANN_TYPE);
//		
//	if (annNumber != null || order != null || annType != null) {
//
//		if (logger.isDebugEnabled()) {
//			logger.debug("::add Ann instructions to CCA: ");
//		}
//		Long annNum = 0L;
//		if (annNumber != null)
//			annNum = Long.parseLong(annNumber);
//
//		Long annOrd = 0L;
//		if (order != null)
//			annOrd = Long.parseLong(order);
//
//		Long annTy = 0L;
//		if (annType != null)
//			annTy = Long.parseLong(annType);
//
//		List<DiameterAVPAttribute> grpAvps = new ArrayList<DiameterAVPAttribute>();
//
//		DiameterAVPAttribute avp1 = new DiameterAVPAttribute(
//				DiameterAVPNames.Announcement_Number,
//				DiameterConstants.VENDOR_NAME_ERICSSON,
//				DiameterAVPType.Unsigned32);
//		avp1.setValue(annNum);
//
//		DiameterAVPAttribute avp2 = new DiameterAVPAttribute(
//				DiameterAVPNames.Announcement_Order,
//				DiameterConstants.VENDOR_NAME_ERICSSON,
//				DiameterAVPType.Unsigned32);
//		avp2.setValue(annOrd);
//
//		DiameterAVPAttribute avp3 = new DiameterAVPAttribute(
//				DiameterAVPNames.Announcement_Type,
//				DiameterConstants.VENDOR_NAME_ERICSSON,
//				DiameterAVPType.Unsigned32);
//		avp3.setValue(annTy);
//
//		grpAvps.add(avp1);
//		grpAvps.add(avp2);
//		grpAvps.add(avp3);
//
//		AvpDiameterGrouped groupedAvp = cca.addDiameterGroupedAVP(
//				DiameterAVPNames.Announcement_Instructions,
//				DiameterConstants.VENDOR_NAME_ERICSSON);
//		addAvpsToGrouped(grpAvps, groupedAvp);
//		
//		}
//	}

	private static void addAVPSToCCRSetInLeg(LegData legData,
			CreditControlRequest ccr, String serviceId, boolean terminate) {

		if (logger.isDebugEnabled()) {
			logger.debug("::addAVPSToCCRSetInLeg...");
		}

		List<String> avpCodes = new ArrayList<String>();
		avpCodes.add(DiameterAVPNames.Subscription_Id);

		if (logger.isDebugEnabled()) {
			logger.debug("::addAVP Subscription_Id by reading individual attributes");
		}
		List<DiameterAVP> subscriptionIdAvpist = null;
		try {
			subscriptionIdAvpist = DiameterGroupedAVPBuilder
					.createSubscriptionIdToCCR(avpCodes, serviceId, legData);
			ccr.addDiameterGroupedAVP(DiameterAVPNames.Subscription_Id,
					DiameterConstants.VENDOR_NAME_BASE, subscriptionIdAvpist);
		} catch (DiameterInvalidArgumentException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (DiameterException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		String destHost = (String) legData
				.get(LegDataAttributes.DIAMETER_DEST_HOST);
		if(destHost!=null){
			if (logger.isDebugEnabled()) {
				logger.debug("::Dest Host : " +destHost);
			}
			ccr.setDestinationHost(destHost);	
		}

		String ctxId = (String) legData
				.get(LegDataAttributes.DIAMETER_SERVICE_CONTEXT_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("::DIAMETER_SERVICE_CONTEXT_ID: " + ctxId);
		}

		if (ctxId != null) {
			ccr.addDiameterOctetStringAVP(DiameterAVPNames.Service_Context_Id,
					DiameterConstants.VENDOR_NAME_BASE, ctxId);
		}

		String userN = (String) legData
				.get(LegDataAttributes.DIAMETER_USER_NAME);

		if (logger.isDebugEnabled()) {
			logger.debug("::User_Name : " + userN);
		}
		if (userN != null) {
			ccr.addDiameterOctetStringAVP(DiameterAVPNames.User_Name,
					DiameterConstants.VENDOR_NAME_BASE, ctxId);

		}
		
		if (terminate) {
			String termCause = (String) legData
					.get(LegDataAttributes.DIAMETER_TERMINATION_CAUSE);

			if (logger.isDebugEnabled()) {
				logger.debug("::Send Termination Cause : " + termCause);
			}
			if (termCause != null) {
				int terminationCause = Integer.parseInt(termCause);

				ccr.addDiameterInteger32AVP(DiameterAVPNames.Termination_Cause,
						terminationCause, DiameterConstants.VENDOR_NAME_BASE);

			}
		}
		
	
			String eventTimestamp = (String) legData
					.get(LegDataAttributes.DIAMETER_EVENT_TIMESTAMP);

			if (logger.isDebugEnabled()) {
				logger.debug("::Send EventTimestamp : " + eventTimestamp);
			}
			if (eventTimestamp != null) {
			//	long eventTimestampp = Long.parseLong(eventTimestamp);

				ccr.addDiameterOctetStringAVP(DiameterAVPNames.Event_Timestamp,
						 DiameterConstants.VENDOR_NAME_BASE,eventTimestamp);

			}
		

		String stateid = (String) legData
				.get(LegDataAttributes.DIAMETER_ORIG_STATE_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("::DIAMETER_ORIG_STATE_ID : " + stateid);
		}
		if (stateid != null) {
			ccr.addDiameterOctetStringAVP(DiameterAVPNames.Origin_State_Id,
					DiameterConstants.VENDOR_NAME_BASE, stateid);

		}

		/*
		 * Value should contain list of grouped avp code contained by MSCC
		 */
		avpCodes = new ArrayList<String>();
		avpCodes.add(DiameterAVPNames.Ims_Information);
	//	avpCodes.add(DiameterAVPNames.AoC_Information);

		if (logger.isDebugEnabled()) {
			logger.debug("::addAVP ServiceInformation by reading individual attributes with avpcodes "+avpCodes);
		}
		List<DiameterAVP> serviceInfoAvpList = null;
		try {
			serviceInfoAvpList = DiameterGroupedAVPBuilder
					.createServiceInformationToCCR(avpCodes, serviceId, legData);
			ccr.addDiameterGroupedAVP(DiameterAVPNames.Service_Information,
					DiameterConstants.VENDOR_NAME_3GPP, serviceInfoAvpList);
		} catch (DiameterInvalidArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (DiameterException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
	
	/**
	 * @param codeString
	 * @return
	 */
	static public long getReturnCode(String codeString){
		long returnCode = 2001; // SUCCESS
		
		if(StringUtils.isNotBlank(codeString)){
			switch(codeString){
			case "DIAMETER_MULTI_ROUND_AUTH" : returnCode = 1001; break;
			case "DIAMETER_SUCCESS"          : returnCode = 2001; break;
			case "DIAMETER_LIMITED_SUCCESS"  : returnCode = 2002; break;
			case "DIAMETER_COMMAND_UNSUPPORTED": returnCode = 3001; break;
			case "DIAMETER_UNABLE_TO_DELIVER"  : returnCode = 3002; break;
			case "DIAMETER_REALM_NOT_SERVED"   :returnCode =  3003; break;
			case "DIAMETER_TOO_BUSY"           : returnCode = 3004; break;
			case "DIAMETER_LOOP_DETECTED"         : returnCode = 3005; break;
			case "DIAMETER_REDIRECT_INDICATION"    : returnCode = 3006; break;
			case "DIAMETER_APPLICATION_UNSUPPORTED": returnCode = 3007; break;
			case "DIAMETER_INVALID_HDR_BITS"      : returnCode = 3008; break;
			case "DIAMETER_INVALID_AVP_BITS"      : returnCode = 3009; break;
			case "DIAMETER_UNKNOWN_PEER"           : returnCode = 3010; break;
			case "DIAMETER_AUTHENTICATION_REJECTED": returnCode = 4001; break;
			case "DIAMETER_OUT_OF_SPACE"		  : returnCode = 4002; break;
			case "ELECTION_LOST"				  : returnCode = 4003; break;
			case "DIAMETER_CREDIT_LIMIT_REACHED"  : returnCode = 4012; break;
			case "DIAMETER_AVP_UNSUPPORTED"       : returnCode = 5001; break;
			case "DIAMETER_UNKNOWN_SESSION_ID"    : returnCode = 5002; break;
			case "DIAMETER_AUTHORIZATION_REJECTED": returnCode = 5003; break;
			case "DIAMETER_INVALID_AVP_VALUE"	  : returnCode = 5004; break;
			case "DIAMETER_MISSING_AVP"           : returnCode = 5005; break;
			case "DIAMETER_RESOURCES_EXCEEDED"    : returnCode = 5006; break;
			case "DIAMETER_CONTRADICTING_AVPS"    : returnCode = 5007; break;
			case "DIAMETER_AVP_NOT_ALLOWED"       : returnCode = 5008; break;
			case "DIAMETER_AVP_OCCURS_TOO_MANY_TIMES" : returnCode = 5009; break;
			case "DIAMETER_NO_COMMON_APPLICATION" :returnCode =  5010; break;
			case "DIAMETER_UNSUPPORTED_VERSION"   :returnCode =  5011; break;
			case "DIAMETER_UNABLE_TO_COMPLY"      : returnCode = 5012; break;
			case "DIAMETER_INVALID_BIT_IN_HEADER" : returnCode = 5013; break;
			case "DIAMETER_INVALID_AVP_LENGTH"    : returnCode = 5014; break;
			case "DIAMETER_INVALID_MESSAGE_LENGTH": returnCode = 5015; break;
			case "DIAMETER_INVALID_AVP_BIT_COMBO" : returnCode = 5016; break;
			case "DIAMETER_NO_COMMON_SECURITY"    : returnCode = 5017; break;
			case "DIAMETER_END_USER_SERVICE_DENIED" : returnCode=4010;break;
			case "DIAMETER_CREDIT_CONTROL_NOT_APPLICABLE" : returnCode=4011;break;
			case "DIAMETER_USER_UNKNOWN" : returnCode=5030;break;
			case "DIAMETER_RATING_FAILED" : returnCode=5031;break;
			default: returnCode = 2001;
			}
		}
		
		return returnCode;
	}

}
