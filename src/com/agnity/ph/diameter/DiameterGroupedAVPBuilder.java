package com.agnity.ph.diameter;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.LegData;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.baypackets.ase.ra.diameter.base.avp.AvpDiameterGrouped;

import fr.marben.diameter.DiameterAVP;
import fr.marben.diameter.DiameterException;
import fr.marben.diameter.DiameterGroupedAVP;
import fr.marben.diameter.DiameterInvalidArgumentException;
import fr.marben.diameter.DiameterMessageFactory;
import fr.marben.diameter.DiameterUnsigned32AVP;
import fr.marben.diameter._3gpp.ro.DiameterRoMessageFactory;

public class DiameterGroupedAVPBuilder {

	private static Logger logger = Logger
			.getLogger(DiameterGroupedAVPBuilder.class);

	// <!-- Ref = [3GPP - TS32.299] 7.2.12aB -->
	// <avp name="Announcement-Information" code="3904" mandatory="must"
	// may-encrypt="no" vendor-specific="yes" >
	// <grouped>
	// <gavp name="Announcement-Identifier"/>
	// <gavp name="Variable-Part"/>
	// <gavp name="Time-Indicator"/>
	// <gavp name="Quota-Indicator"/>
	// <gavp name="Announcement-Order"/>
	// <gavp name="Play-Alternative"/>
	// <gavp name="Privacy-Indicator"/>
	// <gavp name="Language"/>
	// </grouped>
	// </avp>

	/**
	 * This method is sued to create Ann information AVP
	 * 
	 * @param serviceId
	 * @param legData
	 * @return
	 * @throws DiameterInvalidArgumentException
	 * @throws DiameterException
	 */
	public static List<DiameterAVP> createAnnouncementInfrmationAVP(
			String serviceId, LegData legData)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createAnnouncementInfrmationAVP:");
		}

		DiameterRoMessageFactory roMsgFactory = PhUtilityServices.getInstance(
				serviceId).getDiameterRoMesageFactory();
		/**
		 * createAnnouncementInformationAVP( java.lang.Long
		 * announcementIdentifier, java.util.List<DiameterAVP>[] variablePart,
		 * java.lang.Long timeIndicator, java.lang.String quotaIndicator,
		 * java.lang.Long announcementOrder, java.lang.String playAlternative,
		 * java.lang.String privacyIndicator, java.lang.String language) Create
		 * a Announcement-Information grouped AVP
		 */
		String annIdenStr = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_IDENTIFIER);

		Long annInd = null;
		if (annIdenStr != null) {
			annInd = Long.parseLong(annIdenStr);
		}
		String annTimeIndicatorStr = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_TIME_INDICATOR);

		Long annTimeInd = null;
		if (annTimeIndicatorStr != null) {
			annTimeInd = Long.parseLong(annTimeIndicatorStr);
		}
		String annQuotaIndStr = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_QUOTA_INDICATOR);
		String annOrderStr = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_ORDER);

		Long annOrder = null;
		if (annOrderStr != null) {
			annOrder = Long.parseLong(annOrderStr);
		}

		String annPlyAterStr = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_PLAY_ALTERNATIVE);
		String annPrivIndStr = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_PRIVACY_INDICATOR);
		String annPlyLangStr = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_PLANGUAGE);

		String annVarPartOrdr = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_VAR_PART_ORDER);
		String annVarPartType = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_VAR_PART_TYPE);
		String annVarPartVal = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_VAR_PART_VALUE);

		Long annVarPrtOrder = null;
		if (annVarPartOrdr != null) {
			annVarPrtOrder = Long.parseLong(annVarPartOrdr);
		}
		Long annVarPrtType = null;
		if (annVarPartType != null) {
			annVarPrtType = Long.parseLong(annVarPartType);
		}
		/*
		 * <gavp name="Variable-Part-Order"/> <gavp name="Variable-Part-Type"/>
		 * <gavp name="Variable-Part-Value"/>
		 */
		List<DiameterAVP> variablePart = roMsgFactory.createVariablePartAVP(
				annVarPrtOrder, annVarPrtType, annVarPartVal);

		List<DiameterAVP>[] vparrray = new List[1];
		vparrray[0] = variablePart;
		return roMsgFactory.createAnnouncementInformationAVP(annInd, vparrray,
				annTimeInd, annQuotaIndStr, annOrder, annPlyAterStr,
				annPrivIndStr, annPlyLangStr);

	}

	/**
	 * This method is sued to create MSCC AVP
	 * 
	 * @param avpCodes
	 * 
	 * @param serviceId
	 * @param legData
	 * @throws DiameterInvalidArgumentException
	 * @throws DiameterException
	 */
	public static List<DiameterAVP> createMultiServiceCreditControlToCCR(
			List<String> avpCodes, String serviceId, LegData legData)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createMultiServiceCreditControlToCCR: addAvps" +avpCodes);
		}

		DiameterRoMessageFactory roMsgfactory = PhUtilityServices.getInstance(
				serviceId).getDiameterRoMesageFactory();

		if (logger.isDebugEnabled()) {
			logger.debug("::createMultiServiceCreditControlToCCR: roMsgfactory " +roMsgfactory);
		}
		/**
		 * create Requested Service Unit AVP
		 */
		List<DiameterAVP> reqSvcUnitAVP = null;
		if (avpCodes.contains(DiameterAVPNames.Requested_Service_Unit)) {
			reqSvcUnitAVP = createRequestedServiceUnitAVP(roMsgfactory,
					serviceId, legData);
			if (logger.isDebugEnabled()) {
				logger.debug("::Requested_Service_Unit created : "+reqSvcUnitAVP);
			}
		}

		/**
		 * Create granted Service Unit
		 */
		List<DiameterAVP> reqGrantedSVCUnitAVP = null;
		if (avpCodes.contains(DiameterAVPNames.Granted_Service_Unit)) {
			reqGrantedSVCUnitAVP = createGrantedServiceUnitAVP(roMsgfactory,
					serviceId, legData);
			if (logger.isDebugEnabled()) {
				logger.debug("::Granted_Service_Unit created : "+reqGrantedSVCUnitAVP);
			}
		}

		/**
		 * create Used Service Unit AVP
		 */
		List<DiameterAVP>[] usuarrray=null;
		if (avpCodes.contains(DiameterAVPNames.Used_Service_Unit)) {
			List<DiameterAVP> usedSVCUnitAVP = createUsedServiceUnitAVP(roMsgfactory, serviceId,
					legData);
		    usuarrray = new List[1];
			usuarrray[0] = usedSVCUnitAVP;
			
			if (logger.isDebugEnabled()) {
				logger.debug("::Used_Service_Unit created : "+usedSVCUnitAVP);
			}
		}

		String serviceIdn = (String) legData
				.get(LegDataAttributes.DIAMETER_MSCC_SERVICE_IDENTIFIER);

		Long svcId = -1L;
		if (serviceIdn != null) {
			svcId = Long.parseLong(serviceIdn);

			if (logger.isDebugEnabled()) {
				logger.debug("::add Service_Identifier to MSCC: ");
			}

		}
		
		Long[] svcid= new Long[]{svcId};

		List<DiameterAVP> multiSCCUnitAVP = roMsgfactory
				.createMultipleServicesCreditControlAVP(reqGrantedSVCUnitAVP,
						reqSvcUnitAVP, usuarrray, svcid, null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						null);

		return multiSCCUnitAVP;
	}
	
	public static List<DiameterAVP> createSubscriptionIdToCCR(
			List<String> avpCodes, String serviceId, LegData legData)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createSubscriptionIdToCCR: addAvps" +avpCodes);
		}

		DiameterRoMessageFactory roMsgfactory = PhUtilityServices.getInstance(
				serviceId).getDiameterRoMesageFactory();

		if (logger.isDebugEnabled()) {
			logger.debug("::createSubscriptionIdToCCR: roMsgfactory " +roMsgfactory);
		}
		/**
		 * create Subscription Id AVP
		 */
		List<DiameterAVP> subscriptionIdAVP = null;
		if (avpCodes.contains(DiameterAVPNames.Subscription_Id)) {
			subscriptionIdAVP = createSubscriptionIdAVP(roMsgfactory,
					serviceId, legData);
			if (logger.isDebugEnabled()) {
				logger.debug("::Subscription_Id created : "+subscriptionIdAVP);
			}
		}

		return subscriptionIdAVP;
	}
	
	
	public static List<DiameterAVP> createServiceInformationToCCR(
			List<String> avpCodes, String serviceId, LegData legData)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createServiceInformationToCCR: addAvps to it " +avpCodes);
		}

		DiameterRoMessageFactory roMsgfactory = PhUtilityServices.getInstance(
				serviceId).getDiameterRoMesageFactory();

		if (logger.isDebugEnabled()) {
			logger.debug("::createServiceInformationToCCR: roMsgfactory " +roMsgfactory);
		}
		/**
		 * create Ims Information AVP
		 */
		List<DiameterAVP> imsInfoAVP = null;
		if (avpCodes.contains(DiameterAVPNames.Ims_Information)) {
			imsInfoAVP = createImsInformationAVP(roMsgfactory,
					serviceId, legData);
			if (logger.isDebugEnabled()) {
				logger.debug("::Ims_Information created : "+imsInfoAVP);
			}
		}
		
		/**
		 * create AOC Information AVP
		 */
		List<DiameterAVP>[] aocInfoAVPArr = null;
		
		List<DiameterAVP> aocInfoAVP=null;
		if (avpCodes.contains(DiameterAVPNames.AoC_Information)) {
			aocInfoAVP = createAocInformation(roMsgfactory,legData);
			if (logger.isDebugEnabled()) {
				logger.debug("::AoC_Information created : "+aocInfoAVP);
			}
			
			aocInfoAVPArr = new List[1];
			aocInfoAVPArr[0] = aocInfoAVP;
		}
		return roMsgfactory.createServiceInformationAVP(aocInfoAVP, null, imsInfoAVP, null, null, null, null, null, null, null, null, null, null, null,null);
	}
	
	
	public static List<DiameterAVP> createServiceInformationToCCA(
			List<String> avpCodes, DiameterRoMessageFactory roMsgfactory , LegData legData)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createServiceInformationToCCA: addAvps" +avpCodes);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("::createServiceInformationToCCA: roMsgfactory " +roMsgfactory);
		}
		
		/**
		 * create AOC Information AVP
		 */
		List<DiameterAVP>[] aocInfoAVPArr = null;
		
		List<DiameterAVP> aocInfoAVP=null;
		if (avpCodes.contains(DiameterAVPNames.AoC_Information)) {
			aocInfoAVP = createAocInformation(roMsgfactory, legData);
			if (logger.isDebugEnabled()) {
				logger.debug("::AoC_Information created : "+aocInfoAVP);
			}
			
			aocInfoAVPArr = new List[1];
			aocInfoAVPArr[0] = aocInfoAVP;
		}
		return roMsgfactory.createServiceInformationAVP(aocInfoAVP, null, null, null, null, null, null, null, null, null, null, null, null, null,null);
	}
	
	public static List<DiameterAVP> createAocInformation(DiameterRoMessageFactory roMsgfactory, LegData legData)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createAocInformation: ");
		}
		String valueDigit = (String) legData
				.get(LegDataAttributes.DIAMETER_AOC_COST_INFO_ACCM_COST_VALUE_DIGITS);
		
		Long vd = 0L;
		if (valueDigit != null)
			vd = Long.parseLong(valueDigit);
		
		String expo = (String) legData
				.get(LegDataAttributes.DIAMETER_AOC_COST_INFO_ACCM_COST_EXPONENT);
		
		Integer exp = 0;
		if (expo != null)
			exp = Integer.parseInt(expo);

		String cc = (String) legData
				.get(LegDataAttributes.DIAMETER_AOC_COST_INFO_CURRENCY_CODE);

		Long ccl = 0L;
		if (cc != null)
			ccl = Long.parseLong(cc);
		
		List<DiameterAVP>  acmcost=roMsgfactory.createAccumulatedCostAVP(vd, exp);
		List<DiameterAVP> aocCostInfo=roMsgfactory.createAoCCostInformationAVP(acmcost, null, ccl);

		return roMsgfactory.createAoCInformationAVP(aocCostInfo, null, null);
				
	}
	
	private static List<DiameterAVP> createImsInformationAVP(
			DiameterRoMessageFactory roMsgfactory, String serviceId,
			LegData legData) throws DiameterInvalidArgumentException, DiameterException {
		if (logger.isDebugEnabled()) {
			logger.debug("::createImsInformationAVP:");
		}
	
		String sipMethod = (String) legData
				.get(LegDataAttributes.DIAMETER_IMS_INFO_EVENT_TYPE_SIP_METHOD);

		String eventType = (String) legData
				.get(LegDataAttributes.DIAMETER_IMS_INFO_EVENT_TYPE_EVENT);

		String expires = (String) legData
				.get(LegDataAttributes.DIAMETER_IMS_INFO_EVENT_TYPE_EXPIRES);
		

		Long typeExp = 0L;
		if (expires != null)
			typeExp = Long.parseLong(expires);
		
		List<DiameterAVP> roEventType=roMsgfactory.createEventTypeAVP(sipMethod, eventType, typeExp);
		
		String nodeNunctionality = (String) legData
				.get(LegDataAttributes.DIAMETER_IMS_INFO_NODE_FUNCTIONALITY);
		
		String roleOfNode = (String) legData
				.get(LegDataAttributes.DIAMETER_IMS_INFO_ROLE_OF_NODE);
		
		String userSessionId = (String) legData
				.get(LegDataAttributes.DIAMETER_IMS_USER_SESSION_ID);
		
		String calngPartyAddress = (String) legData
				.get(LegDataAttributes.DIAMETER_IMS_CALLING_PARTY_ADDRESS);
		
		String[] callingpartaddresses= new String[]{calngPartyAddress};
		
		String calledPartyAddress = (String) legData
				.get(LegDataAttributes.DIAMETER_IMS_CALLED_PARTY_ADDRESS);
		
		String sipReqTimeStamp = (String) legData
				.get(LegDataAttributes.DIAMETER_IMS_SIP_REQUEST_TIMESTAMP);
		
		List<DiameterAVP> timestamps =roMsgfactory.createTimeStampsAVP(sipReqTimeStamp, null);

		return roMsgfactory.createIMSInformationAVP(roEventType, roleOfNode, nodeNunctionality, userSessionId, null, null, callingpartaddresses, 
				calledPartyAddress, null, null, null, null, null, null, 
				null, timestamps, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
                null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,null);
				
		
		
	}

	/**
	 * This method is sued to create MSCC AVP
	 * 
	 * @param avpCodes
	 * 
	 * @param serviceId
	 * @param legData
	 * @param resultCode 
	 * @throws DiameterInvalidArgumentException
	 * @throws DiameterException
	 */
	public static List<DiameterAVP> createMultiServiceCreditControlToCCA(
			List<String> avpCodes, String serviceId, LegData legData,long resultCode)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createMultiServiceCreditControlToCCA:");
		}

		DiameterRoMessageFactory roMsgfactory = PhUtilityServices.getInstance(
				serviceId).getDiameterRoMesageFactory();

		/**
		 * create Requested Service Unit AVP
		 */
		List<DiameterAVP> reqSvcUnitAVP = null;
		if (avpCodes.contains(DiameterAVPNames.Requested_Service_Unit)) {
			reqSvcUnitAVP = createRequestedServiceUnitAVP(roMsgfactory,
					serviceId, legData);
			if (logger.isDebugEnabled()) {
				logger.debug("::Requested_Service_Unit created : "+reqSvcUnitAVP);
			}
		}

		/**
		 * Create granted Service Unit
		 */
		List<DiameterAVP> reqGrantedSVCUnitAVP = null;
		if (avpCodes.contains(DiameterAVPNames.Granted_Service_Unit)) {
			reqGrantedSVCUnitAVP = createGrantedServiceUnitAVP(roMsgfactory,
					serviceId, legData);
			if (logger.isDebugEnabled()) {
				logger.debug("::Granted_Service_Unit created : "+reqGrantedSVCUnitAVP);
			}
		}

		/**
		 * create Used Service Unit AVP
		 */
		List<DiameterAVP>[] usuarrray=null;
		if (avpCodes.contains(DiameterAVPNames.Used_Service_Unit)) {
			List<DiameterAVP> usedSVCUnitAVP = createUsedServiceUnitAVP(roMsgfactory, serviceId,
					legData);
		    usuarrray = new List[1];
			usuarrray[0] = usedSVCUnitAVP;
			
			if (logger.isDebugEnabled()) {
				logger.debug("::Used_Service_Unit created : "+usedSVCUnitAVP);
			}
		}
		/**
		 * Create final unit indication
		 */
		
		List<DiameterAVP> finalUIUnitAVP = null;
		if (avpCodes.contains(DiameterAVPNames.Final_Unit_Indication)) {
			 finalUIUnitAVP =createFinalUnitIndication(roMsgfactory, legData);
				if (logger.isDebugEnabled()) {
					logger.debug("::Final_Unit_Indication created : "+finalUIUnitAVP);
				}
		}
				
		Long ValidityTime = null;
		
		String validityTime = (String) legData
				.get(LegDataAttributes.DIAMETER_VALIDITY_TIME);

		if (validityTime != null) {
			ValidityTime = Long.parseLong(validityTime);
		}
//		if (avpCodes.contains(DiameterAVPNames.Validity_Time)) {
//			//ValidityTime =createValidityTime(roMsgfactory, legData);
//				if (logger.isDebugEnabled()) {
//					logger.debug("::Validity_Time created is  : "+ValidityTime);
//				}
//		//}
		
		Long timeQuotaThres = null;
		//if (avpCodes.contains(DiameterAVPNames.Time_Quota_Threshold)) {
			timeQuotaThres =createTimeQuotaThreshold(roMsgfactory, legData);
				if (logger.isDebugEnabled()) {
					logger.debug("::Time_Quota_Threshold created : "+timeQuotaThres);
				}
		//}


		/**
		 * Create Announcement information AVP
		 */
		List<DiameterAVP>[] annarrray=null;
//		if (avpCodes.contains(DiameterAVPNames.Announcement_Information)) {
//			List<DiameterAVP> AnnInfo = createAnnouncementInfrmationAVP(serviceId,
//				legData);
//			annarrray = new List[1];
//			annarrray[0] = AnnInfo;
//			
//			if (logger.isDebugEnabled()) {
//				logger.debug("::Announcement_Information created : "+annarrray);
//			}
//		}
		
		String serviceIdn = (String) legData
				.get(LegDataAttributes.DIAMETER_MSCC_SERVICE_IDENTIFIER);

		Long svcId = -1L;
		if (serviceIdn != null) {
			svcId = Long.parseLong(serviceIdn);

			if (logger.isDebugEnabled()) {
				logger.debug("::add Service_Identifier to MSCC: " +serviceIdn);
			}

		}
		
		String resutCodeStr = (String) legData
				.get(LegDataAttributes.DIAMETER_MSCC_RESULT_CODE);

		long resultCodeupdated=resultCode;
		if (resutCodeStr != null) {
			resultCodeupdated=Long.parseLong(resutCodeStr);//DiameterProtocolHelper.getReturnCode(resutCodeStr);
		}
		
		
		if (logger.isDebugEnabled()) {
			logger.debug("::add DIAMETER_MSCC_RESULT_CODE to MSCC: " +resultCodeupdated);
		}

		
		Long[] svcid= new Long[]{svcId};

		List<DiameterAVP> multiSCCUnitAVP = roMsgfactory
				.createMultipleServicesCreditControlAVP(reqGrantedSVCUnitAVP,
						reqSvcUnitAVP, usuarrray, svcid, null, null, ValidityTime, resultCodeupdated,
						finalUIUnitAVP, timeQuotaThres, null, null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						annarrray, null, null);
		
		DiameterGroupedAVP groupedAvp=createAnnouncementInstructions(legData, serviceId);
		
		if(groupedAvp!=null){		
			if (logger.isDebugEnabled()) {
				logger.debug("::add Ann instructions to MSCC grouped avp list: ");
			}
			
			multiSCCUnitAVP.add(groupedAvp);
			
		}
		return multiSCCUnitAVP;
	}
	
	
	private static DiameterGroupedAVP createAnnouncementInstructions(
			LegData legData, String serviceId) {
		// TODO Auto-generated method stub

		DiameterGroupedAVP groupedAvp = null;
		String annNumber = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_INST_ANN_NUMBER);
		String order = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_INST_ANN_ORDER);

		String annType = (String) legData
				.get(LegDataAttributes.DIAMETER_ANN_INST_ANN_TYPE);

		if (annNumber != null || order != null || annType != null) {

			if (logger.isDebugEnabled()) {
				logger.debug("::add Ann instructions to MSCC: ");
			}
			Long annNum = 0L;
			if (annNumber != null)
				annNum = Long.parseLong(annNumber);

			Long annOrd = 0L;
			if (order != null)
				annOrd = Long.parseLong(order);

			Long annTy = 0L;
			if (annType != null)
				annTy = Long.parseLong(annType);

			DiameterMessageFactory diaFactory = PhUtilityServices.getInstance(
					serviceId).getDiameterBaseMesageFactory();

			groupedAvp = diaFactory.createGroupedAVP(
					DiameterAVPNames.Announcement_Instructions,
					DiameterConstants.VENDOR_NAME_ERICSSON);

			DiameterUnsigned32AVP avpIn = diaFactory.createUnsigned32AVP(
					DiameterAVPNames.Announcement_Number,
					DiameterConstants.VENDOR_NAME_ERICSSON,
					Long.parseLong(annNumber));
			groupedAvp.add(avpIn);

			DiameterUnsigned32AVP avpIn1 = diaFactory.createUnsigned32AVP(
					DiameterAVPNames.Announcement_Order,
					DiameterConstants.VENDOR_NAME_ERICSSON,
					Long.parseLong(order));
			groupedAvp.add(avpIn1);

			DiameterUnsigned32AVP avpIn2 = diaFactory.createUnsigned32AVP(
					DiameterAVPNames.Announcement_Type,
					DiameterConstants.VENDOR_NAME_ERICSSON,
					Long.parseLong(annType));
			groupedAvp.add(avpIn2);

		}

		return groupedAvp;
	}

	private static long createValidityTime(
			DiameterRoMessageFactory roMsgfactory, LegData legData) {

		if (logger.isDebugEnabled()) {
			logger.debug("::createValidityTime:");
		}
		/**
		 * createValidityTime(java.lang.String createValidityTime)
		 */
		String validityTime = (String) legData
				.get(LegDataAttributes.DIAMETER_VALIDITY_TIME);

		Long validTime = null;
		if (validityTime != null) {
			validTime = Long.parseLong(validityTime);
		}
		return validTime;
	}

	private static Long createTimeQuotaThreshold(
			DiameterRoMessageFactory roMsgfactory, LegData legData) {

		if (logger.isDebugEnabled()) {
			logger.debug("::createTimeQuotaThreshold:");
		}
		/**
		 * createTimeQuotaThreshold(java.lang.String createTimeQuotaThreshold)
		 */
		String timeQuota = (String) legData
				.get(LegDataAttributes.DIAMETER_TIME_QUOTA_THRESHOLD);
		Long timeQuotaLong = null;
		if (timeQuota != null) {
			timeQuotaLong = Long.parseLong(timeQuota);
		}
		return timeQuotaLong;
	}

	/**
	 * 
	 * @param roMsgfactory
	 * @param legData
	 * @return
	 * @throws DiameterInvalidArgumentException
	 * @throws DiameterException
	 */
	private static List<DiameterAVP> createFinalUnitIndication(
			DiameterRoMessageFactory roMsgfactory, LegData legData)
			throws DiameterInvalidArgumentException, DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createFinalUnitIndication:");
		}
		/**
		 * createFinalUnitIndicationAVP(java.lang.String finalUnitAction)
		 */
		String finalunitaction = (String) legData
				.get(LegDataAttributes.DIAMETER_FUI_FINAL_UNIT_ACTION);
		
		List<DiameterAVP> finalUIUnitAVP=null;
		
		if(finalunitaction!=null){
		finalUIUnitAVP = roMsgfactory.createFinalUnitIndicationAVP(finalunitaction);
		}
		
		return finalUIUnitAVP;
	}

	/**
	 * 
	 * @param roMsgfactory
	 * @param serviceId
	 * @param legData
	 * @return
	 * @throws DiameterInvalidArgumentException
	 * @throws DiameterException
	 */
	private static List<DiameterAVP> createUsedServiceUnitAVP(
			DiameterRoMessageFactory roMsgfactory, String serviceId,
			LegData legData) throws DiameterInvalidArgumentException,
			DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createUsedServiceUnitAVP:");
		}

		/**
		 * createUsedServiceUnitAVP(java.lang.String reportingReason,
		 * java.lang.String tariffChangeUsage, java.lang.Long cCTime,
		 * java.math.BigInteger cCTotalOctets, java.math.BigInteger
		 * cCInputOctets, java.math.BigInteger cCOutputOctets,
		 * java.math.BigInteger cCServiceSpecificUnits, java.lang.String[]
		 * eventChargingTimeStamp)
		 */

		String usuRepReason = (String) legData
				.get(LegDataAttributes.DIAMETER_USU_REPORTING_REASON);
		String usuTrafficChangsage = (String) legData
				.get(LegDataAttributes.DIAMETER_USU_TARIFF_CHANGE_USAGE);

		String usuccmCT = (String) legData
				.get(LegDataAttributes.DIAMETER_USU_CC_TIME);
		Long usucct = System.currentTimeMillis();
		if (usuccmCT != null && !usuccmCT.isEmpty()) {
			usucct = Long.parseLong(usuccmCT);
		}

		String ccusuCTO = (String) legData
				.get(LegDataAttributes.DIAMETER_USU_CC_TOTAL_OCTETS);

		BigInteger bigusuccto = null;
		if (ccusuCTO != null){
			ccusuCTO=removeDecimal(ccusuCTO);
			bigusuccto = new BigInteger(ccusuCTO);
		}

		String ccusuCIO = (String) legData
				.get(LegDataAttributes.DIAMETER_USU_CC_INPUT_OCTETS);

		BigInteger bigusuccio = null;
		if (ccusuCIO != null){
			ccusuCIO=removeDecimal(ccusuCIO);
			bigusuccio = new BigInteger(ccusuCIO);
		}

		String ccusuOO = (String) legData
				.get(LegDataAttributes.DIAMETER_USU_CC_OUTPUT_OCTETS);

		BigInteger bigusuccoo = null;
		if (ccusuOO != null){
			ccusuOO=removeDecimal(ccusuOO);
			bigusuccoo = new BigInteger(ccusuOO);
		}

		String ccusussu = (String) legData
				.get(LegDataAttributes.DIAMETER_USU_CC_SERVICE_SPECIFIC_UNITS);
		BigInteger bigusuccssu = null;
		
		if (ccusussu != null){
			ccusussu=removeDecimal(ccusussu);
			bigusuccssu = new BigInteger(ccusussu);
		}

		String ccusuevenTimeStmp = (String) legData
				.get(LegDataAttributes.DIAMETER_USU_EVENT_CHARGING_TIMESTAMP);
	
		String[] timestamps = null;
		
		if(ccusuevenTimeStmp!=null){
			timestamps=new String[] { ccusuevenTimeStmp };
		}

		List<DiameterAVP> usedSVCUnitAVP = roMsgfactory
				.createUsedServiceUnitAVP(usuRepReason, usuTrafficChangsage,
						usucct, bigusuccto, bigusuccio, bigusuccoo,
						bigusuccssu, timestamps);
		
		if (logger.isDebugEnabled()) {
			logger.debug("::createUsedServiceUnitAVP: exit");
		}
		return usedSVCUnitAVP;
	}

	/**
	 * 
	 * @param roMsgfactory
	 * @param serviceId
	 * @param legData
	 * @return
	 * @throws DiameterInvalidArgumentException
	 * @throws DiameterException
	 */
	private static List<DiameterAVP> createGrantedServiceUnitAVP(
			DiameterRoMessageFactory roMsgfactory, String serviceId,
			LegData legData) throws DiameterInvalidArgumentException,
			DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createGrantedServiceUnitAVP:");
		}

		/**
		 * createGrantedServiceUnitAVP(java.lang.String tariffChangeUsage, int
		 * CCTime, int valueDigits, int exponent, int currencyCode, int
		 * CCTotalOctets, int CCInputOctets, int CCOutputOctets, int
		 * CCServiceSpecificUnits)
		 */

		String gsuccmCT = (String) legData
				.get(LegDataAttributes.DIAMETER_GSU_CC_TIME);
		Long gsucct = System.currentTimeMillis();

		if (gsuccmCT != null && !gsuccmCT.isEmpty()) {
			gsucct = Long.parseLong(gsuccmCT);
		}

		String ccmGSUValueDigits = (String) legData
				.get(LegDataAttributes.DIAMETER_GSU_CC_MONEY_VALUE_DIGITS);

		Long gsuvd = null;
		if (ccmGSUValueDigits != null)
			gsuvd = Long.parseLong(ccmGSUValueDigits);

		String ccmGSUExponent = (String) legData
				.get(LegDataAttributes.DIAMETER_GSU_CC_MONEY_EXPONENT);

		Integer gsuexp = null;
		if (ccmGSUExponent != null)
			gsuexp = Integer.parseInt(ccmGSUExponent);

		String ccmGsuCC = (String) legData
				.get(LegDataAttributes.DIAMETER_GSU_CC_MONEY_CURRENCY_CODE);

		Long gsucc = null;
		if (ccmGsuCC != null)
			gsucc = Long.parseLong(ccmGsuCC);

		List<DiameterAVP> gsuccmAVP = null;
		if (gsuccmCT != null || ccmGSUValueDigits != null
				|| ccmGSUExponent != null || ccmGsuCC != null) {

			List<DiameterAVP> gsuvAVP = roMsgfactory.createUnitValueAVP(gsuvd,
					gsuexp);
			gsuccmAVP = roMsgfactory.createCCMoneyAVP(gsuvAVP, gsucc);
		}

		String ccgsuCTO = (String) legData
				.get(LegDataAttributes.DIAMETER_GSU_CC_TOTAL_OCTETS);

		BigInteger biggsuccto = null;
		if (ccgsuCTO != null){
			ccgsuCTO=removeDecimal(ccgsuCTO);
			biggsuccto = new BigInteger(ccgsuCTO);
		}

		String ccgsuCIO = (String) legData
				.get(LegDataAttributes.DIAMETER_GSU_CC_INPUT_OCTETS);

		BigInteger biggsuccio = null;
		if (ccgsuCIO != null){
			ccgsuCIO=removeDecimal(ccgsuCIO);
			biggsuccio = new BigInteger(ccgsuCIO);
		}

		String ccgsuOO = (String) legData
				.get(LegDataAttributes.DIAMETER_GSU_CC_OUTPUT_OCTETS);

		BigInteger biggsuccoo = null;
		if (ccgsuOO != null){
			ccgsuOO=removeDecimal(ccgsuOO);
			biggsuccoo = new BigInteger(ccgsuOO);
		}

		String ccgsussu = (String) legData
				.get(LegDataAttributes.DIAMETER_GSU_CC_SERVICE_SPECIFIC_UNITS);

		BigInteger biggsuccssu = null;
		if (ccgsussu != null){
			ccgsussu=removeDecimal(ccgsussu);
			biggsuccssu = new BigInteger(ccgsussu);
		}
		
		String tarriftime = (String) legData
				.get(LegDataAttributes.DIAMETER_GSU_TARIFF_TIME_CHANGE);


		List<DiameterAVP> reqGrantedSVCUnitAVP = roMsgfactory
				.createGrantedServiceUnitAVP(tarriftime,
						gsucct, gsuccmAVP, biggsuccto, biggsuccio, biggsuccoo,
						biggsuccssu);
		
		if (logger.isDebugEnabled()) {
			logger.debug("::createGrantedServiceUnitAVP: return");
		}
		return reqGrantedSVCUnitAVP;
	}

	/**
	 * 
	 * @param roMsgfactory
	 * @param serviceId
	 * @param legData
	 * @return
	 * @throws DiameterInvalidArgumentException
	 * @throws DiameterException
	 */
	private static List<DiameterAVP> createRequestedServiceUnitAVP(
			DiameterRoMessageFactory roMsgfactory, String serviceId,
			LegData legData) throws DiameterInvalidArgumentException,
			DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createRequestedServiceUnitAVP:");
		}
		/**
		 * createRequestedServiceUnitAVP(java.lang.Long cCTime,
		 * java.util.List<DiameterAVP> cCMoney, java.math.BigInteger
		 * cCTotalOctets, java.math.BigInteger cCInputOctets,
		 * java.math.BigInteger cCOutputOctets, java.math.BigInteger
		 * cCServiceSpecificUnits)
		 */

		String ccmValueDigits = (String) legData
				.get(LegDataAttributes.DIAMETER_RSU_CC_MONEY_VALUE_DIGITS);

		Long vd = null;
		if (ccmValueDigits != null)
			vd = Long.parseLong(ccmValueDigits);

		String ccmExponent = (String) legData
				.get(LegDataAttributes.DIAMETER_RSU_CC_MONEY_EXPONENT);

		Integer exp = null;
		if (ccmExponent != null)
			exp = Integer.parseInt(ccmExponent);

		String ccmCC = (String) legData
				.get(LegDataAttributes.DIAMETER_RSU_CC_MONEY_CURRENCY_CODE);

		Long cc = null;
		if (ccmCC != null)
			cc = Long.parseLong(ccmCC);

		List<DiameterAVP> ccmAVP=null;
		  List<DiameterAVP> uvAVP=null;
		  
		if(ccmValueDigits!=null || ccmExponent!=null ) {
			uvAVP= roMsgfactory.createUnitValueAVP(vd, exp);
		}
		
		 if(ccmCC!=null)
		       ccmAVP = roMsgfactory.createCCMoneyAVP(uvAVP, cc);

		String ccmCT = (String) legData
				.get(LegDataAttributes.DIAMETER_RSU_CC_TIME);
		Long cct = System.currentTimeMillis();
		if (ccmCT != null && !ccmCT.isEmpty()) {
			cct = Long.parseLong(ccmCT);
		}

		String ccCTO = (String) legData
				.get(LegDataAttributes.DIAMETER_RSU_CC_TOTAL_OCTETS);

		BigInteger bigccto = null;

		if (ccCTO != null){
			ccCTO=removeDecimal(ccCTO);
			bigccto = new BigInteger(ccCTO);
		}
		String ccCIO = (String) legData
				.get(LegDataAttributes.DIAMETER_RSU_CC_INPUT_OCTETS);

		BigInteger bigccio = null;

		if (ccCIO != null){
			ccCIO=removeDecimal(ccCIO);
			bigccio = new BigInteger(ccCIO);
		}

		String ccCOO = (String) legData
				.get(LegDataAttributes.DIAMETER_RSU_CC_OUTPUT_OCTETS);

		BigInteger bigccoo = null;

		if (ccCOO != null){
			ccCOO=removeDecimal(ccCOO);
			bigccoo = new BigInteger(ccCOO);
		}

		String ccssu = (String) legData
				.get(LegDataAttributes.DIAMETER_RSU_CC_SERVICE_SPECIFIC_UNITS);

		BigInteger bigccssu = null;

		if (ccssu != null){
			ccssu=removeDecimal(ccssu);
			bigccssu = new BigInteger(ccssu);
		}
		
		List<DiameterAVP> reqRequstedSVCUnitAVP= roMsgfactory.createRequestedServiceUnitAVP(cct, ccmAVP, bigccto,
				bigccio, bigccoo, bigccssu);
		
		if (logger.isDebugEnabled()) {
			logger.debug("::createRequestedServiceUnitAVP: exit");
		}
		return reqRequstedSVCUnitAVP;
	}
	
	/**
	 * 
	 * @param roMsgfactory
	 * @param serviceId
	 * @param legData
	 * @return
	 * @throws DiameterInvalidArgumentException
	 * @throws DiameterException
	 */
	private static List<DiameterAVP> createSubscriptionIdAVP(
			DiameterRoMessageFactory roMsgfactory, String serviceId,
			LegData legData) throws DiameterInvalidArgumentException,
			DiameterException {

		if (logger.isDebugEnabled()) {
			logger.debug("::createSubscriptionIdAVP:");
		}

		String subsIdType = (String) legData
				.get(LegDataAttributes.DIAMETER_SUBSCRIPTION_ID_TYPE);


		String subsIdData = (String) legData
				.get(LegDataAttributes.DIAMETER_SUBSCRIPTION_ID_DATA);

		if (logger.isDebugEnabled()) {
			logger.debug("::subsIdType : " +subsIdType);
			logger.debug("::subsIdData : " +subsIdData);
		}

		return roMsgfactory.createSubscriptionIdAVP(subsIdType, subsIdData);
	}
	
	private static String removeDecimal(String bigIStr) {

		String decimal = null;
		if (bigIStr.indexOf(".") != -1) {
			decimal = bigIStr.substring(0, bigIStr.indexOf("."));
		}else{
			return bigIStr;
		}
		return decimal;
	}

	/**
	 * This method is used to create cost information avp
	 * 
	 * @param serviceId
	 * @param legData
	 * @return
	 * @throws DiameterInvalidArgumentException
	 * @throws DiameterException
	 */
	public static List<DiameterAVP> createCostInformation(String serviceId,
			LegData legData) throws DiameterInvalidArgumentException,
			DiameterException {

		DiameterRoMessageFactory roMsgfactory = PhUtilityServices.getInstance(
				serviceId).getDiameterRoMesageFactory();

		if (logger.isDebugEnabled()) {
			logger.debug("::createCostInformation:");
		}

		/**
		 * createRequestedServiceUnitAVP(java.lang.Long cCTime,
		 * java.util.List<DiameterAVP> cCMoney, java.math.BigInteger
		 * cCTotalOctets, java.math.BigInteger cCInputOctets,
		 * java.math.BigInteger cCOutputOctets, java.math.BigInteger
		 * cCServiceSpecificUnits)
		 */

		String costInfoCostUnit = (String) legData
				.get(LegDataAttributes.DIAMETER_COST_INFO_COST_UNIT);
		String costValueDigits = (String) legData
				.get(LegDataAttributes.DIAMETER_COST_INFO_VALUE_DIGITS);
		String costInfoExp = (String) legData
				.get(LegDataAttributes.DIAMETER_COST_INFO_EXPONENT);

		Long valDigits = null;
		if (costValueDigits != null) {
			valDigits = Long.parseLong(costValueDigits);
		}

		Integer costExp = null;
		if (costValueDigits != null) {
			costExp = Integer.parseInt(costInfoExp);
		}

		String costInfoCurrCode = (String) legData
				.get(LegDataAttributes.DIAMETER_COST_INFO_CURRENCY_CODE);

		Long cccode = null;
		if (costInfoCurrCode != null) {
			cccode = Long.parseLong(costInfoCurrCode);
		}

		// <!-- Ref = [base - RFC4006] 8.7 -->
		// <avp name="Cost-Information" code="423" mandatory="must"
		// may-encrypt="yes"
		// vendor-specific="no" >
		// <grouped>
		// <gavp name="Unit-Value"/>
		// <gavp name="Currency-Code"/>
		// <gavp name="Cost-Unit"/>
		// </grouped>
		// </avp>
		//
		// java.util.List<DiameterAVP>
		// createCostInformationAVP(java.util.List<DiameterAVP> unitValue,
		// java.lang.Long currencyCode, java.lang.String costUnit)
		// Create a Cost-Information grouped AVP.

		List<DiameterAVP> unitValueAVP = roMsgfactory.createUnitValueAVP(
				valDigits, costExp);

		return roMsgfactory.createCostInformationAVP(unitValueAVP, cccode,
				costInfoCostUnit);

	}
	
 public static void main(String[] ar){
		 
		 String str="100";
		System.out.println("without  decimal is"+ removeDecimal(str));
	 }

}
