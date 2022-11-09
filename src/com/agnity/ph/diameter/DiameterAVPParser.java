package com.agnity.ph.diameter;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Date;
import java.util.List;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.diameter.emum.ServiceIdType;
import com.baypackets.ase.ra.diameter.ro.CreditControlAnswer;
import com.baypackets.ase.ra.diameter.ro.CreditControlRequest;
import com.baypackets.ase.ra.diameter.ro.enums.RedirectAddressTypeEnum;
import com.baypackets.ase.ra.diameter.ro.enums.ReportingReasonEnum;
import com.baypackets.ase.ra.diameter.ro.enums.TariffChangeUsageEnum;
import com.baypackets.ase.resource.ResourceException;

import fr.marben.diameter.DiameterAVP;
import fr.marben.diameter.DiameterGroupedAVP;
import fr.marben.diameter.DiameterInteger32AVP;
import fr.marben.diameter.DiameterInteger64AVP;
import fr.marben.diameter.DiameterOctetStringAVP;
import fr.marben.diameter.DiameterUnsigned32AVP;
import fr.marben.diameter.DiameterUnsigned64AVP;

/**
 * This class is used to fetch incoming CCR and CCA AVPs
 * 
 * @author reeta
 *
 */
public class DiameterAVPParser {

	private static final Logger logger = Logger.getLogger(DiameterAVPParser.class);

	/**
	 * This method is used to parse CCR basic mandatory AVPS
	 * 
	 * @param request
	 * @param callData
	 * @param vendorId
	 * @throws ResourceException
	 * @throws UnsupportedEncodingException
	 */
	public static void fetchCCRAVPs(CreditControlRequest request, CallData callData, long vendorId)
			throws ResourceException, UnsupportedEncodingException {

		if (logger.isDebugEnabled()) {
			logger.debug("Entering fetchCCRAVPs from request");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.DIAMETER_CCR_SESSION_ID, request.getSessionId());
		legData.set(LegDataAttributes.DIAMETER_DEST_REALM, request.getDestinationRealm());
		legData.set(LegDataAttributes.DIAMETER_DEST_HOST, request.getDestinationHost());
		legData.set(LegDataAttributes.DIAMETER_ORIGIN_REALM, request.getOriginRealm());
		legData.set(LegDataAttributes.DIAMETER_ORIGIN_HOST, request.getOriginHost());
		legData.set(LegDataAttributes.DIAMETER_CC_REQUEST_NUMBER, "" + request.getCCRequestNumber());
		legData.set(LegDataAttributes.DIAMETER_CC_REQUEST_TYPE, "" + request.getCCRequestType());
		legData.set(LegDataAttributes.DIAMETER_AUTH_APP_ID, "" + request.getAuthApplicationId());
		legData.set(LegDataAttributes.DIAMETER_SERVICE_CONTEXT_ID, "" + request.getServiceContextId());

		// adding Destination Host by OCS Team
	//	legData.set(LegDataAttributes.DIAMETER_USER_NAME, request.getName());

		// legData.set(LegDataAttributes.DIAMETER_ORIG_STATE_ID,
		// request.getO);
		//legData.set(LegDataAttributes.DIAMETER_EVENT_TIMESTAMP, "" + request.getTimestamp());
		
		DiameterAVP eventT = request.getAvp(DiameterAVPCodes.Event_Timestamp, DiameterConstants.VENDOR_ID_BASE);
		
		if (logger.isDebugEnabled()) {
			logger.debug("got event timestamp "+ eventT);
		}
		if (eventT != null) {
			
			DiameterOctetStringAVP userNStr = (DiameterOctetStringAVP) eventT;
			byte[] eventStrB = userNStr.getValue();


			// DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			 
			if (logger.isDebugEnabled()) {
					logger.debug("got event timestamp byte[]  eventStrB "+ eventStrB);
			}
			 
			String sss=new BigInteger(1, eventStrB).toString(16);
			
			if (logger.isDebugEnabled()) {
				logger.debug("got event timestamp bigint string  "+ sss);
			}
		//	int decimal=Integer.parseInt(sss,16);  
       
	    //    Instant instant = Instant.ofEpochSecond( decimal );

	      //  Date date = Date.from( instant );
	        
	        
			legData.set(LegDataAttributes.DIAMETER_EVENT_TIMESTAMP, sss);//date.toString());//dateFormat.format(c.getTime()));
		}

		DiameterAVP avp = request.getAvp(DiameterAVPCodes.Subscription_Id, DiameterConstants.VENDOR_ID_BASE);

		if (avp != null && avp instanceof DiameterGroupedAVP) {
			DiameterGroupedAVP groupedAVP = (DiameterGroupedAVP) avp;
			List<DiameterAVP> svcIdAVPs = groupedAVP.getValue();
			for (DiameterAVP svcAvp : svcIdAVPs) {
				if (svcAvp.getCode() == DiameterAVPCodes.Subscription_Id_Type) {
					DiameterInteger32AVP avpSvcID = (DiameterInteger32AVP) svcAvp;
					int id = avpSvcID.getValue();
					String svc_type = ServiceIdType.findName(id);
					legData.set(LegDataAttributes.DIAMETER_SUBSCRIPTION_ID_TYPE, "" + svc_type);
				}
				if (svcAvp.getCode() == DiameterAVPCodes.Subscription_Id_Data) {
					DiameterOctetStringAVP avpSvcID = (DiameterOctetStringAVP) svcAvp;
					byte[] svc_id_data = avpSvcID.getValue();

					String s = new String(svc_id_data, StandardCharsets.US_ASCII);
					legData.set(LegDataAttributes.DIAMETER_SUBSCRIPTION_ID_DATA, "" + s);
				}

			}
		}

		List<DiameterAVP> msccInfoList = request.getAvp(DiameterAVPCodes.Mutil_Service_Credit_Control);

		if (logger.isDebugEnabled()) {
			logger.debug("Mutil_Service_Credit_Control is " + msccInfoList);
		}

		if (msccInfoList != null && !msccInfoList.isEmpty()) {

			DiameterAVP msccInfo = msccInfoList.get(0);

			fetchMultiServiceCreditControl(msccInfo, legData);
		}

		DiameterAVP userN = request.getAvp(DiameterAVPCodes.User_Name, DiameterConstants.VENDOR_ID_BASE);
		if (userN != null) {
			DiameterOctetStringAVP userNStr = (DiameterOctetStringAVP) userN;
			byte[] userNStrB = userNStr.getValue();

			String s = new String(userNStrB, StandardCharsets.US_ASCII);
			legData.set(LegDataAttributes.DIAMETER_USER_NAME, "" + s);
		}
		
		
		DiameterAVP origState = request.getAvp(DiameterAVPCodes.Origin_State_ID, DiameterConstants.VENDOR_ID_3GPP);
		if (origState != null) {
			DiameterOctetStringAVP userNStr = (DiameterOctetStringAVP) origState;
			byte[] userNStrB = userNStr.getValue();

			String s = new String(userNStrB, StandardCharsets.US_ASCII);
			legData.set(LegDataAttributes.DIAMETER_ORIG_STATE_ID, "" + s);
		}

		// DiameterAVP userUnkwn = request
		// .getAvp(DiameterCCRAVPCodes.Unnown, 6431);

		// legData.set(LegDataAttributes.DIAMETER_PRECEDENCE,request.getServiceContextId());

		DiameterAVP svcInfoList = request.getAvp(DiameterAVPCodes.Service_Information,
				DiameterConstants.VENDOR_ID_3GPP);// DiameterConstants.VENDOR_3GPP);

		fetchServiceInformation(svcInfoList, legData);

		if (logger.isDebugEnabled()) {
			logger.debug("Leaving fetchCCRAVPs");
		}
		if (logger.isDebugEnabled()) {
			logger.debug("Leaving fetchCCRAVPs");
		}
	}

	/**
	 * This method is used to fetch Service Information AVP from CCR
	 * 
	 * @param svcInfoList
	 * @param legData
	 */
	private static void fetchServiceInformation(DiameterAVP svcInfoList, LegData legData) {
		// TODO Auto-generated method stub

		if (logger.isDebugEnabled()) {
			logger.debug("fetch fetchServiceInformation");
		}
		// <grouped>
		// <gavp name="Granted-Service-Unit"/>
		// <gavp name="Requested-Service-Unit"/>
		// <gavp name="Used-Service-Unit"/>
		// <gavp name="Final-Unit-Indication"/>

		if (svcInfoList == null) {
			return;
		}
		DiameterGroupedAVP groupedAVP = (DiameterGroupedAVP) svcInfoList;
		List<DiameterAVP> svcInfoListAvps = groupedAVP.getValue();

		for (DiameterAVP srvcInfoAvp : svcInfoListAvps) {

			if (srvcInfoAvp.getCode() == DiameterAVPCodes.Ims_Information) {
				fetchImsInformation(srvcInfoAvp, legData);
			}

			if (srvcInfoAvp.getCode() == DiameterAVPCodes.AoC_Information) {

				fetchAocInformation(srvcInfoAvp, legData);
			}
		} 

	}
	
	
	/**
	 * This method is used to fetch Service Information AVP from CCR
	 * 
	 * @param svcInfoList
	 * @param legData
	 */
	private static void fetchAnnInstructions(DiameterAVP svcInfoList, LegData legData) {
		// TODO Auto-generated method stub

		if (logger.isDebugEnabled()) {
			logger.debug("fetch fetchAnnInstructions");
		}
		if (svcInfoList == null) {
			return;
		}
		DiameterGroupedAVP groupedAVP = (DiameterGroupedAVP) svcInfoList;
		List<DiameterAVP> annInsListAvps = groupedAVP.getValue();

		for (DiameterAVP srvcAvp : annInsListAvps) {

			if (logger.isDebugEnabled()) {
				logger.debug("fetchAnnInstructions read all grouped AVPS");
			}
			if (srvcAvp.getCode() == DiameterAVPCodes.Announcement_Number) {
				DiameterUnsigned32AVP annNumber = (DiameterUnsigned32AVP) srvcAvp;
				legData.set(LegDataAttributes.DIAMETER_ANN_INST_ANN_NUMBER, "" + annNumber.getValue());
			}
			
			if (srvcAvp.getCode() == DiameterAVPCodes.Announcement_Inst_Order) {
				DiameterUnsigned32AVP order = (DiameterUnsigned32AVP) srvcAvp;
				legData.set(LegDataAttributes.DIAMETER_ANN_INST_ANN_ORDER, "" + order.getValue());
			}
			
			if (srvcAvp.getCode() == DiameterAVPCodes.Announcement_Type) {
				DiameterUnsigned32AVP type32 = (DiameterUnsigned32AVP) srvcAvp;
				legData.set(LegDataAttributes.DIAMETER_ANN_INST_ANN_TYPE, "" + type32.getValue());
			}
		}

	}

	///by ocs team
	private static void fetchImsInformation(DiameterAVP srvcInfoAvp, LegData legData) {

		if (logger.isDebugEnabled()) {
			logger.debug("fetchImsInformation");
		}

		if (srvcInfoAvp instanceof DiameterGroupedAVP) {

			DiameterGroupedAVP groupedSrvcInfo = (DiameterGroupedAVP) srvcInfoAvp;
			List<DiameterAVP> groupedSrvcInfoAVPS = groupedSrvcInfo.getValue();

			
//			  <gavp name="Event-Type"/>
//		        <gavp name="Role-Of-Node"/>
//		        <gavp name="Node-Functionality"/>
//		        <gavp name="User-Session-Id"/>
//		        <gavp name="Calling-Party-Address"/>
//		        <gavp name="Called-Party-Address"/>
//		        <gavp name="Time-Stamps"/>

			for (DiameterAVP srvcAvp : groupedSrvcInfoAVPS) {

				if (logger.isDebugEnabled()) {
					logger.debug("IMS-Information read all grouped AVPS");
				}
				if (srvcAvp.getCode() == DiameterAVPCodes.Ims_Called_Party_Adress) {
					DiameterOctetStringAVP calledPartyAddress = (DiameterOctetStringAVP) srvcAvp;
					legData.set(LegDataAttributes.DIAMETER_IMS_CALLED_PARTY_ADDRESS, "" + calledPartyAddress.getStringValue());
				}
				
				if (srvcAvp.getCode() == DiameterAVPCodes.Ims_Calling_Party_Adress) {
					DiameterOctetStringAVP callingPartyAddress = (DiameterOctetStringAVP) srvcAvp;
					legData.set(LegDataAttributes.DIAMETER_IMS_CALLING_PARTY_ADDRESS, "" + callingPartyAddress.getStringValue());
				}

				// <grouped>
				// <gavp name="Unit-Value"/>
				// <gavp name="Currency-Code"/>
				// </grouped>

				if (srvcAvp.getCode() == DiameterAVPCodes.Ims_Event_Type) {
					DiameterGroupedAVP eventTypeAVP = (DiameterGroupedAVP) srvcAvp;
					List<DiameterAVP> groupedeventTypeAVPs = eventTypeAVP.getValue();

					for (DiameterAVP groupedAvp : groupedeventTypeAVPs) {

						if (groupedAvp.getCode() == DiameterAVPCodes.Ims_Event_Type_Sip_Method) {

							DiameterOctetStringAVP groupedAvpSipmethod = (DiameterOctetStringAVP) groupedAvp;
							legData.set(LegDataAttributes.DIAMETER_IMS_INFO_EVENT_TYPE_SIP_METHOD, "" +groupedAvpSipmethod.getStringValue());
						}
					}
				} // / cc money ends here
				
				if (srvcAvp.getCode() == DiameterAVPCodes.Ims_Node_Functionality) {
					DiameterInteger32AVP uvAvpDigit = (DiameterInteger32AVP) srvcAvp;
					long digit = uvAvpDigit.getValue();
					legData.set(LegDataAttributes.DIAMETER_IMS_INFO_NODE_FUNCTIONALITY, "" + digit);
				}

				if (srvcAvp.getCode() == DiameterAVPCodes.Ims_Role_Of_Node) {
					DiameterInteger32AVP uvAvpDigit = (DiameterInteger32AVP) srvcAvp;
					long digit = uvAvpDigit.getValue();
					legData.set(LegDataAttributes.DIAMETER_IMS_INFO_ROLE_OF_NODE, "" + digit);
				}

				if (srvcAvp.getCode() == DiameterAVPCodes.Ims_User_Session_Id) {
					DiameterOctetStringAVP userSessId = (DiameterOctetStringAVP) srvcAvp;
					legData.set(LegDataAttributes.DIAMETER_IMS_USER_SESSION_ID, "" + userSessId.getStringValue());
				}
				
				if (srvcAvp.getCode() == DiameterAVPCodes.Ims_Time_stamps) {
					
					if (logger.isDebugEnabled()) {
						logger.debug("IMS-Information Ims_Time_stamps");
					}
					DiameterGroupedAVP eventTypeAVP = (DiameterGroupedAVP) srvcAvp;
					List<DiameterAVP> groupedeventTypeAVPs = eventTypeAVP.getValue();

					for (DiameterAVP groupedAvp : groupedeventTypeAVPs) {
						if (groupedAvp.getCode() == DiameterAVPCodes.Ims_SIP_Request_Timestamp) {
							

							if (logger.isDebugEnabled()) {
								logger.debug("Ims SIP_Request_Timestamp is instance of "+groupedAvp.getInstanceType());
							}
							
							try{
								DiameterOctetStringAVP userNStr = (DiameterOctetStringAVP) groupedAvp;
								byte[] eventStrB = userNStr.getValue();
								
//								DateFormat dateFormat = new SimpleDateFormat(
//										"yyyy-MM-dd HH:mm:ss");
								String sss = new BigInteger(1, eventStrB)
										.toString(16);
								//int decimal = Integer.parseInt(sss, 16);
						
							        
							     //   Instant instant = Instant.ofEpochSecond( decimal );

							      //  Date date = Date.from( instant );
							        

								legData.set(
										LegDataAttributes.DIAMETER_IMS_SIP_REQUEST_TIMESTAMP,sss);//date.toString());
										//dateFormat.format(c.getTime()));
							}catch(Exception e){
								logger.error(" could not parse ims sip request timestamp " +e);
							}
						}
					}
				}
				
				if (srvcAvp.getCode() == DiameterAVPCodes.Ims_Access_Network_Info) {
					DiameterOctetStringAVP accessnw = (DiameterOctetStringAVP) srvcAvp;
					if (logger.isDebugEnabled()) {
						logger.debug("Ims Ims_Access_Network_Info is instance of "+accessnw.getInstanceType());
					}
					legData.set(LegDataAttributes.DIAMETER_IMS_ACCESS_NETWORK_INFO, "" + accessnw.getStringValue());
				}
				
				
				if (srvcAvp.getCode() == DiameterAVPCodes.Ims_Inter_Operator_Identifier){
					
					if (logger.isDebugEnabled()) {
						logger.debug("Read Ims_Inter_Operator_Identifier");
					}
					DiameterGroupedAVP interOpAvp = (DiameterGroupedAVP) srvcAvp;
					List<DiameterAVP> groupedeventTypeAVPs = interOpAvp.getValue();

					for (DiameterAVP groupedAvp : groupedeventTypeAVPs) {
						if (groupedAvp.getCode() == DiameterAVPCodes.Ims_Terminating_IOI) {
							
							DiameterOctetStringAVP termoi = (DiameterOctetStringAVP) groupedAvp;
							if (logger.isDebugEnabled()) {
								logger.debug("Ims Ims_Access_Network_Info is instance of "+termoi.getInstanceType());
							}
							legData.set(LegDataAttributes.DIAMETER_IMS_TERMINATING_IOI, "" + termoi.getStringValue());
							
						}
				}
				}


			} // groupd requested service unit ends here
			
			
		}
	}
	
	
	private static void fetchAocInformation(DiameterAVP aocInfoAvp, LegData legData){
		
		if (logger.isDebugEnabled()) {
			logger.debug("fetchAocInformation");
		}
		
		if (aocInfoAvp instanceof DiameterGroupedAVP) {

			DiameterGroupedAVP aocInfoAVP = (DiameterGroupedAVP) aocInfoAvp;
			List<DiameterAVP> aocInfoAvps = aocInfoAVP.getValue();

			// <grouped>
			// <gavp name="CC-Time"/>
			// <gavp name="CC-Money"/>
			// <gavp name="CC-Total-Octets"/>
			// <gavp name="CC-Input-Octets"/>
			// <gavp name="CC-Output-Octets"/>
			// <gavp name="CC-Service-Specific-Units"/>
			// <gavp name="AVP"/>
			// </grouped>

			for (DiameterAVP rsuAvp : aocInfoAvps) {

				if (logger.isDebugEnabled()) {
					logger.debug("fetchAocInformation read all grouped AVPS");
				}
				if (rsuAvp.getCode() == DiameterAVPCodes.AoC_Cost_Information) {
					DiameterGroupedAVP ccmAVP = (DiameterGroupedAVP) rsuAvp;
					List<DiameterAVP> groupedAocCostAVPs = ccmAVP.getValue();

						for (DiameterAVP ccmAvp : groupedAocCostAVPs) {

							if (ccmAvp.getCode() == DiameterAVPCodes.Accumulated_Cost) {

								DiameterGroupedAVP ccmAVPs = (DiameterGroupedAVP) ccmAvp;
								// <grouped>
								// <gavp name="Value-Digits"/>
								// <gavp name="Exponent"/>
								// </grouped>
								for (DiameterAVP uvAvp : ccmAVPs.getValue()) {

									if (uvAvp.getCode() == DiameterAVPCodes.CC_Money_Value_digits) {
										DiameterInteger64AVP uvAvpDigit = (DiameterInteger64AVP) uvAvp;
										long digit = uvAvpDigit.getValue();
										legData.set(LegDataAttributes.DIAMETER_AOC_COST_INFO_ACCM_COST_VALUE_DIGITS, "" + digit);
									}
									if (uvAvp.getCode() == DiameterAVPCodes.CC_Money_Exponent) {
										DiameterInteger32AVP uvAvpExp = (DiameterInteger32AVP) uvAvp;
										int exp = uvAvpExp.getValue();
										legData.set(LegDataAttributes.DIAMETER_AOC_COST_INFO_ACCM_COST_EXPONENT, "" + exp);
									}

								}
							}

							if (ccmAvp.getCode() == DiameterAVPCodes.CC_Money_Currency_Code) {

								DiameterUnsigned32AVP uvAvpDigit = (DiameterUnsigned32AVP) ccmAvp;
								long digit = uvAvpDigit.getValue();
								legData.set(LegDataAttributes.DIAMETER_AOC_COST_INFO_CURRENCY_CODE, "" + digit);
							}
						}
					} // / cc money ends here
					}
				} // / cc money ends here
	}
	
	

	/**
	 * This method id used to fetch Requested Service Unit AVP
	 * 
	 * @param msccAvp
	 * @param legData
	 */
	private static void fetchRequestedServiceUnit(DiameterAVP msccAvp, LegData legData) {

		if (logger.isDebugEnabled()) {
			logger.debug("fetchRequestedServiceUnit");
		}

		if (msccAvp instanceof DiameterGroupedAVP) {

			DiameterGroupedAVP groupedRSUAVP = (DiameterGroupedAVP) msccAvp;
			List<DiameterAVP> groupedRSUAVPs = groupedRSUAVP.getValue();

			// <grouped>
			// <gavp name="CC-Time"/>
			// <gavp name="CC-Money"/>
			// <gavp name="CC-Total-Octets"/>
			// <gavp name="CC-Input-Octets"/>
			// <gavp name="CC-Output-Octets"/>
			// <gavp name="CC-Service-Specific-Units"/>
			// <gavp name="AVP"/>
			// </grouped>

			for (DiameterAVP rsuAvp : groupedRSUAVPs) {

				if (logger.isDebugEnabled()) {
					logger.debug("fetchRequestedServiceUnit read all grouped AVPS");
				}
				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Time) {
					DiameterUnsigned32AVP rsuAvpTime = (DiameterUnsigned32AVP) rsuAvp;
					long time = rsuAvpTime.getValue();
					legData.set(LegDataAttributes.DIAMETER_RSU_CC_TIME, "" + time);
				}

				// <grouped>
				// <gavp name="Unit-Value"/>
				// <gavp name="Currency-Code"/>
				// </grouped>

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Money) {
					DiameterGroupedAVP ccmAVP = (DiameterGroupedAVP) rsuAvp;
					List<DiameterAVP> groupedccmAVPs = ccmAVP.getValue();

					for (DiameterAVP ccmAvp : groupedccmAVPs) {

						if (ccmAvp.getCode() == DiameterAVPCodes.CC_Money_Unit_Value) {

							DiameterGroupedAVP ccmAVPs = (DiameterGroupedAVP) ccmAvp;
							// <grouped>
							// <gavp name="Value-Digits"/>
							// <gavp name="Exponent"/>
							// </grouped>
							for (DiameterAVP uvAvp : ccmAVPs.getValue()) {

								if (uvAvp.getCode() == DiameterAVPCodes.CC_Money_Value_digits) {
									DiameterInteger64AVP uvAvpDigit = (DiameterInteger64AVP) uvAvp;
									long digit = uvAvpDigit.getValue();
									legData.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_VALUE_DIGITS, "" + digit);
								}
								if (uvAvp.getCode() == DiameterAVPCodes.CC_Money_Exponent) {
									DiameterInteger32AVP uvAvpExp = (DiameterInteger32AVP) uvAvp;
									int exp = uvAvpExp.getValue();
									legData.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_EXPONENT, "" + exp);
								}

							}
						}

						if (ccmAvp.getCode() == DiameterAVPCodes.CC_Money_Currency_Code) {

							DiameterUnsigned32AVP uvAvpDigit = (DiameterUnsigned32AVP) ccmAvp;
							long digit = uvAvpDigit.getValue();
							legData.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_CURRENCY_CODE, "" + digit);
						}
					}
				} // / cc money ends here

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Total_Octets) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_RSU_CC_TOTAL_OCTETS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Input_Octets) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_RSU_CC_INPUT_OCTETS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Output_Octets) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_RSU_CC_OUTPUT_OCTETS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Service_Specific_Units) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_RSU_CC_SERVICE_SPECIFIC_UNITS, "" + bigI.doubleValue());
				}

			} // groupd requested service unit ends here

		}
	}

	/**
	 * This method is used to fetch Granted Service Unit
	 * 
	 * @param msccAvp
	 * @param legData
	 * @throws UnsupportedEncodingException
	 */
	private static void fetchGrantedServiceUnit(DiameterAVP msccAvp, LegData legData)
			throws UnsupportedEncodingException {

		if (logger.isDebugEnabled()) {
			logger.debug("fetchGrantedServiceUnit");
		}

		if (msccAvp instanceof DiameterGroupedAVP) {

			if (logger.isDebugEnabled()) {
				logger.debug("fetchGrantedServiceUnit fetch grouped VPS");
			}
			DiameterGroupedAVP groupedRSUAVP = (DiameterGroupedAVP) msccAvp;
			List<DiameterAVP> groupedRSUAVPs = groupedRSUAVP.getValue();
			// <gavp name="Tariff-Time-Change"/>
			// <gavp name="CC-Time"/>
			// <gavp name="CC-Money"/>
			// <gavp name="CC-Total-Octets"/>
			// <gavp name="CC-Input-Octets"/>
			// <gavp name="CC-Output-Octets"/>
			// <gavp name="CC-Service-Specific-Units"/>
			// <gavp name="AVP"/>
			// </grouped>

			for (DiameterAVP rsuAvp : groupedRSUAVPs) {

				if (logger.isDebugEnabled()) {
					logger.debug("fetchGrantedServiceUnit AVP fetched is  " + rsuAvp);
				}
				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Traffic_Time_Change) {

					DiameterOctetStringAVP rsuAvpTime = (DiameterOctetStringAVP) rsuAvp;
					byte[] time = rsuAvpTime.getValue();
					String s = new String(time, StandardCharsets.US_ASCII);
					legData.set(LegDataAttributes.DIAMETER_GSU_TARIFF_TIME_CHANGE, "" + s);
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Time) {
					DiameterUnsigned32AVP rsuAvpTime = (DiameterUnsigned32AVP) rsuAvp;
					long time = rsuAvpTime.getValue();
					legData.set(LegDataAttributes.DIAMETER_GSU_CC_TIME, "" + time);
				}

				// <grouped>
				// <gavp name="Unit-Value"/>
				// <gavp name="Currency-Code"/>
				// </grouped>

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Money) {
					DiameterGroupedAVP ccmAVP = (DiameterGroupedAVP) rsuAvp;
					List<DiameterAVP> groupedccmAVPs = ccmAVP.getValue();

					for (DiameterAVP ccmAvp : groupedccmAVPs) {

						if (ccmAvp.getCode() == DiameterAVPCodes.CC_Money_Unit_Value) {

							DiameterGroupedAVP ccmAVPs = (DiameterGroupedAVP) ccmAvp;
							// <grouped>
							// <gavp name="Value-Digits"/>
							// <gavp name="Exponent"/>
							// </grouped>
							for (DiameterAVP uvAvp : ccmAVPs.getValue()) {

								if (uvAvp.getCode() == DiameterAVPCodes.CC_Money_Value_digits) {
									DiameterInteger64AVP uvAvpDigit = (DiameterInteger64AVP) uvAvp;
									long digit = uvAvpDigit.getValue();
									legData.set(LegDataAttributes.DIAMETER_GSU_CC_MONEY_VALUE_DIGITS, "" + digit);
								}
								if (uvAvp.getCode() == DiameterAVPCodes.CC_Money_Exponent) {
									DiameterInteger32AVP uvAvpExp = (DiameterInteger32AVP) uvAvp;
									int exp = uvAvpExp.getValue();
									legData.set(LegDataAttributes.DIAMETER_GSU_CC_MONEY_EXPONENT, "" + exp);
								}

							}
						}

						if (ccmAvp.getCode() == DiameterAVPCodes.CC_Money_Currency_Code) {

							DiameterUnsigned32AVP uvAvpDigit = (DiameterUnsigned32AVP) ccmAvp;
							long digit = uvAvpDigit.getValue();
							legData.set(LegDataAttributes.DIAMETER_GSU_CC_MONEY_CURRENCY_CODE, "" + digit);
						}
					}
				} // / cc money ends here

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Total_Octets) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_GSU_CC_TOTAL_OCTETS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Input_Octets) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_GSU_CC_INPUT_OCTETS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Output_Octets) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_GSU_CC_OUTPUT_OCTETS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Service_Specific_Units) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_GSU_CC_SERVICE_SPECIFIC_UNITS, "" + bigI.doubleValue());
				}

			}
		} // granted service unit
	}

	/**
	 * This method is used to parse CCA AVPs
	 * 
	 * @param response
	 * @param callData
	 * @throws ResourceException
	 * @throws UnsupportedEncodingException
	 */
	public static void fetchCCAAVPs(CreditControlAnswer response, CallData callData)
			throws ResourceException, UnsupportedEncodingException {

		if (logger.isDebugEnabled()) {
			logger.debug("Entering fetchCCAAVPs");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.DIAMETER_IN_CCR_RES, response);
		legData.set(LegDataAttributes.DIAMETER_CCR_SESSION_ID, response.getSessionId());
		legData.set(LegDataAttributes.DIAMETER_DEST_REALM, response.getDestinationRealm());
		legData.set(LegDataAttributes.DIAMETER_DEST_HOST, response.getDestinationHost());
		legData.set(LegDataAttributes.DIAMETER_ORIGIN_REALM, response.getOriginRealm());
		legData.set(LegDataAttributes.DIAMETER_ORIGIN_HOST, response.getOriginHost());
		legData.set(LegDataAttributes.DIAMETER_CC_REQUEST_NUMBER, "" + response.getCCRequestNumber());
		legData.set(LegDataAttributes.DIAMETER_CC_REQUEST_TYPE, "" + response.getCCRequestType());

		DiameterAVP avp = response.getAvp(DiameterAVPCodes.Subscription_Id, DiameterConstants.VENDOR_ID_3GPP);

		List<DiameterAVP> msccInfoList = response.getAvp(DiameterAVPCodes.Mutil_Service_Credit_Control);

		if (logger.isDebugEnabled()) {
			logger.debug("Mutil_Service_Credit_Control is " + msccInfoList);
		}

		if (msccInfoList != null && !msccInfoList.isEmpty()) {

			DiameterAVP msccInfo = msccInfoList.get(0);

			fetchMultiServiceCreditControl(msccInfo, legData);
		}

		List<DiameterAVP> annInfo = response.getAvp(DiameterAVPCodes.Announcement_Information);

		if (logger.isDebugEnabled()) {
			logger.debug("Announcement_Information is " + annInfo);
		}
		if (annInfo != null && !annInfo.isEmpty()) {

			DiameterAVP gsuAvp = annInfo.get(0);
			fetchAnnouncementInformation(gsuAvp, legData);
		} // used service unit

		List<DiameterAVP> costInfo = response.getAvp(DiameterAVPCodes.Cost_Information);

		if (logger.isDebugEnabled()) {
			logger.debug("Cost_Information is " + annInfo);
		}
		if (costInfo != null && !costInfo.isEmpty()) {

			DiameterAVP gsuAvp = annInfo.get(0);
			fetchCostInformation(gsuAvp, legData);
		} // used service unit
		
		DiameterAVP AnnInstList = response.getAvp(DiameterAVPCodes.Announcement_Instructions,
				DiameterConstants.VENDOR_ID_ERICSSON);// DiameterConstants.VENDOR_3GPP);

		fetchAnnInstructions(AnnInstList, legData);


	}

	private static void fetchCostInformation(DiameterAVP gsuAvp, LegData legData) {
		// TODO Auto-generated method stub

	}

	/**
	 * This method is used to fetch MSCC AVP
	 * 
	 * @param msccInfo
	 * @param legData
	 * @throws UnsupportedEncodingException
	 */
	private static void fetchMultiServiceCreditControl(DiameterAVP msccInfo, LegData legData)
			throws UnsupportedEncodingException {

		if (logger.isDebugEnabled()) {
			logger.debug("fetchMultiServiceCreditControl");
		}
		// <grouped>
		// <gavp name="Granted-Service-Unit"/>
		// <gavp name="Requested-Service-Unit"/>
		// <gavp name="Used-Service-Unit"/>
		// <gavp name="Final-Unit-Indication"/>

		resetExistingMsccFields(legData);
		
		if (msccInfo == null) {
			return;
		}
		DiameterGroupedAVP groupedAVP = (DiameterGroupedAVP) msccInfo;
		List<DiameterAVP> msccInfoAvps = groupedAVP.getValue();

		for (DiameterAVP msccAvp : msccInfoAvps) {

			if (msccAvp.getCode() == DiameterAVPCodes.Requested_Service_Unit) {
				fetchRequestedServiceUnit(msccAvp, legData);
			}

			if (msccAvp.getCode() == DiameterAVPCodes.Granted_Service_Unit) {

				fetchGrantedServiceUnit(msccAvp, legData);
			} // granted service unit

			if (msccAvp.getCode() == DiameterAVPCodes.Used_Service_Unit) {

				// <grouped>
				// <gavp name="Reporting-Reason"/>
				// <gavp name="Tariff-Change-Usage"/>
				// <gavp name="CC-Time"/>
				// <gavp name="CC-Total-Octets"/>
				// <gavp name="CC-Input-Octets"/>
				// <gavp name="CC-Output-Octets"/>
				// <gavp name="CC-Service-Specific-Units"/>
				// <gavp name="Event-Charging-TimeStamp"/>
				// </grouped>
				fetchUsedServiceUnit(msccAvp, legData);

			} // granted service unit

			if (msccAvp.getCode() == DiameterAVPCodes.Final_Unit_Indication) {

				// <grouped>
				// <gavp name="Final-Unit-Action"/>
				// <gavp name="Restriction-Filter-Rule"/>
				// <gavp name="Filter-Id"/>
				// <gavp name="Redirect-Server"/>
				// </grouped>

				fetchFinalUnitIndication(msccAvp, legData);
			} // / final unit indication ends here
		}
	}

	private static void resetExistingMsccFields(LegData legData) {
		// TODO Auto-generated method stub
		
		legData.set(LegDataAttributes.DIAMETER_RSU_CC_INPUT_OCTETS, null);
		legData.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_CURRENCY_CODE, null);
		legData.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_EXPONENT, null);
		legData.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_VALUE_DIGITS, null);
		legData.set(LegDataAttributes.DIAMETER_RSU_CC_OUTPUT_OCTETS, null);
		legData.set(LegDataAttributes.DIAMETER_RSU_CC_SERVICE_SPECIFIC_UNITS, null);
		legData.set(LegDataAttributes.DIAMETER_RSU_CC_TIME, null);
		legData.set(LegDataAttributes.DIAMETER_RSU_CC_TOTAL_OCTETS,null);
		
		
		legData.set(LegDataAttributes.DIAMETER_USU_CC_INPUT_OCTETS,null);
		legData.set(LegDataAttributes.DIAMETER_USU_CC_OUTPUT_OCTETS, null);
		legData.set(LegDataAttributes.DIAMETER_USU_CC_SERVICE_SPECIFIC_UNITS, null);
		legData.set(LegDataAttributes.DIAMETER_USU_CC_TIME, null);
		legData.set(LegDataAttributes.DIAMETER_USU_CC_TOTAL_OCTETS, null);
		legData.set(LegDataAttributes.DIAMETER_USU_EVENT_CHARGING_TIMESTAMP, null);
		legData.set(LegDataAttributes.DIAMETER_USU_REPORTING_REASON, null);
		legData.set(LegDataAttributes.DIAMETER_USU_TARIFF_CHANGE_USAGE, null);
		
		
		legData.set(LegDataAttributes.DIAMETER_GSU_CC_INPUT_OCTETS, null);
		legData.set(LegDataAttributes.DIAMETER_GSU_CC_MONEY_CURRENCY_CODE, null);
		legData.set(LegDataAttributes.DIAMETER_GSU_CC_MONEY_EXPONENT, null);
		legData.set(LegDataAttributes.DIAMETER_GSU_CC_MONEY_VALUE_DIGITS, null);
		legData.set(LegDataAttributes.DIAMETER_GSU_CC_OUTPUT_OCTETS, null);
		legData.set(LegDataAttributes.DIAMETER_GSU_CC_SERVICE_SPECIFIC_UNITS, null);
		legData.set(LegDataAttributes.DIAMETER_GSU_CC_TIME, null);
		legData.set(LegDataAttributes.DIAMETER_GSU_CC_TOTAL_OCTETS, null);
		legData.set(LegDataAttributes.DIAMETER_GSU_TARIFF_TIME_CHANGE, null);
		
		legData.set(LegDataAttributes.DIAMETER_FUI_FILTER_ID, null);
		legData.set(LegDataAttributes.DIAMETER_FUI_FINAL_UNIT_ACTION, null);
		legData.set(LegDataAttributes.DIAMETER_FUI_REDIRECT_ADDRESS, null);
		legData.set(LegDataAttributes.DIAMETER_FUI_REDIRECT_ADDRESS_TYPE, null);
		legData.set(LegDataAttributes.DIAMETER_FUI_RESTRICTION_FILTER_RULE, null);	
	}

	/**
	 * This method is used to fetch final unit indication AVP
	 * 
	 * @param msccAvp
	 * @param legData
	 * @throws UnsupportedEncodingException
	 */
	private static void fetchFinalUnitIndication(DiameterAVP msccAvp, LegData legData)
			throws UnsupportedEncodingException {

		if (logger.isDebugEnabled()) {
			logger.debug("fetchFinalUnitIndication");
		}
		if (msccAvp instanceof DiameterGroupedAVP) {

			if (logger.isDebugEnabled()) {
				logger.debug("fetchFinalUnitIndication fetc grouped AVPS");
			}

			DiameterGroupedAVP groupedFUIAVP = (DiameterGroupedAVP) msccAvp;
			List<DiameterAVP> groupedFUIAVPs = groupedFUIAVP.getValue();

			for (DiameterAVP rsuAvp : groupedFUIAVPs) {

				if (rsuAvp.getCode() == DiameterAVPCodes.Final_Unit_Action) {
					DiameterInteger32AVP rsuAvpTime = (DiameterInteger32AVP) rsuAvp;
					String reason = ReportingReasonEnum.fromCode(rsuAvpTime.getValue()).name();
					legData.set(LegDataAttributes.DIAMETER_FUI_FINAL_UNIT_ACTION, "" + reason);
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.Restriction_Filter_Rule) {
					DiameterInteger32AVP rsuAvpTime = (DiameterInteger32AVP) rsuAvp;
					String reason = TariffChangeUsageEnum.fromCode(rsuAvpTime.getValue()).name();
					legData.set(LegDataAttributes.DIAMETER_FUI_RESTRICTION_FILTER_RULE, "" + reason);
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.Filter_Id) {
					DiameterInteger32AVP rsuAvpTime = (DiameterInteger32AVP) rsuAvp;
					int time = rsuAvpTime.getValue();
					legData.set(LegDataAttributes.DIAMETER_FUI_FILTER_ID, "" + time);
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.Redirect_Server) {

					DiameterGroupedAVP groupedRSAVP = (DiameterGroupedAVP) rsuAvp;
					List<DiameterAVP> groupedRSAVPs = groupedFUIAVP.getValue();

					for (DiameterAVP rsAvp : groupedRSAVPs) {

						if (rsAvp.getCode() == DiameterAVPCodes.Redirect_Address_Type) {
							DiameterInteger32AVP rsuAvpTime = (DiameterInteger32AVP) rsuAvp;
							String addType = RedirectAddressTypeEnum.fromCode(rsuAvpTime.getValue()).name();
							legData.set(LegDataAttributes.DIAMETER_FUI_REDIRECT_ADDRESS_TYPE, "" + addType);
						}

						if (rsAvp.getCode() == DiameterAVPCodes.Redirect_Server_Address) {

							if (rsAvp instanceof DiameterOctetStringAVP) {
								DiameterOctetStringAVP userNStr = (DiameterOctetStringAVP) rsAvp;
								byte[] userNStrB = userNStr.getValue();

								String s = new String(userNStrB, StandardCharsets.US_ASCII);
								legData.set(LegDataAttributes.DIAMETER_FUI_REDIRECT_ADDRESS, "" + s);
							}
						}
					}
				}
			}

		}

	}

	/**
	 * This method is used to fetch Used Service unit AVPS
	 * 
	 * @param msccAvp
	 * @param legData
	 * @throws UnsupportedEncodingException
	 */
	private static void fetchUsedServiceUnit(DiameterAVP msccAvp, LegData legData) throws UnsupportedEncodingException {

		if (logger.isDebugEnabled()) {
			logger.debug("fetchUsedServiceUnit");
		}
		if (msccAvp instanceof DiameterGroupedAVP) {

			if (logger.isDebugEnabled()) {
				logger.debug("fetchUsedServiceUnit fetch grouped AVPS");
			}
			DiameterGroupedAVP groupedRSUAVP = (DiameterGroupedAVP) msccAvp;
			List<DiameterAVP> groupedRSUAVPs = groupedRSUAVP.getValue();

			for (DiameterAVP rsuAvp : groupedRSUAVPs) {

				if (logger.isDebugEnabled()) {
					logger.debug("fetchUsedServiceUnit fetch AVP " + rsuAvp);
				}
				if (rsuAvp.getCode() == DiameterAVPCodes.Reporting_Reason) {
					DiameterInteger32AVP rsuAvpTime = (DiameterInteger32AVP) rsuAvp;
					String reason = ReportingReasonEnum.fromCode(rsuAvpTime.getValue()).name();
					legData.set(LegDataAttributes.DIAMETER_USU_REPORTING_REASON, "" + reason);
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.Tariff_Change_Usage) {
					DiameterInteger32AVP rsuAvpTime = (DiameterInteger32AVP) rsuAvp;
					String reason = TariffChangeUsageEnum.fromCode(rsuAvpTime.getValue()).name();
					legData.set(LegDataAttributes.DIAMETER_USU_TARIFF_CHANGE_USAGE, "" + reason);
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Time) {
					DiameterUnsigned32AVP rsuAvpTime = (DiameterUnsigned32AVP) rsuAvp;
					long time = rsuAvpTime.getValue();
					legData.set(LegDataAttributes.DIAMETER_USU_CC_TIME, "" + time);
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Total_Octets) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_USU_CC_TOTAL_OCTETS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Input_Octets) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_USU_CC_INPUT_OCTETS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Output_Octets) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_USU_CC_OUTPUT_OCTETS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.CC_Service_Specific_Units) {
					DiameterUnsigned64AVP rsuAvpTO = (DiameterUnsigned64AVP) rsuAvp;
					BigInteger bigI = rsuAvpTO.getValue();
					legData.set(LegDataAttributes.DIAMETER_USU_CC_SERVICE_SPECIFIC_UNITS, "" + bigI.doubleValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.Event_Charging_Timestamp) {
					DiameterOctetStringAVP rsuAvpTO = (DiameterOctetStringAVP) rsuAvp;
					byte[] ect = rsuAvpTO.getValue();

					String s = new String(ect, StandardCharsets.US_ASCII);
					// Date d=new Date(bigI);

					legData.set(LegDataAttributes.DIAMETER_USU_EVENT_CHARGING_TIMESTAMP, "" + s);
				}

			}

		}
	}

	/**
	 * This method is sued to fetch Ann information
	 * 
	 * @param AnnInfoAvp
	 * @param legData
	 * @throws UnsupportedEncodingException
	 */
	private static void fetchAnnouncementInformation(DiameterAVP AnnInfoAvp,

			LegData legData) throws UnsupportedEncodingException {

		if (logger.isDebugEnabled()) {
			logger.debug("fetchAnnouncementInformation");
		}
		if (AnnInfoAvp instanceof DiameterGroupedAVP) {

			if (logger.isDebugEnabled()) {
				logger.debug("fetchAnnouncementInformation fetch grouped AVPS");
			}
			DiameterGroupedAVP groupedRSUAVP = (DiameterGroupedAVP) AnnInfoAvp;
			List<DiameterAVP> groupedRSUAVPs = groupedRSUAVP.getValue();

			for (DiameterAVP rsuAvp : groupedRSUAVPs) {

				if (logger.isDebugEnabled()) {
					logger.debug("fetchAnnouncementInformation fetch AVP " + rsuAvp);
				}
				if (rsuAvp.getCode() == DiameterAVPCodes.Announcement_Identifier) {
					DiameterUnsigned32AVP rsuAvpTime = (DiameterUnsigned32AVP) rsuAvp;

					legData.set(LegDataAttributes.DIAMETER_ANN_IDENTIFIER, "" + rsuAvpTime.getValue());
				}

				if (rsuAvp.getCode() == DiameterAVPCodes.Ann_Time_Indicator) {
					DiameterUnsigned32AVP rsuAvpTime = (DiameterUnsigned32AVP) rsuAvp;
					legData.set(LegDataAttributes.DIAMETER_ANN_TIME_INDICATOR, "" + rsuAvpTime.getValue());
				}

				// rest will do TODO

			}

		}
	}

	// String s = "some text here";
	// byte[] b = s.getBytes(StandardCharsets.UTF_8);
	// Convert from byte[] to String:
	//
	// byte[] b = {(byte) 99, (byte)97, (byte)116};
	// String s = new String(b, StandardCharsets.US_ASCII);
}
