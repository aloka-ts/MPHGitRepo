package com.agnity.ph.diameter;

import org.apache.log4j.Logger;

import com.baypackets.ase.ra.diameter.ro.CreditControlRequest;
import com.baypackets.ase.resource.ResourceException;


public class CCRParser_NotUsed {
	
	private static CCRParser_NotUsed ccrParser = new CCRParser_NotUsed();
	private static Logger logger = Logger.getLogger(CCRParser_NotUsed.class);

	private CCRParser_NotUsed() {
	};

	public static CCRParser_NotUsed getParser() {
		return ccrParser;
	}
	
	
	// <--- generic method section starts --->
		public CCRAVPAttributes_NotUsed parseCCR(CreditControlRequest request) throws ResourceException {
			String methodname = "parseCCR()";

			if (logger.isDebugEnabled()) {
				logger.debug(" insize methodname");
				}
			CCRAVPAttributes_NotUsed avpAttributes = new CCRAVPAttributes_NotUsed();
//
//			String sessionId = request.getSessionId();
//			String originHost = request.getOriginHost();
//			String originRealm = request.getOriginRealm();
//			String destnRealm = request.getDestinationRealm();
//			String destnHost = request.getDestinationHost();
//			long requestNo = request.getCCRequestNumber();
//			long authAppId = request.getAuthApplicationId();
//			String userName = request.getUserName();
//			long originStateId = request.getOriginStateId();
//			Date eventTimestamp = request.getEventTimestamp();
//			String serviceCtxtId = request.getServiceContextId();
//
//			HashMap<SubscriptionIdTypeEnum, String> subsIdMap = new HashMap<SubscriptionIdTypeEnum, String>();
//			int subsIdType;
//			String subsIdData;
//			SubscriptionIdAvp subsIdAvpArr[] = request.getGroupedSubscriptionIds();
//			for (SubscriptionIdAvp subsIdAvp : subsIdAvpArr) {
//				subsIdType = subsIdAvp.getSubscriptionIdType();
//				subsIdData = subsIdAvp.getSubscriptionIdData();
//				subsIdMap.put(SubscriptionIdTypeEnum.fromCode(subsIdType), subsIdData);
//			}
//
//			int terminationCause = 0;
//			try {
//				terminationCause=request.getTerminationCause();
//			} catch (Exception e) {
//				logger.error("Error in parsing termination cause: " + e.getMessage());
//			}
//
//			int requestedAction = 0;
//			try {
//				requestedAction=request.getRequestedAction();
//			} catch (Exception e) {
//				logger.error("Error in parsing requested action: " + e.getMessage());
//			}
//
//			int multipleServicesIndicator = -1;
//			try {
//				if (request.getMultipleServicesIndicator() != 0) {
//					multipleServicesIndicator = request.getMultipleServicesIndicator();
//				}
//			} catch (Exception e) {
//				logger.error("Error in parsing multiple services indicator: " + e.getMessage());
//			}
//
//			long serviceIdentifier = -1L;
//			long ratingGroup = -1L;
//			long ccTimeRequested = 0L;
//			long ccTotalOctets = -1L;
//			long ccTimeUsed = 0L;
//			int reportingReasonUsed = 0;
//
//			MultipleServicesCreditControlAvp multiServicesCC = request.getGroupedMultipleServicesCreditControls()[0];
//
//			if (multiServicesCC != null) {
//
//				try {
//					if (multiServicesCC.getServiceIdentifiers() != null) {
//						serviceIdentifier = multiServicesCC.getServiceIdentifiers()[0];
//					}
//				} catch (Exception e) {
//					logger.error("Error in parsing service identifiers: " + e.getMessage());
//				}
//
//				try {
//					if (multiServicesCC.getRatingGroup() != 0) {
//						ratingGroup = multiServicesCC.getRatingGroup();
//					}
//				} catch (Exception e) {
//					logger.error("Error in parsing rating group: " + e.getMessage());
//				}
//			}
//
//			try {
//				RequestedServiceUnitAvp requestedServiceUnit = multiServicesCC.getGroupedRequestedServiceUnit();
//				if (requestedServiceUnit != null) {
//					ccTimeRequested = requestedServiceUnit.getCCTime();
//
//					try {
//						if (requestedServiceUnit.getCCTotalOctets() != 0) {
//							ccTotalOctets = requestedServiceUnit.getCCTotalOctets();
//						}
//					} catch (Exception e) {
//						logger.error("Error in parsing total octets: " + e.getMessage());
//					}
//				}
//			} catch (Exception e) {
//				logger.error("Error in parsing requested service unit: " + e.getMessage());
//			}
//			try {
//				UsedServiceUnitAvp usedServiceUnit = multiServicesCC.getGroupedUsedServiceUnits()[0];
//				if (usedServiceUnit != null) {
//					try {
//						reportingReasonUsed = usedServiceUnit.getReportingReason();
//					} catch (Exception e) {
//						logger.error("Error in parsing used reporting reason: " + e.getMessage());
//					}
//					try {
//						ccTimeUsed = usedServiceUnit.getCCTime();
//					} catch (Exception e) {
//						logger.error("Error in parsing used cc time: " + e.getMessage());
//					}
//				}
//			} catch (Exception e) {
//				logger.error("Error in parsing used service unit: " + e.getMessage());
//			}
//			int reportingReason = 0;
//			try {
//				reportingReason=multiServicesCC.getReportingReasons()[0];
//			} catch (Exception e) {
//				logger.error("Error in parsing reporting reason: " + e.getMessage());
//			}
//			int triggerType = 0;
//			try {
//				TriggerAvp trigger = multiServicesCC.getGroupedTrigger();
//				triggerType=trigger.getTriggerTypes()[0];
//			} catch (Exception e) {
//				logger.error("Error in parsing trigger: " + e.getMessage());
//			}
//
//			int roleOfNode = -1;
//			String userSessionId = "";
//			String cgpn = "";
//			String cdpn = "";
//			String calledAssertedId = "";
//			String chargingId = "";
//			String origIOI = "";
//			String termIOI = "";
//			Date sipRequestTimestamp = null;
//			Date sipResponseTimestamp = null;
//			try {
//				ServiceInformationAvp serviceInfo = request.getGroupedServiceInformation();
//				if (serviceInfo != null) {
//					IMSInformationAvp IMSInfo = serviceInfo.getGroupedIMSInformation();
//					if (IMSInfo != null) {
//						try {
//							roleOfNode = IMSInfo.getRoleOfNode();
//						} catch (Exception e) {
//							logger.error("Error in parsing role of node: " + e.getMessage());
//						}
//						try {
//							userSessionId = IMSInfo.getUserSessionId();
//						}  catch (Exception e) {
//							logger.error("Error in parsing user session Id: " + e.getMessage());
//						}
//						try {
//							cgpn = (IMSInfo.getCallingPartyAddresss())[0];
//						}  catch (Exception e) {
//							logger.error("Error in parsing calling party address: " + e.getMessage());
//						}
//						try {
//							cdpn = IMSInfo.getCalledPartyAddress();
//						}
//						catch (Exception e) {
//							logger.error("Error in parsing called party address: " + e.getMessage());
//						}
//						try {
//							calledAssertedId = IMSInfo.getCalledAssertedIdentitys()[0];
//						} catch (Exception e) {
//							logger.error("Error in parsing called asserted Id: " + e.getMessage());
//						}
//						try {
//							chargingId = IMSInfo.getIMSChargingIdentifier();
//						} catch (Exception e) {
//							logger.error("Error in parsing charging Id: " + e.getMessage());
//						}
//						try {
//							InterOperatorIdentifierAvp interOperatorId = IMSInfo.getGroupedInterOperatorIdentifiers()[0];
//							if (interOperatorId != null) {
//								origIOI = interOperatorId.getOriginatingIOI();
//								termIOI = interOperatorId.getTerminatingIOI();
//							}
//						} catch (Exception e) {
//							logger.error("Error in parsing inter operator Id: " + e.getMessage());
//						}
//						try {
//							TimeStampsAvp SIPTimestamps = IMSInfo.getGroupedTimeStamps();
//							if (SIPTimestamps != null) {
//								try {
//									sipRequestTimestamp = SIPTimestamps.getSIPRequestTimestamp();
//									sipResponseTimestamp = SIPTimestamps.getSIPResponseTimestamp();
//								} catch (Exception e) {
//									logger.error("Error in parsing SIP request/response: " + e.getMessage());
//								}
//							}
//						} catch (Exception e) {
//							logger.error("Error in parsing SIP timestamp: " + e.getMessage());
//						}
//					}
//				}
//			} catch (Exception e) {
//				logger.error("Error in parsing service information: " + e.getMessage());
//			}
//
//			if(cgpn.startsWith("sip")) {
//				cgpn = cgpn.substring(4, cgpn.indexOf('@'));
//			} else if (cgpn.startsWith("tel")) {
//				int idx = cgpn.indexOf(';');
//				if (idx == -1) {
//					cgpn = cgpn.substring(4);
//				} else {
//					cgpn = cgpn.substring(4, idx);
//				}
//			}
//
//			if(cdpn.startsWith("sip")) {
//				cdpn = cdpn.substring(4, cdpn.indexOf('@'));
//			} else if (cdpn.startsWith("tel")) {
//				int idx = cdpn.indexOf(';');
//				if (idx == -1) {
//					cdpn = cdpn.substring(4);
//				} else {
//					cdpn = cdpn.substring(4, idx);
//				}
//			}
//
//			String subsId = subsIdMap.get(SubscriptionIdTypeEnum.END_USER_SIP_URI);
//			if (subsId != null && !"".equals(subsId)) {
//				subsId = subsId.substring(4, subsId.indexOf('@'));
//			}
//
//			int aocRequestType = 0;
//			try {
//				aocRequestType = request.getAoCRequestType();
//			} catch (Exception e) {
//				logger.error("Error in parsing AoC: " + e.getMessage());
//			}
//
//			avpAttributes.setAttribute(CCRAVPAttributes.sessionId_str,sessionId);
//			avpAttributes.setAttribute(CCRAVPAttributes.originHost_str,originHost);
//			avpAttributes.setAttribute(CCRAVPAttributes.originRealm_str,originRealm);
//			avpAttributes.setAttribute(CCRAVPAttributes.destnRealm_str,destnRealm);
//			avpAttributes.setAttribute(CCRAVPAttributes.destnHost_str,destnHost);
//			avpAttributes.setAttribute(CCRAVPAttributes.requestNo_long,requestNo);
//			avpAttributes.setAttribute(CCRAVPAttributes.authAppId_long,authAppId);
//			avpAttributes.setAttribute(CCRAVPAttributes.userName_str,userName);
//			avpAttributes.setAttribute(CCRAVPAttributes.originStateId_long,originStateId);
//			avpAttributes.setAttribute(CCRAVPAttributes.eventTimestamp_date,eventTimestamp);
//			avpAttributes.setAttribute(CCRAVPAttributes.serviceCtxtId_str,serviceCtxtId);
//			avpAttributes.setAttribute(CCRAVPAttributes.subsIdMap,subsIdMap);
//			avpAttributes.setAttribute(CCRAVPAttributes.terminationCause_int,terminationCause);
//			avpAttributes.setAttribute(CCRAVPAttributes.requestedAction_int,requestedAction);
//			avpAttributes.setAttribute(CCRAVPAttributes.multipleServicesIndicator_int,multipleServicesIndicator);
//			avpAttributes.setAttribute(CCRAVPAttributes.serviceIdentifier_long,serviceIdentifier);
//			avpAttributes.setAttribute(CCRAVPAttributes.ratingGroup_long,ratingGroup);
//			avpAttributes.setAttribute(CCRAVPAttributes.ccTimeRequested_long,ccTimeRequested);
//			avpAttributes.setAttribute(CCRAVPAttributes.ccTotalOctets_long,ccTotalOctets);
//			avpAttributes.setAttribute(CCRAVPAttributes.reportingReasonUsed_int,reportingReasonUsed);
//			avpAttributes.setAttribute(CCRAVPAttributes.ccTimeUsed_long,ccTimeUsed);
//			avpAttributes.setAttribute(CCRAVPAttributes.reportingReason_int,reportingReason);
//			avpAttributes.setAttribute(CCRAVPAttributes.triggerType_int,triggerType);
//			avpAttributes.setAttribute(CCRAVPAttributes.roleOfNode_int,roleOfNode);
//			avpAttributes.setAttribute(CCRAVPAttributes.userSessionId_str,userSessionId);
//			avpAttributes.setAttribute(CCRAVPAttributes.cgpn_str,cgpn);
//			avpAttributes.setAttribute(CCRAVPAttributes.cdpn_str,cdpn);
//			avpAttributes.setAttribute(CCRAVPAttributes.calledAssertedId_str,calledAssertedId);
//			avpAttributes.setAttribute(CCRAVPAttributes.chargingId_str,chargingId);
//			avpAttributes.setAttribute(CCRAVPAttributes.origIOI_str,origIOI);
//			avpAttributes.setAttribute(CCRAVPAttributes.termIOI_str,termIOI);
//			avpAttributes.setAttribute(CCRAVPAttributes.subscriptionId_str,subsId);
//			avpAttributes.setAttribute(CCRAVPAttributes.sipRequestTimestamp_date,sipRequestTimestamp);
//			avpAttributes.setAttribute(CCRAVPAttributes.sipResponseTimestamp_date,sipResponseTimestamp);
//			avpAttributes.setAttribute(CCRAVPAttributes.aocRequestType_int,aocRequestType);
//
//			if(logger.isDebugEnabled()){
//				logger.debug("Leaving..."+ methodname);
//			}
			return avpAttributes;
		}

}
