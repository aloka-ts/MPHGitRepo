package com.agnity.ph.sip.isup;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import javax.activation.DataHandler;
import javax.mail.BodyPart;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMultipart;
import javax.mail.util.ByteArrayDataSource;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipServletMessage;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;
import javax.servlet.sip.SipSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.MultipartBody;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.SignalingTypeEnum;
import com.agnity.mphdata.common.SignalingTypeEnum.SIGNALING_TYPE;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.sip.SipProtocolConfig;
import com.agnity.ph.sip.SipProtocolRelReasonCode;
import com.agnity.ph.sip.SipProtocolUtil;
import com.genband.isup.datatypes.CalledPartyNum;
import com.genband.isup.datatypes.CallingPartyNum;
import com.genband.isup.datatypes.Cause;
import com.genband.isup.datatypes.GenericNumber;
import com.genband.isup.datatypes.NatOfConnIndicators;
import com.genband.isup.datatypes.UserServiceInfo;
import com.genband.isup.enumdata.AddPrsntRestEnum;
import com.genband.isup.enumdata.CalgPartyCatgEnum;
import com.genband.isup.enumdata.ChargeIndEnum;
import com.genband.isup.enumdata.CodingStndEnum;
import com.genband.isup.enumdata.ContCheckIndEnum;
import com.genband.isup.enumdata.EchoContDeviceIndEnum;
import com.genband.isup.enumdata.IntNwNumEnum;
import com.genband.isup.enumdata.NatureOfAddEnum;
import com.genband.isup.enumdata.NumIncmpltEnum;
import com.genband.isup.enumdata.NumPlanEnum;
import com.genband.isup.enumdata.NumQualifierIndEnum;
import com.genband.isup.enumdata.SatelliteIndEnum;
import com.genband.isup.enumdata.ScreeningIndEnum;
import com.genband.isup.enumdata.bearercapability.InfoTrfrRateEnum;
import com.genband.isup.enumdata.bearercapability.InfoTrnsfrCapEnum;
import com.genband.isup.enumdata.bearercapability.LayerIdentifierEnum;
import com.genband.isup.enumdata.bearercapability.TransferModeEnum;
import com.genband.isup.enumdata.bearercapability.UserInfoLayer1ProtocolEnum;
import com.genband.isup.exceptions.InvalidInputException;
import com.genband.isup.messagetypes.ACMMessage;
import com.genband.isup.messagetypes.ANMMessage;
import com.genband.isup.messagetypes.CPGMessage;
import com.genband.isup.messagetypes.IAMMessage;
import com.genband.isup.operations.ISUPConstants;
import com.genband.isup.operations.ISUPOperationsCoding;

/**
 * This class is helper class which is used to create incoming and
 * outgoing SIP-T messages.
 * @author reeta
 *
 */
public class SipIsupHelper {

	private static Logger	logger	= Logger.getLogger(SipIsupHelper.class);

	/*
	 * Private construct to restrict the instance
	 * creation as all the methods of this class are static
	 */
	private  SipIsupHelper() {

	}

	private static final Set<Integer>	BUSY_SIP_ISUP_CAUSE		= new HashSet<Integer>();
	private static final Set<Integer>	NOANS_SIP_ISUP_CAUSE	= new HashSet<Integer>();

	static {
		//Sip and Isup busy codes
		BUSY_SIP_ISUP_CAUSE.add(17);
		BUSY_SIP_ISUP_CAUSE.add(18);
		BUSY_SIP_ISUP_CAUSE.add(27);
		BUSY_SIP_ISUP_CAUSE.add(63);		
		BUSY_SIP_ISUP_CAUSE.add(486);

		//Sip and Isup no-answer cause
		NOANS_SIP_ISUP_CAUSE.add(19);
		//TODO: Should we treat 480 as no-Answer
		NOANS_SIP_ISUP_CAUSE.add(480);
		NOANS_SIP_ISUP_CAUSE.add(408);
	}

	/**
	 * This method is used to check is the current/orig call leg is SIPT call or not. if it is
	 * SIP-T then the Signalling key is set as SIPT in its legdata
	 * @param sipRequest
	 * @param origLegCallId
	 * @return
	 * @throws Exception
	 */
	 public static boolean  isSiptCall(SipServletRequest sipRequest,String origLegCallId) throws Exception{
		 /*
		 * Store received ISUP content type for future reference, contentType should include version
		 */
			MultipartBody multipartIsupContent = SipIsupHelper.getMultipartIsupContent(sipRequest,
							origLegCallId);
			
			CallData callData=SipProtocolUtil.getCallData(sipRequest.getApplicationSession());
			LegData legData=SipProtocolUtil.getLegDataForSipSession(sipRequest.getApplicationSession(), sipRequest.getSession(), callData);
			if (multipartIsupContent != null) {
				String contentType = multipartIsupContent.getContentType();
			//	byte[] iamMessageBytes = multipartIsupContent.getContent();

				if (contentType.contains(PhConstants.APPLICATION_ISUP_VERSION)) {
					callData.set(CallDataAttribute.P_ISUP_CONTENT_TYPE, contentType);
				} else {
					callData.set(CallDataAttribute.P_ISUP_CONTENT_TYPE,
									PhConstants.APPLICATION_ISUP_WITH_VERSION);
				}
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Isup content type is "
									+ callData.get(CallDataAttribute.P_ISUP_CONTENT_TYPE));
				}
				/*
				 * Set signaling type for call leg data
				 */
				legData.set(LegDataAttributes.P_SIGNALING_TYPE_KEY,  SIGNALING_TYPE.SIP_T);
				return true;
			} else {
				/*
				 * Set same signaling type in call data for term leg
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Isup content is missing");
					logger.debug(origLegCallId + ":: Set signaling type to SIP");
				}
				/*
				 * Set signaling type for orig leg data
				 */
				legData.set(LegDataAttributes.P_SIGNALING_TYPE_KEY, SIGNALING_TYPE.SIP);
			}

			return false;	
	 }
	 
	 
	 /**
	  * This method is used to set content in term when term leg signaling key is SIP-T
	  * @param appSession
	  * @param termSipRequest
	  * @param sendSdp
	  * @throws MessagingException
	  * @throws InvalidInputException
	  * @throws Exception
	  */
	 public static void setMultipartContentTerm(SipApplicationSession appSession,SipServletRequest termSipRequest,boolean sendSdp) throws
	                                                          MessagingException, InvalidInputException, Exception{
		 
		 CallData callData = SipProtocolUtil.getCallData(appSession);
			String origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
			SipServletRequest origInvRequest = SipIsupHelper.getOrigInitialInvite(appSession);
		 LegData termLegData= SipProtocolUtil.getLegDataForConnectionType(appSession, ConnectionType.TERM_CONNECTION, callData);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Retrieve Content from mine multipart");
			}

			MimeMultipart mimeMultipart = (MimeMultipart) origInvRequest.getContent();
			Multipart leg2Multipart = null;
			BodyPart leg2BodyPart = null;
			for (int indx = 0; indx < mimeMultipart.getCount(); indx++) {
				BodyPart bodyPart = mimeMultipart.getBodyPart(indx);
				
				if (bodyPart.getContentType().startsWith(PhConstants.APPLICATION_ISUP)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Body part content type is ISUP");
					}
					String isupContentDisposition = bodyPart
						.getHeader(PhConstants.CONTENT_DISPOSITION_HDR) == null ? PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP : bodyPart
						.getHeader(PhConstants.CONTENT_DISPOSITION_HDR)[0];
					if (termLegData.get(LegDataAttributes.P_SIGNALING_TYPE_KEY) == SIGNALING_TYPE.SIP_T) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + ":: Term signaling is SIP_T");
							logger.debug(origLegCallId
											+ ":: form multipart message from leg 2 IAM message");
						}

						ByteArrayInputStream byteArrInpStream = (ByteArrayInputStream) bodyPart
							.getContent();
						int byteArrLen = byteArrInpStream.available();
						byte[] byteArr = new byte[byteArrLen];
						byteArrInpStream.read(byteArr, 0, byteArrLen);
						IAMMessage iamMessage = SipIsupParser.parseIam(callData, byteArr, false);
						byte[] leg2IamMsg = SipIsupParser.createLeg2Iam(callData, iamMessage);
						
						
						if (leg2Multipart == null) {
							leg2Multipart = formMultipartMessage(leg2IamMsg, bodyPart
								.getContentType(), isupContentDisposition);
						} else {
							    formMultipartMessage(leg2Multipart, leg2IamMsg, bodyPart
								.getContentType(), isupContentDisposition);
						}
					} else {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
											+ ":: Term signaling is not SIP_T, do nothing");
						}
					}
				} else if (bodyPart.getContentType().startsWith(PhConstants.APPLICATION_SDP)
								&& sendSdp) {
					if (logger.isDebugEnabled()) {
						logger
							.debug(origLegCallId
											+ ":: Body part content type is SDP and sendSdp flag is TRUE");
					}
					String sdpContentDisposition = bodyPart
						.getHeader(PhConstants.CONTENT_DISPOSITION_HDR) == null ? PhConstants.CONTENT_DISPOSITION_VALUE_FOR_SDP : bodyPart
						.getHeader(PhConstants.CONTENT_DISPOSITION_HDR)[0];
					if (leg2Multipart == null) {
						if (logger.isDebugEnabled()) {
							logger
								.debug(origLegCallId + ":: ISUP content not present in multipart");
						}
						if (termLegData.get(LegDataAttributes.P_SIGNALING_TYPE_KEY) == SIGNALING_TYPE.SIP_T) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId + ":: Term signaling is SIP_T");
								logger
									.debug(origLegCallId
													+ ":: form multipart message from body part content");
							}
							
							//reeta changed here for lvel 3 if no isup conetnt available in incoming invite bu it is required in outgoing thencreate from scratch
							
							leg2Multipart= formMultipartMessage(SipIsupParser.createIam(callData), bodyPart
									.getContentType(),PhConstants.CONTENT_DISPOSITION_VALUE_FOR_SDP);
//							leg2Multipart = formMultipartMessage(
//											(ByteArrayInputStream) bodyPart.getContent(), bodyPart
//												.getContentType(), sdpContentDisposition);
						} else {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId + ":: Term signaling is not SIP_T");
								logger
									.debug(origLegCallId
													+ ":: from body part message from body part content");
							}

							leg2BodyPart = formBodyPartMessage(
											(ByteArrayInputStream) bodyPart.getContent(), bodyPart
												.getContentType(), sdpContentDisposition);
						}
					} else {
						if (logger.isDebugEnabled()) {
							logger
								.debug(origLegCallId
												+ ":: ISUP content present so form multipart message from body part content");
						}
						formMultipartMessage(leg2Multipart,
										(ByteArrayInputStream) bodyPart.getContent(), bodyPart
											.getContentType(), sdpContentDisposition);
					}
				}
			}
			if (leg2Multipart != null && leg2Multipart.getCount() > 0) {
				termSipRequest.setContent(leg2Multipart, leg2Multipart.getContentType());
			} else if (leg2BodyPart != null) {
				termSipRequest.setContent(leg2BodyPart, leg2BodyPart.getContentType());
			}
		
	 }
	

	private static byte[] createIam(CallData callData) {
		// TODO Auto-generated method stub
		return null;
	}


	/**
	 * This method returns the initial invite sent to originating side
	 * @param appSession represents an instance of SipApplicationSession
	 * @return an instance of SipServletRequest
	 */	
	public static SipServletRequest getOrigInitialInvite(SipApplicationSession appSession) {
		return SipProtocolUtil.getOrigInitialInvite(appSession, SipProtocolUtil.getCallData(appSession));
	}

	/**
	 * This method returns the initial invite sent to originating side
	 * @param appSession represents an instance of SipApplicationSession
	 * @return an instance of SipServletRequest
	 */	
	public static SipServletRequest getTermInitialInvite(
			SipApplicationSession appSession) {
		return SipProtocolUtil.getInitialInvite(appSession, SipProtocolUtil
				.getLegDataForConnectionType(appSession,
						ConnectionType.TERM_CONNECTION,
						SipProtocolUtil.getCallData(appSession)));
	}


	/**
	 * This method sets the received ANM in session and return true/false based on whether ANM
	 * received or not
	 * @param sipResponse represents the instance of SipServletResponse
	 * @param origLegCallId represents the instance of String
	 * @return boolean representation of operation status
	 * @throws Exception
	 */
	public static boolean setReceivedAnm(SipServletResponse sipResponse, String origLegCallId)
					throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside setReceivedAnm");
		}
		//Store received ANM for future reference
		MultipartBody anmContent = getMultipartIsupContent(sipResponse, origLegCallId);
		if (anmContent != null && anmContent.getContent() != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: ANM received, store it for future reference");
			}

			CallData callData=SipProtocolUtil.getCallData(sipResponse.getApplicationSession());
			LegData legData = SipProtocolUtil.getLegDataForSipSession(sipResponse.getApplicationSession(),
					sipResponse.getSession(), callData);
			
			legData.set(LegDataAttributes.P_CURRENT_RECEIVED_ANM, anmContent);

			return true;
		}
		return false;
	}

	/**
	 * This method check a sip session variable to identify the message leg (orig or term)
	 * @param sipMessage represents the instance of SipServletMessage
	 * @return boolean representation of operation status
	 */
	public static ConnectionType getConnectionType(SipServletMessage sipMessage) {
		int connectionType = -1;
		SipSession sipSession = sipMessage.getSession();
		
		CallData callData=SipProtocolUtil.getCallData(sipSession.getApplicationSession());
		LegData legData = SipProtocolUtil.getLegDataForSipSession(sipSession.getApplicationSession(),
				sipSession, callData);
		ConnectionType connTypeInt = (ConnectionType)legData.get(LegDataAttributes.P_CONNECTION_TYPE);
		return connTypeInt;
	}
	/**
	 * This method retrieves the peer leg invite from SipServletMessage.
	 * @param sipMessage represents an instance of SipServletMessage
	 * @return an instance of SipServletRequest
	 */
	public static SipServletRequest getPeerLegInviteReq(SipServletMessage sipMessage) {
		
		if(sipMessage==null){
			return null;
		}
		SipApplicationSession appSession = sipMessage.getApplicationSession();
		SipServletRequest peerSipReq = null;
		ConnectionType connType=getConnectionType(sipMessage);
		
		if(connType.equals(ConnectionType.ORIG_CONNECTION)){
				peerSipReq = getTermInitialInvite(appSession);
		}else if(connType.equals(ConnectionType.TERM_CONNECTION)){
				peerSipReq = getOrigInitialInvite(appSession);
		}
		return peerSipReq;
	}


	/**
	 * This method stores the Error, Bye and Cancel message to peer session,
	 * so REL and reason header can be copied when required.
	 * @param sipMessage represents an instance of SipServletMessage
	 */	
	public static void setErrByeCanMsgOnOtherLeg(SipServletMessage sipMessage) {
		SipServletRequest peerSipReq = getPeerLegInviteReq(sipMessage);
		if (peerSipReq != null) {
			
			CallData callData=SipProtocolUtil.getCallData(peerSipReq.getApplicationSession());
			LegData legData = SipProtocolUtil.getLegDataForSipSession(peerSipReq.getApplicationSession(),
					peerSipReq.getSession(), callData);
			legData.set(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG, sipMessage);
		}
	}
	/**
	 * This method deletes the Error, Bye and Cancel message from peer session
	 * @param sipMessage represents an instance of SipServletMessage
	 */
	public static void removeErrByeCanMsgOnOtherLeg(SipServletMessage sipMessage) {
		SipServletRequest peerSipReq = getPeerLegInviteReq(sipMessage);
		if (peerSipReq != null) {
			
			CallData callData=SipProtocolUtil.getCallData(peerSipReq.getApplicationSession());
			LegData legData = SipProtocolUtil.getLegDataForSipSession(peerSipReq.getApplicationSession(),
					peerSipReq.getSession(), callData);
			legData.set(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG,null);
		}
	}


	/**
	 * This method creates the REL message for the leg which sipSession is passed as argument
	 * @param sipSession represents an instance of SipSession
	 * @param causeValue represents integer representation of causevalue 
	 * @return boolean representation of operation status
	 * @throws Exception
	 */
	public static MultipartBody createRelMessage(SipSession sipSession, int causeValue)
					throws Exception {
		SipApplicationSession appSession = sipSession.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating REL message");
		}

		if (leg2Data.get(LegDataAttributes.P_ISUP_CAUSE_CODE) != null) {
			causeValue = (Integer) leg2Data.get(LegDataAttributes.P_ISUP_CAUSE_CODE);
		} else if (callData.get(CallDataAttribute.P_ISUP_CAUSE_CODE) != null) {
			causeValue = (Integer) callData.get(CallDataAttribute.P_ISUP_CAUSE_CODE);
		}

		LegData legData = SipProtocolUtil.getLegDataForSipSession(sipSession.getApplicationSession(),
				sipSession, callData);
		SipServletMessage peerLegErrByeCanMsg=(SipServletMessage)legData.get(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG);

		String contentType = null;
		String contentDisposition = null;
		byte[] relMessage = null;
		if (peerLegErrByeCanMsg != null) {
			MultipartBody multipartIsupContent = getMultipartIsupContent(peerLegErrByeCanMsg,
							origLegCallId);
			if (multipartIsupContent != null) {
				contentType = multipartIsupContent.getContentType();
				contentDisposition = multipartIsupContent.getContentDisposition();
				relMessage = multipartIsupContent.getContent();
				//if ISUP content is not REL, ignore it
				if (relMessage[0] != 0x0c) {
					relMessage = null;
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Send REL received from peer leg");
					}
				}
			}
		}

		if (relMessage == null) {
			contentType = (String) callData.get(CallDataAttribute.P_ISUP_CONTENT_TYPE);
			relMessage = SipIsupParser.createRelWithCauseValue(causeValue, callData);
		}

		MultipartBody bodyPart = new MultipartBody(relMessage, contentType);
		bodyPart.setContentDisposition(contentDisposition);
		return bodyPart;
	}


	/**
	 * This method create success response for the INVITE received.
	 * @param sipRequest represents an instance of SipServletRequest
	 * @param sdpContent represents an instance of MultipartBody
	 * @param anmContent represents an instance of MultipartBody
	 * @return an instance of SipServletResponse
	 * @throws Exception
	 */
	public static SipServletResponse createOrigInviteSuccessResponse(SipServletRequest sipRequest) throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		
		LegData legData=(LegData) SipProtocolUtil.getLegDataForConnectionType(appSession, ConnectionType.ORIG_CONNECTION, callData);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		LegData termLegData=(LegData) SipProtocolUtil.getLegDataForConnectionType(appSession, ConnectionType.TERM_CONNECTION, callData);
		MultipartBody anmContent=(MultipartBody)termLegData.get(LegDataAttributes.P_CURRENT_RECEIVED_ANM);

		MultipartBody sdpContent=(MultipartBody)termLegData.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Creating success response for initial invite");
		}

		SipServletResponse successResponse = sipRequest.createResponse(SipServletResponse.SC_OK);

		if (SignalingTypeEnum.SIGNALING_TYPE.SIP_T.equals((SignalingTypeEnum.SIGNALING_TYPE) legData
			.get(LegDataAttributes.P_SIGNALING_TYPE_KEY))) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Orig leg is not SIP, form multipart with ANM");
			}

			byte[] anmMessage = null;
			String contentType = null;
			String isupContentDisposition = null;
			if (anmContent != null) {
				ANMMessage leg2Anm = SipIsupParser.parseAnm(callData, anmContent.getContent(), false);
				anmMessage = SipIsupParser.createLeg1AnmFromLeg2Anm(callData, leg2Anm);
				contentType = anmContent.getContentType();
				isupContentDisposition = anmContent.getContentDisposition();
			}

			if (anmMessage == null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Create new ANM as has not been received yet");
				}
				anmMessage = SipIsupParser.createAnm(callData,ChargeIndEnum.CHARGE); //ForMediaInteraction
				contentType = (String) callData.get(CallDataAttribute.P_ISUP_CONTENT_TYPE);
			}

			Multipart respMultipart = null;
			String sdpContentDisposition = sdpContent.getContentDisposition() == null ? PhConstants.CONTENT_DISPOSITION_VALUE_FOR_SDP : sdpContent
				.getContentDisposition();
			isupContentDisposition = isupContentDisposition == null ? PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP : isupContentDisposition;
			
			respMultipart = formMultipartMessage(sdpContent.getContent(), sdpContent
				.getContentType(), sdpContentDisposition);

			if (respMultipart == null) {
				/*
				 * Only Isup content is received from B-Leg and now only ISUP content need to be
				 * forwarded to A-Side
				 */
				respMultipart = formMultipartMessage(anmMessage, contentType, isupContentDisposition);
			} else {
				/*
				 * SDP+Isup content are received from B-Leg and now SDP+ISUP content need to be
				 * forwarded to A-Side
				 */
				formMultipartMessage(respMultipart, anmMessage, contentType, isupContentDisposition);
			}
			
			successResponse.setContent(respMultipart, respMultipart.getContentType());

		} else {
			successResponse.setContent(sdpContent.getContent(), sdpContent.getContentType());
		}

		String timerValue = (String) legData
				.get(LegDataAttributes.P_SESSION_EXPIRE_TIME);

		if (timerValue == null || timerValue.isEmpty()) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Session refresh is not supported for the call");
				logger.debug(origLegCallId + ":: Dont add header related to session refresh");
			}
			return successResponse;
		}

		/*
		 * Set Session-Expires
		 */
		successResponse.addHeader(PhConstants.SESSION_EXPIRE_HEADER, timerValue + "; "
						+ PhConstants.REFRESHER_TAG + "=" + PhConstants.REFRESHER_UAC);

		/*
		 * As per rfc 4028 Page 15, If the UAS sets the refresher parameter = "uac", then UAS
		 * must place a Require header into the 200 response with the value timer.
		 */

		//Set Require header
		successResponse.addHeader(PhConstants.REQUIRE_HEADER, PhConstants.SUPPORTED_TIMER);

		return successResponse;
	}


	/*
	 * This method creates the success response for the BYE received
	 */
	/**
	 * This method creates the success response for the BYE received
	 * @param sipRequest represents an instance of SipServletRequest
	 * @return an instance of SipServletResponse
	 * @throws Exception
	 */
	public static SipServletResponse createSuccessResponseBye(SipServletRequest sipRequest)
					throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		
		LegData legData=SipProtocolUtil.getLegDataForSipSession(appSession, sipRequest.getSession(), callData);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside createSuccessResponseBye");
		}
		SipServletResponse sipResponse = sipRequest.createResponse(SipServletResponse.SC_OK);

		//Set the ISUP content if signaling type is SIP-T 
		if (SignalingTypeEnum.SIGNALING_TYPE.SIP != (SignalingTypeEnum.SIGNALING_TYPE)legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Form multipart message from rlc msg byte array");
			}
			byte[] rlcMsgByteArr = SipIsupParser.createRlc(callData);

			Multipart multipart = formMultipartMessage(rlcMsgByteArr, (String) callData
				.get(CallDataAttribute.P_ISUP_CONTENT_TYPE), null);
			sipResponse.setContent(multipart, multipart.getContentType());
			// Content-Disposition
			sipResponse.addHeader(PhConstants.CONTENT_DISPOSITION_HDR,
							PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP);
		}

		return sipResponse;
	}

	/**
	 * This method creates the error response for
	 * the sip request provided as argument in this method
	 * @param sipRequest represents an instance of SipServletRequest
	 * @param causeValue represents integer equivalent of cause value
	 * @return an instance of SipServletResponse
	 * @throws Exception
	 */
	public static SipServletResponse createErrorResponse(SipServletRequest sipRequest,
					int causeValue, int sipStatusCode) throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		LegData legData=SipProtocolUtil.getLegDataForSipSession(appSession, sipRequest.getSession(), callData);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating Error response");
		}

		SipServletMessage peerLegErrByeCanMsg = (SipServletMessage)legData.get(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG);

		int statusCode = 487;
		if (peerLegErrByeCanMsg != null
						&& !(peerLegErrByeCanMsg.getMethod().equals(PhConstants.BYE_REQUEST) || peerLegErrByeCanMsg
							.getMethod().equals(PhConstants.CANCEL_REQUEST))) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
								+ ":: Create the same response as received from other leg");
			}
			statusCode = ((SipServletResponse) peerLegErrByeCanMsg).getStatus();
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Create response based on cause value");
			}
			if (sipStatusCode != -1) {
				statusCode = sipStatusCode;
			} else {
				statusCode = getStatusCodeForCauseValue(causeValue);
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Decided SIP response status code is " + statusCode);
		}

		SipServletResponse errorResponse = sipRequest.createResponse(statusCode);

		if (SignalingTypeEnum.SIGNALING_TYPE.SIP != (SignalingTypeEnum.SIGNALING_TYPE)legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set REL message as multipart content");
			}

			MultipartBody relMsg = createRelMessage(sipRequest.getSession(), causeValue);
			String contentDisposition = relMsg.getContentDisposition();
			Multipart multipart = formMultipartMessage(relMsg.getContent(), relMsg.getContentType(), contentDisposition);

			errorResponse.setContent(multipart, multipart.getContentType());

			// Content-Disposition
			if(contentDisposition == null){
				errorResponse.addHeader(PhConstants.CONTENT_DISPOSITION_HDR,
								PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP);
			}		

		}

		//Add reason header
		if (peerLegErrByeCanMsg != null
						&& peerLegErrByeCanMsg.getHeader(PhConstants.REASON_HEADER) != null) {
			errorResponse.addHeader(PhConstants.REASON_HEADER, peerLegErrByeCanMsg
				.getHeader(PhConstants.REASON_HEADER));
		} else {
			errorResponse.addHeader(PhConstants.REASON_HEADER,
							PhConstants.REASON_HEADER_VALUE_INITIAL + causeValue);
		}
		
		SipIsupHelper.removeErrByeCanMsgOnOtherLeg(SipIsupHelper.getPeerLegInviteReq(sipRequest));
		return errorResponse;
	}
		

	/**
	 * This method creates the cancel request for
	 * the invite request provided as argument in this method
	 * @param sipRequest represents an instance of SipServletRequest
	 * @param causeValue represents integer equivalent of cause value
	 * @return an instance of SipServletRequest
	 * @throws Exception
	 */
	public static SipServletRequest createCancelRequest(SipServletRequest sipRequest, int causeValue)
					throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData=SipProtocolUtil.getLegDataForSipSession(appSession, sipRequest.getSession(), callData);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating CANCEL request");
		}

		SipServletRequest cancelRequest = sipRequest.createCancel();

		SipServletMessage peerLegErrByeCanMsg = (SipServletMessage)legData.get(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG);

		if (SignalingTypeEnum.SIGNALING_TYPE.SIP != (SignalingTypeEnum.SIGNALING_TYPE)legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set REL message as multipart content");
			}

			MultipartBody relMsg = createRelMessage(sipRequest.getSession(), causeValue);
			String contentDisposition = relMsg.getContentDisposition();
			Multipart multipart = formMultipartMessage(relMsg.getContent(), relMsg.getContentType(), contentDisposition);

			cancelRequest.setContent(multipart, multipart.getContentType());

			// Content-Disposition
			if(contentDisposition == null){
				cancelRequest.addHeader(PhConstants.CONTENT_DISPOSITION_HDR,
								PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP);
			}
		}
		//Add reason header
		if (peerLegErrByeCanMsg != null
						&& peerLegErrByeCanMsg.getHeader(PhConstants.REASON_HEADER) != null) {
			cancelRequest.addHeader(PhConstants.REASON_HEADER, peerLegErrByeCanMsg
				.getHeader(PhConstants.REASON_HEADER));
		} else {
			cancelRequest.addHeader(PhConstants.REASON_HEADER,
							PhConstants.REASON_HEADER_VALUE_INITIAL + causeValue);
		}
		return cancelRequest;
	}

	/**
	 * This method create BYE request for the
	 * invite request provided as argument in this method
	 * @param sipSession represents an instance of SipSession
	 * @param causeValue represents integer equivalent of cause value
	 * @return an instance of SipServletRequest
	 * @throws Exception
	 */
	public static SipServletRequest createByeRequest(SipSession sipSession, int causeValue)
					throws Exception {
		SipApplicationSession appSession = sipSession.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData=SipProtocolUtil.getLegDataForSipSession(appSession, sipSession, callData);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating BYE request");
		}

		SipServletRequest byeRequest = sipSession.createRequest(PhConstants.BYE_REQUEST);

		SipServletMessage peerLegErrByeCanMsg = (SipServletMessage)legData.get(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG);
		String peerLegReasonHeader = peerLegErrByeCanMsg != null ? peerLegErrByeCanMsg
			.getHeader(PhConstants.REASON_HEADER) : null;

		/*
		 * SBTM-UAT-1242:
		 * 1. If BYE is received with REL from A-Leg, then transparently forward this REL to
		 * B-Leg in BYE
		 * 2. If BYE is received without REL but with Reason header, then send BYE to B
		 * Leg having Cause Value = Cause Value in Reason Header
		 * 3. If BYE is received without REL and without REASON header, then send BYE to B
		 * Leg having Cause Value = 31
		 */
		int causeFromReason = getCauseFromReasonHeader(peerLegReasonHeader);
		if (causeFromReason > 0) {
			causeValue = causeFromReason;
		}
		
		if (SignalingTypeEnum.SIGNALING_TYPE.SIP != (SignalingTypeEnum.SIGNALING_TYPE)legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set REL message as multipart content");
			}

			MultipartBody relMsg = createRelMessage(sipSession, causeValue);
			String contentDisposition = relMsg.getContentDisposition();
			Multipart multipart = formMultipartMessage(relMsg.getContent(), relMsg.getContentType(), contentDisposition);

			byeRequest.setContent(multipart, multipart.getContentType());
			// Content-Disposition
			if(contentDisposition == null){
				byeRequest.addHeader(PhConstants.CONTENT_DISPOSITION_HDR,
								PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP);
			}			

		}

		//Add reason header
		if (peerLegReasonHeader != null) {
			byeRequest.addHeader(PhConstants.REASON_HEADER, peerLegReasonHeader);
		} else {
			byeRequest.addHeader(PhConstants.REASON_HEADER, PhConstants.REASON_HEADER_VALUE_INITIAL
							+ causeValue);
		}
		return byeRequest;
	}
	/*
	 * Fix for SBTM-UAT-1242
	 */
	/**
	 * This method returns the "Cause Value" given in Reason header
	 * @param reasonHeader : Value of Reason Header
	 * @return
	 */
	public static int getCauseFromReasonHeader(String reasonHeader) {
		int causeValue = -1;
		if (reasonHeader == null || reasonHeader.isEmpty()) {
			return causeValue;
		}
		//Example of Reason header is Reason: Q.850;cause=16
		String[] reasonHeaderParams = reasonHeader.split(";");
		for (String param : reasonHeaderParams) {
			if (param.startsWith("cause=")) {
				String cause = param.substring(param.indexOf("cause=") + 6);
				if (cause != null && !cause.isEmpty()) {
					try {
						causeValue = Integer.parseInt(cause);
					} catch (Exception e) {
						logger.error("Failed to get CV from Reason Header=" + reasonHeader
										+ ". Error = " + e.getMessage());
						return causeValue;
					}
					
				}
			}
		}
		return causeValue;
	}

	/**
	 * This method creates 183 with CHG message.
	 * @param sipRequest represents an instance of SipServletRequest
	 * @param sdpContent represents an instance of MultipartBody
	 * @return  an instance of SipServletResponse
	 * @throws Exception
	 */
	public static SipServletResponse createChgProvResponse(SipServletRequest sipRequest,
					MultipartBody sdpContent) throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData=SipProtocolUtil.getLegDataForSipSession(appSession, sipRequest.getSession(), callData);
		SipSession sipSession = sipRequest.getSession();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Creating prov response for CHG message");
		}

		SipServletResponse chgProvResponse = sipRequest
			.createResponse(SipServletResponse.SC_SESSION_PROGRESS);

		if (SignalingTypeEnum.SIGNALING_TYPE.SIP != (SignalingTypeEnum.SIGNALING_TYPE)legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Orig leg is not SIP, form multipart with CHG");
			}

			byte[] chgMessage = SipIsupParser.createChg(callData);

			String contentType = (String) callData.get(CallDataAttribute.P_ISUP_CONTENT_TYPE);

			Multipart respMultipart = null;
			String sdpContentDisposition = sdpContent.getContentDisposition() == null ? PhConstants.CONTENT_DISPOSITION_VALUE_FOR_SDP : sdpContent
				.getContentDisposition();
			respMultipart = formMultipartMessage((byte[]) sdpContent.getContent(), sdpContent
				.getContentType(), sdpContentDisposition);
			
			if (respMultipart == null) {
				respMultipart = formMultipartMessage(chgMessage, contentType, PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP);
			} else {				
				formMultipartMessage(respMultipart, chgMessage, contentType, PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP);
			}
			
			chgProvResponse.setContent(respMultipart, respMultipart.getContentType());

		} else {
			chgProvResponse.setContent(sdpContent.getContent(), sdpContent.getContentType());
		}

		return chgProvResponse;
	}


	/**
	 * This method create the provisional response for the other leg
	 * from received provisional response.
	 * @param sipRequest represents an instance of SipServletRequest
	 * @param responseCode represents integer value of response code
	 * @param sdpContent represents an instance of MultipartBody
	 * @param isupContent represents an instance of MultipartBody
	 * @return an instance of SipServletResponse
	 * @throws Exception
	 */
	public static SipServletResponse createProvisionalResponse(SipServletRequest sipRequest,SipServletResponse response
					)
					throws Exception {
		SipServletResponse peerProvisionlResponse = null;
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		LegData legData=SipProtocolUtil.getLegDataForSipSession(appSession, sipRequest.getSession(), callData);
		LegData termlegData=SipProtocolUtil.getLegDataForConnectionType(appSession, ConnectionType.TERM_CONNECTION, callData);
		SipSession termSipSession = SipIsupHelper.getTermInitialInvite(appSession).getSession();
		int responseCode=response.getStatus();
		
		boolean serviceInterestedInNoAnswer = PhConstants.TRUE.equals(termSipSession
			.getAttribute(Integer.toString(PhConstants.CAUSE_CODE_NOTAVAIL)));

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside createProvisionalResponse");
		}

		boolean isRingingResponseSent = PhConstants.TRUE.equals(legData.get(LegDataAttributes.P_IS_RINGING_SENT));

		boolean isAcmSentEarlier = PhConstants.TRUE.equals(legData.get(LegDataAttributes.P_IS_ACM_SENT));
		
		 MultipartBody sdpContent = (MultipartBody) termlegData.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
		 MultipartBody isupContent= getMultipartIsupContent(response, response.getCallId());

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: isAcmSent=" + isAcmSentEarlier + ", isRingingSent="
							+ isRingingResponseSent);
		}

		if ((responseCode == SipServletResponse.SC_RINGING) && isRingingResponseSent) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
								+ ":: Ringing already sent so create session in progress");
			}
			peerProvisionlResponse = sipRequest
				.createResponse(SipServletResponse.SC_SESSION_PROGRESS);
		} else {
			peerProvisionlResponse = sipRequest.createResponse(responseCode);
			if (responseCode == SipServletResponse.SC_RINGING) {
				legData.set(LegDataAttributes.P_IS_RINGING_SENT, PhConstants.TRUE);
			}
		}

		//Set the ISUP content if signaling type is SIP-T 
		if (SignalingTypeEnum.SIGNALING_TYPE.SIP != (SignalingTypeEnum.SIGNALING_TYPE)legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Create multipart content");
			}

			//For multipart message
			byte[] byteArrContent = null;
			String contentType = null;
			String isupContentDisposition = null;

			if (isupContent != null) {
				contentType = isupContent.getContentType();
				isupContentDisposition = isupContent.getContentDisposition();
				byteArrContent = (byte[]) isupContent.getContent();

				Object flexiChargeAppliedObj = callData.get(CallDataAttribute.P_FLEXI_CHRG_APPLIED);
				if (byteArrContent[0] == 0x06) {
					if (isAcmSentEarlier) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + ":: Create CPG from ACM received");
						}
						ACMMessage acmMessage = SipIsupParser
							.parseAcm(callData, byteArrContent, false);
						byteArrContent = SipIsupParser.createLeg1CpgFromLeg2Acm(callData, acmMessage,
										serviceInterestedInNoAnswer);
					} else {
						if (flexiChargeAppliedObj != null && flexiChargeAppliedObj.equals(PhConstants.TRUE)) {
							/*
							 * Bug#11434. If Flexible charging is applied then ACM/CPG to A-Party should
							 * contain Charge Indicator = No_Indication
							 */
							if (logger.isDebugEnabled()) {
								logger
									.debug(origLegCallId
													+ ":: Flexible charging applied so create ACM from ACM received");
							}
							ACMMessage acmMessage = SipIsupParser.parseAcm(callData, byteArrContent, false);
							byteArrContent = SipIsupParser.createLeg1AcmFromLeg2Acm(callData,
											acmMessage, serviceInterestedInNoAnswer);
						}
						legData.set(LegDataAttributes.P_IS_ACM_SENT, PhConstants.TRUE);
					}
				} else if (byteArrContent[0] == 0x2c) {
					if (flexiChargeAppliedObj != null && flexiChargeAppliedObj.equals(PhConstants.TRUE)) {
						/*
						 * Bug#11434. If Flexible charging is applied then ACM/CPG to A-Party should
						 * contain Charge Indicator = No_Indication
						 */
						if (logger.isDebugEnabled()) {
							logger
								.debug(origLegCallId
												+ ":: Flexible charging applied so create CPG from CPG received");
						}
						CPGMessage cpgMessage = SipIsupParser.parseCpg(callData, byteArrContent, false);
						byteArrContent = SipIsupParser.createLeg1CpgFromLeg2Cpg(callData, cpgMessage,
										serviceInterestedInNoAnswer);
					}
				}
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Set IS_BLEG_ACM_CPG_SENT flag to TRUE");
				}
				legData.set(LegDataAttributes.P_IS_BLEG_ACM_CPG_SENT, PhConstants.TRUE);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Received isup content null, so create new");
				}

				 contentType = (String) callData.get(CallDataAttribute.P_ISUP_CONTENT_TYPE);
				//As ISUP not received in provisional response from peer so create new ACM/CPG
				if (isAcmSentEarlier) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: ACM sent so create CPG");
					}

					byteArrContent = SipIsupParser.createLeg1CpgForAlternateRouting(callData);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Create ACM");
					}

					byteArrContent = SipIsupParser.createAcm(callData);//ForB2bua
					legData.set(LegDataAttributes.P_IS_ACM_SENT, PhConstants.TRUE);
				}
			}

			Multipart respMultipart = null;
			
			if (sdpContent != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: SDP content is not null so set in multipart");
				}
				String sdpContentDisposition = sdpContent.getContentDisposition() == null ? PhConstants.CONTENT_DISPOSITION_VALUE_FOR_SDP : sdpContent
								.getContentDisposition();
				respMultipart = formMultipartMessage(sdpContent.getContent(), sdpContent
					.getContentType(), sdpContentDisposition);
			}
			isupContentDisposition = isupContentDisposition == null ? PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP : isupContentDisposition;
					
			if (respMultipart == null) {
				respMultipart = formMultipartMessage(byteArrContent, contentType, isupContentDisposition);
			} else {
				formMultipartMessage(respMultipart, byteArrContent, contentType, isupContentDisposition);
			}
			
			peerProvisionlResponse.setContent(respMultipart, respMultipart.getContentType());
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set SDP only as signaing type is only sip");
			}

			if (sdpContent != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: SDP received from other leg is set");
				}

				peerProvisionlResponse.setContent(sdpContent.getContent(), sdpContent
					.getContentType());
			}
		}

		return peerProvisionlResponse;
	}
	
	
	/**
	 * This method is used to check if cause value is coming in ACM/CPG or not
	 * @param response
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static boolean checkforCauseValueInAcmCpg(SipServletResponse response,CallData callData) throws Exception{
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		 MultipartBody isupContent= getMultipartIsupContent(response, response.getCallId());
		
		 //For multipart message
		byte[] byteArrContent = null;
		String contentType = null;
		String isupContentDisposition = null;
		byte[] causeByte=null;
		Cause cause=null;

		if (isupContent != null) {
			contentType = isupContent.getContentType();
			isupContentDisposition = isupContent.getContentDisposition();
			byteArrContent = (byte[]) isupContent.getContent();

			if (byteArrContent[0] == 0x06) {
			
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Parse ACM received");
					}
					ACMMessage acmMessage = SipIsupParser
						.parseAcm(callData, byteArrContent, false);
					
				    cause=acmMessage.getCauseIndicators();
					causeByte=acmMessage.getCauseIndicatorsBytes();
				
			} else if (byteArrContent[0] == 0x2c) {
				
					if (logger.isDebugEnabled()) {
						logger
							.debug(origLegCallId
											+ ":: Parse CPG ");
					}
					CPGMessage cpgMessage = SipIsupParser.parseCpg(callData, byteArrContent, false);
					causeByte=cpgMessage.getCauseIndicatorBytes();
					
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: checkforCauseIndicatorInAcmCpg ");
			}		
		
			if(cause!=null || causeByte!=null){
				return true;
			}
			
			return false;
	}

	/*
	 * This method create the multipart message
	 */
	/**
	 * This method create the multipart message.
	 * @param content represents the instance of byte[]
	 * @param contentType represents the instance of String
	 * @param contentDisposition represents the instance of String
	 * @return an instance of Multipart
	 * @throws MessagingException
	 */
	public static Multipart formMultipartMessage(byte[] content, String contentType, String contentDisposition)
					throws MessagingException {
		Multipart multipart = new MimeMultipart();
		MimeBodyPart mimeBodyPart = new MimeBodyPart();
		ByteArrayDataSource ds = new ByteArrayDataSource(content, contentType);
		mimeBodyPart.setDataHandler(new DataHandler(ds));
		mimeBodyPart.setHeader(PhConstants.CONTENT_TYPE, contentType);
		if(contentDisposition != null){
			mimeBodyPart.setHeader(PhConstants.CONTENT_DISPOSITION_HDR, contentDisposition);
		}
		multipart.addBodyPart(mimeBodyPart);
		return multipart;
	}

	/**
	 * This method create the multipart message
	 * @param content represents the instance of ByteArrayInputStream
	 * @param contentType represents the instance of String
	 * @param contentDisposition represents the instance of String
	 * @return an instance of Multipart
	 * @throws MessagingException
	 * @throws IOException
	 */
	public static Multipart formMultipartMessage(ByteArrayInputStream content, String contentType, String contentDisposition)
					throws MessagingException, IOException {
		Multipart multipart = new MimeMultipart();
		MimeBodyPart mimeBodyPart = new MimeBodyPart();
		ByteArrayDataSource ds = new ByteArrayDataSource(content, contentType);
		mimeBodyPart.setDataHandler(new DataHandler(ds));
		mimeBodyPart.setHeader(PhConstants.CONTENT_TYPE, contentType);
		if(contentDisposition != null){
			mimeBodyPart.setHeader(PhConstants.CONTENT_DISPOSITION_HDR, contentDisposition);
		}
		multipart.addBodyPart(mimeBodyPart);
		return multipart;
	}


	/**	
	 * This method create the multipart message
	 * @param multipart represents the instance of Multipart
	 * @param content represents the instance of byte []
	 * @param contentType represents the instance of String
	 * @param contentDisposition represents the instance of String	 
	 * @throws MessagingException
	 */
	public static void formMultipartMessage(Multipart multipart, byte[] content, String contentType, String contentDisposition)
					throws MessagingException {
		MimeBodyPart mimeBodyPart = new MimeBodyPart();
		ByteArrayDataSource ds = new ByteArrayDataSource(content, contentType);
		mimeBodyPart.setDataHandler(new DataHandler(ds));
		mimeBodyPart.setHeader(PhConstants.CONTENT_TYPE, contentType);
		if(contentDisposition != null){
			mimeBodyPart.setHeader(PhConstants.CONTENT_DISPOSITION_HDR, contentDisposition);
		}
		multipart.addBodyPart(mimeBodyPart);
	}


	/**
	 * This method create the multipart message
	 * @param multipart represents the instance of Multipart
	 * @param content represents the instance ofByteArrayInputStream
	 * @param contentType represents the instance of String
	 * @param contentDisposition represents the instance of String	 
	 * @throws MessagingException
	 * @throws IOException
	 */
	public static void formMultipartMessage(Multipart multipart, ByteArrayInputStream content,
					String contentType, String contentDisposition) throws MessagingException, IOException {
		MimeBodyPart mimeBodyPart = new MimeBodyPart();
		ByteArrayDataSource ds = new ByteArrayDataSource(content, contentType);
		mimeBodyPart.setDataHandler(new DataHandler(ds));
		mimeBodyPart.setHeader(PhConstants.CONTENT_TYPE, contentType);
		if(contentDisposition != null){
			mimeBodyPart.setHeader(PhConstants.CONTENT_DISPOSITION_HDR, contentDisposition);
		}
		multipart.addBodyPart(mimeBodyPart);
	}


	/**
	 * This method create the multipart message
	 * @param content represents the instance ofByteArrayInputStream
	 * @param contentType represents the instance of String
	 * @param contentDisposition represents the instance of String
	 * @return an instance of BodyPart
	 * @throws MessagingException
	 * @throws IOException
	 */
	public static BodyPart formBodyPartMessage(ByteArrayInputStream content, String contentType, String contentDisposition)
					throws MessagingException, IOException {
		MimeBodyPart mimeBodyPart = new MimeBodyPart();
		ByteArrayDataSource ds = new ByteArrayDataSource(content, contentType);
		mimeBodyPart.setDataHandler(new DataHandler(ds));
		mimeBodyPart.setHeader(PhConstants.CONTENT_TYPE, contentType);
		if(contentDisposition != null){
			mimeBodyPart.setHeader(PhConstants.CONTENT_DISPOSITION_HDR, contentDisposition);
		}
		return mimeBodyPart;
	}

	

	/**
	 * This method returns the ISUP content form the message if present
	 * @param msg represents an instance of SipServletMessage
	 * @param callId represents an instance of String
	 * @return an instance of MultipartBody
	 * @throws Exception
	 */
	public static MultipartBody getMultipartIsupContent(SipServletMessage msg, String callId)
					throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: Inside getMultipartIsupContent");
		}

		BodyPart bodyPart = null;
		MultipartBody isupMultipartBody = null;

		if ((msg.getContentType() != null)
						&& (msg.getContentType().startsWith(PhConstants.MULTIPART_MIXED))) {
			if (logger.isDebugEnabled()) {
				logger.debug(callId + ":: Parse nultipart mixed content");
			}

			MimeMultipart mimeMultipart = (MimeMultipart) msg.getContent();
			for (int indx = 0; indx < mimeMultipart.getCount(); indx++) {
				bodyPart = mimeMultipart.getBodyPart(indx);
				if (logger.isDebugEnabled()) {
					logger.debug(callId + ":: Body part content type is "
									+ bodyPart.getContentType());
				}
				
				if (bodyPart.getContentType().startsWith(PhConstants.APPLICATION_ISUP)) {
					isupMultipartBody = new MultipartBody(SipProtocolUtil.getByteArrayFromStream(
									(ByteArrayInputStream) bodyPart.getContent(), callId), bodyPart
						.getContentType());
					String contentDisposition = bodyPart
						.getHeader(PhConstants.CONTENT_DISPOSITION_HDR) == null ? null : bodyPart
						.getHeader(PhConstants.CONTENT_DISPOSITION_HDR)[0];
					isupMultipartBody.setContentDisposition(contentDisposition);
					contentDisposition = null;
					break;
				}
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(callId + ":: Message content type is not multipart mixed");
			}
		}

		if (isupMultipartBody == null) {
			if (logger.isInfoEnabled()) {
				logger.info(callId + ":: No ISUP content found");
			}
		}

		return isupMultipartBody;
	}


	/**
	 * This method checks whether the error response is of type busy/no-answer
	 * @param sipResponse represents an instance of SipServletResponse
	 * @return an integer
	 */
	public static int isBusyNoAnswerError(SipServletResponse sipResponse) {
		SipApplicationSession appSession = sipResponse.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		LegData legData=SipProtocolUtil.getLegDataForSipSession(appSession, sipResponse.getSession(), callData);
		
		int cause=-1;

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: inside isBusyNoAnswerError");
		}
		/*
		 * Initialize the cause code based on sipResponse status,
		 * later it would be replaced during REL parsing.
		 */
		legData.set(LegDataAttributes.P_CAUSE_CODE, sipResponse.getStatus());

		try {
			MultipartBody multipartIsupContent = getMultipartIsupContent(sipResponse, origLegCallId);
			if (multipartIsupContent != null) {
				//String contentType = (String) multipartIsupContent.get(0);
				byte[] relContent = multipartIsupContent.getContent();
				SipIsupParser.parseRel(callData, relContent, true);
			}
		} catch (Exception ex) {
			logger.warn(origLegCallId + ":: Unable to parse REL", ex);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Identified error cause code is "
							+ legData.get(LegDataAttributes.P_CAUSE_CODE));
		}

		if (BUSY_SIP_ISUP_CAUSE.contains(legData.get(LegDataAttributes.P_CAUSE_CODE))) {
			legData.set(LegDataAttributes.P_CAUSE_CODE,PhConstants.CAUSE_CODE_BUSY);
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Error response is of type BUSY");
			}
			cause= PhConstants.CAUSE_CODE_BUSY;
		} else if (NOANS_SIP_ISUP_CAUSE.contains(legData.get(LegDataAttributes.P_CAUSE_CODE))) {
			legData.set(LegDataAttributes.P_CAUSE_CODE,PhConstants.CAUSE_CODE_NOTAVAIL);
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Error response is of type NO-ANSWER");
			}
			cause= PhConstants.CAUSE_CODE_NOTAVAIL;
		}
		
		int defaultCauseValue;//Fix for SBTM-UAT-903
		if (cause == PhConstants.CAUSE_CODE_NOTAVAIL) {
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,SipProtocolRelReasonCode.NOANS_RCVD_FROM_TERM);
			defaultCauseValue = 19;
		} else if (cause == PhConstants.CAUSE_CODE_BUSY) {
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,SipProtocolRelReasonCode.BUSY_RCVD_FROM_TERM);
			defaultCauseValue = 17;
		} else {
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,SipProtocolRelReasonCode.OTH_ERR_RCVD_FROM_TERM);
			defaultCauseValue = 41;
		}

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId + ":: Received Error is not in category of busy/no-answer");
		}
		
		if(callData.get(CallDataAttribute.NP_RELEASE_REASON_VALUE)!=null ){
			
			if((Integer)callData.get(CallDataAttribute.NP_RELEASE_REASON_VALUE)<=0){
		     callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, defaultCauseValue);
			}
		}

		return defaultCauseValue;
	}
	
	/**
	 * This utility method returns status code corresponding to the cause value specified.
	 * @param causeValue represents the integer equivalent of causevalue 
	 * @return integer value of cause code
	 */
	public static int getStatusCodeForCauseValue(int causeValue) {
		int statusCode = SipServletResponse.SC_REQUEST_TERMINATED;

		switch (causeValue) {
			case 1:
				statusCode = SipServletResponse.SC_NOT_FOUND;
				break;

			case 41:
			case 42:
			case 58:
			case 63:
			case 88:
				statusCode = SipServletResponse.SC_SERVICE_UNAVAILABLE;
				break;

			case 17:
				statusCode = SipServletResponse.SC_BUSY_HERE;
				break;

			case 28:
				statusCode = SipServletResponse.SC_ADDRESS_INCOMPLETE;
				break;

			case 79:
				statusCode = SipServletResponse.SC_NOT_IMPLEMENTED;
				break;

			case 19:
				statusCode = SipServletResponse.SC_TEMPORARLY_UNAVAILABLE;
				break;

			case 21:
			case 57:
				statusCode = SipServletResponse.SC_FORBIDDEN;
				break;

			case 96:
				statusCode = SipServletResponse.SC_SERVER_INTERNAL_ERROR;
				break;
		}

		return statusCode;
	}
	
	/**
	 * This method is used to create ANSI SIPT IAM message
	 * @param callData
	 * @param action
	 * @param origInvRequest
	 * @param termSipRequest
	 */
	public static void createSipTBodyPart(CallData callData, Action action,
			SipServletRequest origInvRequest, String fromUser, SipServletRequest termSipRequest) {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData termLegData = (LegData) callData.get(CallDataAttribute.valueOf(action.getLeg()));

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: In createSipIBodyPart ");
		}

		try {
			 /*
			  * Need to create Mandatory portion of IAM which contains following fields 
			  * Nature of connection indicators - Mandatory - 1 byte
			  * Forward call indicators - Mandatory - 2 byte
			  * calling party's category - Mandatory - 1 byte
			  * User service information  - Mandatory Variable - 2 to 11 [ Q.763, section 3.57]
			  * Called Party number - Mandatory Variable - 
			  * Create IAM buffer
			  */
			 
			IAMMessage iam = new IAMMessage(); 
			/*
			 *  Protocol 0 - ITU and 1- ANSI
			 */
			  String variant= SipProtocolConfig.getConfigData(SipProtocolConfig.ISUP_PROTOCOL_VARIANT);
			  
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: In createSipIBodyPart , use protocol variant is "+variant + " Note: 0 - ITU and 1- ANSI");
				}
			
			int protocolVar=variant!=null && !variant.isEmpty() ? Integer.parseInt(variant) : 1;
			iam.setProtocol(protocolVar);  

			/*
			 *  message Type
			 */
			byte [] field1 = new byte[1];
			field1[0] = (byte) 0x01;

			iam.setMessageType(field1);

			/*
			 *  Nature of Call Indicator
			 */
			SatelliteIndEnum satInd = SatelliteIndEnum.NO_SATELLITE;              // No Satellite
			ContCheckIndEnum contChk = ContCheckIndEnum.CONTINUITY_NOT_REQUIRED;             // Continuity check not required
			EchoContDeviceIndEnum echoCont = EchoContDeviceIndEnum.DEVICE_NOT_INCLUDED;  // Device not included

			byte [] nci = NatOfConnIndicators.encodeConnIndicators(satInd, contChk, echoCont);
			iam.setNatureOfConnIndicators(nci);

			//                  Forward Call Indicator
			//					NatIntNatCallIndEnum    val1 = NatIntNatCallIndEnum.fromInt(0); // 0 National
			//					EndToEndMethodIndEnum   val2 = EndToEndMethodIndEnum.fromInt(0); // No end to end method available
			//					InterNwIndEnum          val3 = InterNwIndEnum.fromInt(0); // No interworking required
			//					EndToEndInfoIndEnum     val4 = EndToEndInfoIndEnum.fromInt(0);
			//					ISDNUserPartIndEnum     val5 = ISDNUserPartIndEnum.fromInt(1); // ISDN user part used
			//					ISDNUserPartPrefIndEnum val6 = ISDNUserPartPrefIndEnum.fromInt(0); //ISDN preferred
			//					ISDNAccessIndEnum       val7 = ISDNAccessIndEnum.fromInt(0); //Originating access non-ISDN
			//					SCCPMethodIndENum       val8 = SCCPMethodIndENum.fromInt(0); //No indication

			/*
			 * byte [] fci = FwCallIndicators.encodeFwCallInd(val1, val2, val3, val4, val5, val6, val7, val8);
			 */
			byte [] fci = new byte[2];
			fci[0] = 0x20;
			fci[1] = 0x10;
			iam.setForwardCallIndicators(fci);

			/*
			 *  Calling party category
			 */
			
			String cpcFromHeader=(String) origLegData.get(LegDataAttributes.P_CPC_FROM_HEADER);
			
			CalgPartyCatgEnum cpc =null;
			
			if(cpcFromHeader!=null &&!cpcFromHeader.isEmpty()){

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: CPC from From header is "+cpcFromHeader);
				}
				cpc = CalgPartyCatgEnum.fromInt(Integer.parseInt(cpcFromHeader));
			}else{
				cpc = CalgPartyCatgEnum.ORD_SUBSR;
			}
			
			byte [] cpcByte = new byte[1];
			
			cpcByte[0] = (byte)cpc.getCode();
			iam.setCallingPartyCategory(cpcByte);
			
			/*
			 *  User Service Information
			 */
			InfoTrnsfrCapEnum ic = InfoTrnsfrCapEnum.SPEECH;
			CodingStndEnum    cs = CodingStndEnum.ITUT_STANDARDIZED_CODING;
			InfoTrfrRateEnum  ir = InfoTrfrRateEnum.KBITS_64;
			TransferModeEnum  tm = TransferModeEnum.CIRCUIT_MODE;
			LayerIdentifierEnum li = LayerIdentifierEnum.LAYER_1;
			UserInfoLayer1ProtocolEnum ui = UserInfoLayer1ProtocolEnum.RECOMMEND_G711_U; // 2 - G711 u law

			byte [] userSrvInfo = UserServiceInfo.encodeUserServiceInfo(ic, cs, ir, tm, li, ui, null, null);
			iam.setUserServiceInfoByte(userSrvInfo);

			// Called Party Number
			PhoneNumber destPhoneNum = (PhoneNumber) termLegData
					.get(LegDataAttributes.P_DESTINATION_NUMBER);

			destPhoneNum.setNatureOfAddress(NatureOfAddEnum.NATIONAL_NO.getCode());
			destPhoneNum.setNumberingPlan(NumPlanEnum.ISDN_NP.getCode());


			byte [] cldPtyNum = CalledPartyNum.encodeCaldParty(destPhoneNum.getAddress(), 
					NatureOfAddEnum.fromInt(destPhoneNum.getNatureOfAddress()), 
					NumPlanEnum.fromInt(destPhoneNum.getNumberingPlan()),
					IntNwNumEnum.ROUTING_ALLWD); 
			iam.setCalledPartyNumber(cldPtyNum);


			/*
			 *  Optional parameter Calling party Address
			 */
			PhoneNumber clgPtyNum = new PhoneNumber();
			
			//(PhoneNumber) origLegData.get(LegDataAttributes.P_CALLING_PARTY_FROM_HEADER);
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: In createSipIBodyPart, From user to be encoded as CallingPartyNumber is  "+fromUser);
			}
			clgPtyNum.setAddress(fromUser);

			clgPtyNum.setNatureOfAddress(NatureOfAddEnum.NATIONAL_NO.getCode());
			clgPtyNum.setNumberingPlan(1);

			byte [] clgPtyNumByte = CallingPartyNum.encodeCalgParty(clgPtyNum.getAddress(), 
					NatureOfAddEnum.fromInt(clgPtyNum.getNatureOfAddress()), 
					NumPlanEnum.fromInt(clgPtyNum.getNumberingPlan()), 
					AddPrsntRestEnum.fromInt(clgPtyNum.getPresentationIndicator()), 
					ScreeningIndEnum.NETWORK_PROVD,  NumIncmpltEnum.COMPLETE);

			iam.setCallingPartyNumber(clgPtyNumByte);


			/*
			 *  Generic Address/Number it will be DNIS set by application
			 */
			PhoneNumber origCalledPartyId = (PhoneNumber) termLegData
					.get(LegDataAttributes.NP_ORIG_CALLED_PARTY_NUMBER);

			origCalledPartyId.setNatureOfAddress(NatureOfAddEnum.NATIONAL_NO.getCode());
			origCalledPartyId.setNumberingPlan(NumPlanEnum.ISDN_NP.getCode());
			NumQualifierIndEnum numQualifierInd = NumQualifierIndEnum.fromInt(0);

			byte [] genNum = GenericNumber.encodeGenericNum
					(   numQualifierInd, 
							origCalledPartyId.getAddress(), 
							NatureOfAddEnum.fromInt(origCalledPartyId.getNatureOfAddress()), 
							NumPlanEnum.fromInt(origCalledPartyId.getNumberingPlan()),
							AddPrsntRestEnum.fromInt(origCalledPartyId.getPresentationIndicator()), 
							ScreeningIndEnum.USER_PROVD_NOT_VERFD, NumIncmpltEnum.COMPLETE);
			iam.setGenericNumber(genNum);


			/*
			 *  Encode IAM Message
			 */
			LinkedList<Object> operationObjs = new LinkedList<Object>();
			LinkedList<String> opCode = new LinkedList<String>();
			operationObjs.add(iam);
			opCode.add(ISUPConstants.OP_CODE_IAM);

			LinkedList<byte[]> encodeList = null;
			byte [] isupContent = null;
			encodeList = ISUPOperationsCoding
					.encodeOperations(operationObjs, opCode);

			if (encodeList != null){
				isupContent = encodeList.getFirst();
			}

			/*
			 *  Create Multipart MIME 
			 */
			Multipart multipart = new MimeMultipart();

			/*
			 * create SDP part
			 */
			MimeBodyPart sdpBodyPart = new MimeBodyPart();
			sdpBodyPart.setContent(origInvRequest.getContent(), origInvRequest.getContentType());
			sdpBodyPart.setHeader(PhConstants.CONTENT_TYPE, origInvRequest.getContentType());
			sdpBodyPart.setHeader(PhConstants.CONTENT_DISPOSITION_HDR, PhConstants.CONTENT_DISPOSITION_VALUE_FOR_SDP);

			multipart.addBodyPart(sdpBodyPart);

			
			String ansiVersion= SipProtocolConfig.getConfigData(SipProtocolConfig.ISUP_ANSI_VERSION);
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: In createSipIBodyPart, ISUP_ANSI_VERSION "+ansiVersion);
			}
			
			MimeBodyPart isupBodyPart = new MimeBodyPart();
			ByteArrayDataSource ds = new ByteArrayDataSource(isupContent, "application/isup; "+"version="+ansiVersion+"; base="+ansiVersion);
			isupBodyPart.setDataHandler(new DataHandler(ds));
			isupBodyPart.setHeader(PhConstants.CONTENT_TYPE, "application/isup; "+"version="+ansiVersion+"; base="+ansiVersion);

			isupBodyPart.setHeader(PhConstants.CONTENT_DISPOSITION_HDR, PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP);
			multipart.addBodyPart(isupBodyPart);

			/*
			 *  Set the body in termSip Request
			 */
			String finalContentType = multipart.getContentType();
			finalContentType = finalContentType.replaceAll("\\r|\\n|\\t|\\ ", "");

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: In createSipIBodyPart, multipart contentType:[" + 
						finalContentType + "]" + " OrigRequestContent: [" + origInvRequest.getContentType() + "] Leaving ");
			}

			termSipRequest.setContent(multipart, finalContentType);
			termSipRequest.addHeader("MIME-Version", "1.0");

		}
		catch(Exception ex) {
			logger.error("createSipIBodyPart Exception receieved " +ex);
		}
	}


}
