/*******************************************************************************
 *   Copyright (c) 2011 Agnity, Inc. All rights reserved.
 *   
 *   This is proprietary source code of Agnity, Inc. 
 *   
 *   Agnity, Inc. retains all intellectual property rights associated 
 *   with this source code. Use is subject to license terms.
 *   
 *   This source code contains trade secrets owned by Agnity, Inc.
 *   Confidentiality of this computer program must be maintained at 
 *   all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.ph.sip;

import java.util.List;
import java.util.ListIterator;

import javax.servlet.sip.Address;
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
import com.agnity.mphdata.common.MultiChoiceContact;
import com.agnity.mphdata.common.MultipartBody;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.SignalingTypeEnum;
import com.agnity.mphdata.common.TermRedirectionContact;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.isup.SipIsupHelper;

/**
 * This class is used to create out going sip messages
 *
 */
public class SipProtocolMessageCreator {

	private static Logger logger = Logger
			.getLogger(SipProtocolMessageCreator.class);

	/**
	 * This method create success response for the INVITE received
	 * 
	 * @param sipRequest
	 *            represents an instance of SipServletRequest
	 * @param sdpContent
	 *            represents an instance of MultipartBody
	 * @return an instance of SipServletResponse
	 * @throws Exception
	 */
	public static SipServletResponse createSuccessResponseInvite(
			SipServletRequest sipRequest, MultipartBody sdpContent)
			throws Exception {
		
		
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
				sipRequest.getSession(), callData);

		
		SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Creating success response for invite");
		}

		if (SignalingTypeEnum.SIGNALING_TYPE.SIP_T.equals(legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY))) {
			SipServletResponse successResponse = SipIsupHelper
					.createOrigInviteSuccessResponse(sipRequest);
			
			if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null){
				successResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
			}
			return successResponse;
		}

		SipServletResponse successResponse = sipRequest
				.createResponse(SipServletResponse.SC_OK);

		SipServletResponse sipPeerResponse =(SipServletResponse)appSession.getAttribute(PhConstants.INVITE_SUCCESS_RESPONSE);
		if (sipPeerResponse.getContent() != null) {
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: peer response content is not empty ");
			}
			
			if(sdpContent==null)
			   successResponse.setContent(sipPeerResponse.getContent() ,sipPeerResponse.getContentType()); 
			else{
				successResponse.setContent(sdpContent.getContent(),sdpContent.getContentType());
			}
			
		}else{
			
			if(sdpContent!=null){
			successResponse.setContent(sdpContent.getContent(),
					sdpContent.getContentType());
			}
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: send last sent sdp ");
			}
		}
		
//		if (sdpContent != null) {
//			successResponse.setContent(sdpContent.getContent(),
//					sdpContent.getContentType());
//		}
		
		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
		if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
			successResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
		}

		ListIterator<String> supportedHeaderIter = sipRequest
				.getHeaders(PhConstants.SUPPORTED_HEADER);
		if (supportedHeaderIter != null) {
			String supportedHeaderInvite = null;
			while (supportedHeaderIter.hasNext()) {
				supportedHeaderInvite = supportedHeaderIter.next();
				if (supportedHeaderInvite != null
						&& supportedHeaderInvite
								.contains(PhConstants.SUPPORTED_TIMER)) {
					String timerValue = (String) legData
							.get(LegDataAttributes.P_SESSION_EXPIRE_TIME);

					if (timerValue == null || timerValue.isEmpty()) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Session refresh is not supported for the call");
							logger.debug(origLegCallId
									+ ":: Dont add header related to session refresh");
						}
						return successResponse;
					}
					
					String origMinSEallowed = SipProtocolConfig.getConfigData(SipProtocolConfig.ORIG_ALLOWED_MIN_SE);
	
					long origMinSEAllowedValue = Long.parseLong(origMinSEallowed);
					
					long origMinSEValue = Long.parseLong(timerValue);
	
					if (origMinSEValue < origMinSEAllowedValue) {
						timerValue = "" + origMinSEAllowedValue;

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: orig session expiry value is lower than minimum allowd so sending configured value ");
						}
					}
							

					// Set Session-Expires
					successResponse.addHeader(
							PhConstants.SESSION_EXPIRE_HEADER, timerValue
									+ "; " + PhConstants.REFRESHER_TAG + "="
									+ PhConstants.REFRESHER_UAC);

					/*
					 * As per rfc 4028 Page 15, If the UAS sets the refresher
					 * parameter = "uac", then UAS must place a Require header
					 * into the 200 response with the value timer.
					 */

					// Set Require header
					successResponse.addHeader(PhConstants.REQUIRE_HEADER,
							PhConstants.SUPPORTED_TIMER);
					break;
				}
			}
		}

		return successResponse;
	}

	/**
	 * This method create BYE request for the invite request provided as
	 * argument in this method
	 * 
	 * @param sipSession
	 *            represents an instance of SipSession
	 * @param causeValue
	 * @param causeValue
	 *            represents integer equivalent of cause value
	 * @return an instance of SipServletRequest
	 * @throws Exception
	 */
	public static SipServletRequest createByeRequest(String origLegCallId,
			SipSession sipSession, int causeValue, boolean addcustomHeaders)
			throws Exception {

		SipServletRequest byeRequest = null;
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating BYE request");
		}
		CallData callData = SipProtocolUtil.getCallData(sipSession
				.getApplicationSession());
		LegData legData = SipProtocolUtil.getLegDataForSipSession(
				sipSession.getApplicationSession(), sipSession, callData);

		if (SignalingTypeEnum.SIGNALING_TYPE.SIP_T.equals(legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY))) {
			byeRequest = SipIsupHelper.createByeRequest(sipSession, causeValue);
		} else {
			byeRequest = sipSession.createRequest(PhConstants.BYE_REQUEST);
		}

		if (addcustomHeaders) {
			SipProtocolUtil.addCustomHeaders(callData, legData, byeRequest);
		}
		
		SipServletMessage peerLegErrByeCanMsg = (SipServletMessage)legData.get(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG);
		String peerLegReasonHeader = peerLegErrByeCanMsg != null ? peerLegErrByeCanMsg
			.getHeader(PhConstants.REASON_HEADER) : null;

			if (peerLegReasonHeader != null) {
				byeRequest.addHeader(PhConstants.REASON_HEADER, peerLegReasonHeader);
			} else {
				byeRequest.addHeader(PhConstants.REASON_HEADER, PhConstants.REASON_HEADER_VALUE_INITIAL
								+ "16;text=\"Terminated\"");
			}
			Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
			if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
				byeRequest.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
			}
			
			SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		return byeRequest;
	}

	/**
	 * This method creates the cancel request for the invite request provided as
	 * argument in this method
	 * 
	 * @param sipRequest
	 *            represents an instance of SipServletRequest
	 * @param causeValue
	 * @param causeValue
	 *            represents integer equivalent of cause value
	 * @return an instance of SipServletRequest
	 * @throws Exception
	 */
	public static SipServletRequest createCancelRequest(String origLegCallId,
			SipServletRequest sipRequest, int causeValue) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating CANCEL request");
		}

		CallData callData = SipProtocolUtil.getCallData(sipRequest
				.getApplicationSession());
		LegData legData = SipProtocolUtil.getLegDataForSipSession(
				sipRequest.getApplicationSession(), sipRequest.getSession(),
				callData);

		if (SignalingTypeEnum.SIGNALING_TYPE.SIP_T.equals(legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY))) {
			return SipIsupHelper.createCancelRequest(sipRequest, causeValue);
		}

		SipServletRequest cancelRequest = sipRequest.createCancel();
		
		SipServletMessage peerLegErrByeCanMsg = (SipServletMessage)legData.get(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG);
		String peerLegReasonHeader = peerLegErrByeCanMsg != null ? peerLegErrByeCanMsg
			.getHeader(PhConstants.REASON_HEADER) : null;

			if (peerLegReasonHeader != null) {
				cancelRequest.addHeader(PhConstants.REASON_HEADER, peerLegReasonHeader);
			} else {
				cancelRequest.addHeader(PhConstants.REASON_HEADER, PhConstants.REASON_HEADER_VALUE_INITIAL
								+ "16;text=\"Terminated\"");
			}
			
			Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
			if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
				cancelRequest.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
			}
			
			SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		return cancelRequest;
	}

	/**
	 * This method creates the cancel request for the invite request provided as
	 * argument in this method
	 * 
	 * @param sipRequest
	 *            represents an instance of SipServletRequest
	 * @param causeValue
	 *            represents integer equivalent of cause value
	 * @return an instance of SipServletResponse
	 * @throws Exception
	 */
	public static SipServletResponse createErrorResponse(
			SipServletRequest sipRequest,Action action) throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		
		SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
				sipRequest.getSession(), callData);
		
		int statusCode = Integer.parseInt(SipProtocolUtil.getConfig(SipProtocolConfig.DEFAULT_STATUS_CODE));
		
		SipServletResponse errorResponse = null;
		
		if (legData == null) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Leg data not found send the default response");
			}

			errorResponse = sipRequest.createResponse(statusCode);
			Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
			if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
				errorResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
			}
			return errorResponse;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating Error response");
		}
		
		int causeCode=0;
		
		if (action.getReleaseCauseValue() > 0) {
			causeCode = action.getReleaseCauseValue();
		}

		//String causeCodeStr = action.getReleaseCauseText();

		if (causeCode <= 0) {
			Object causeCodeObj = legData.get(LegDataAttributes.P_CAUSE_CODE);

			if (causeCodeObj != null) {
				if (causeCodeObj instanceof Short) {
					causeCode = (Short) causeCodeObj;
				} else {
					causeCode = (Integer) causeCodeObj;
				}
			}
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "P_CAUSE_CODE is "+causeCode);
		}
		
		SipServletMessage peerLegErrByeCanMsg = (SipServletMessage) legData
				.get(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG);// P_PEER_LEG_CANCEL_BYE_ERR_MSG
		if (peerLegErrByeCanMsg != null
				&& !(peerLegErrByeCanMsg.getMethod().equals(
						PhConstants.BYE_REQUEST) || peerLegErrByeCanMsg
						.getMethod().equals(PhConstants.CANCEL_REQUEST))) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Create the same response as received from other leg");
			}
			statusCode = ((SipServletResponse) peerLegErrByeCanMsg).getStatus();
		} else if (((legData.get(LegDataAttributes.P_CAUSE_CODE) != null)||(action.getReleaseCauseValue()>0))
				&& PhConstants.INVALID_CAUSE_CODE != causeCode) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Create response based on cause value provided by service in legData");
			}
			statusCode = causeCode;
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: send the default response");
			}
			
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Decided SIP response status code is " + statusCode);
		}

		if (SignalingTypeEnum.SIGNALING_TYPE.SIP_T.equals(legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY))) {
			return SipIsupHelper.createErrorResponse(sipRequest,
					PhConstants.NO_ANS_TIMEOUT_CV_TERM_LEG,statusCode);
		}

		errorResponse = sipRequest.createResponse(statusCode);//(causeCodeStr != null)?sipRequest.createResponse(statusCode, causeCodeStr):
		
		/*
		 * Set reason header in case set in Call Data
		 */
		String reason = null;
		if ((reason = (String) legData
				.get(LegDataAttributes.P_REASON)) != null) {
			errorResponse.addHeader(PhConstants.REASON_HEADER, reason);
		} else {
			String peerLegReasonHeader = peerLegErrByeCanMsg != null ? peerLegErrByeCanMsg
					.getHeader(PhConstants.REASON_HEADER) : null;

			if (peerLegReasonHeader != null) {
				errorResponse.addHeader(PhConstants.REASON_HEADER,
						peerLegReasonHeader);
			} else {
				errorResponse.addHeader(PhConstants.REASON_HEADER,
						PhConstants.REASON_HEADER_VALUE_INITIAL + statusCode
								+ ";text=\"" + errorResponse.getReasonPhrase()
								+ "\"");
				
			}
		}

		if (peerLegErrByeCanMsg != null) {
			SipIsupHelper.removeErrByeCanMsgOnOtherLeg(SipIsupHelper
					.getPeerLegInviteReq(sipRequest));
		}
		

		if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null){
			errorResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
		}
		return errorResponse;
	}

	/**
	 * This method is called by protocol handler for creating re-invite request.
	 * 
	 * @param initialInviteRequest
	 *            represents an instance of SipServletRequest
	 * @param sdpContent
	 *            represents an instance of MultipartBody
	 * @return represents an instance of SipServletRequest
	 * @throws Exception
	 */
	public static SipServletRequest createReinviteRequest(
			SipServletRequest initialInviteRequest, MultipartBody sdpContent)
			throws Exception {
		SipApplicationSession appSession = initialInviteRequest
				.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
				initialInviteRequest.getSession(), callData);
		ConnectionType connectionType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		ConnectionType legType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		
		//SipProtocolUtil.incrementNetworkTransactions(callData, 1); will increment on 200 ok

		SipServletRequest reinviteRequest = null;

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Creating reinvite for "
					+ connectionType);
		}

		SipSession sipSession = initialInviteRequest.getSession();
		
		ServiceInterface serviceHandler = PhUtilityServices
				.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		sipSession.setHandler(
				serviceHandler.getServletName());

		reinviteRequest = sipSession.createRequest(PhConstants.INVITE_REQUEST);

		if (sdpContent != null) {
			reinviteRequest.setContent(sdpContent.getContent(),
					sdpContent.getContentType());

			legData.set(LegDataAttributes.NP_IS_OFFER_SENT, PhConstants.TRUE);
		}else{
			legData.set(LegDataAttributes.NP_IS_OFFER_SENT, PhConstants.FALSE);
		}
		
		if (legType == ConnectionType.TERM_CONNECTION) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: set P_REINVITE_SENT_TO_TERM to TRUE ");
			}
			callData.set(CallDataAttribute.P_REINVITE_SENT_TO_TERM,
					PhConstants.TRUE);
		}

		int sTermSessionRefresh = Integer.parseInt(SipProtocolUtil.getConfig(
				SipProtocolConfig.TERM_SESSION_REF_SUPPORT));
		
		String timerValue = (String) legData
				.get(LegDataAttributes.P_SESSION_EXPIRE_TIME);

		if (timerValue == null || timerValue.isEmpty()) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Session refresh is not supported for the call");
				logger.debug(origLegCallId
						+ ":: Dont add header related to session refresh");
			}
			
			Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
			if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
				reinviteRequest.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
			}
			return reinviteRequest;
		}

		/**
		 * check if timer not supported on term leg and we are sending reinite on term then donot add session refesh param
		 */
		
	
		if ((sTermSessionRefresh != 0 && legType==ConnectionType.TERM_CONNECTION) || (legType==ConnectionType.ORIG_CONNECTION)) {
		 
			String minSeValue = (String) legData
					.get(LegDataAttributes.P_SESSION_EXPIRE_MIN_SE);
			
			// Set Min-SE		
			if (minSeValue != null) {
				reinviteRequest
						.addHeader(PhConstants.MIN_SE_HEADER, minSeValue);
			}
				// Set Session-Expires
			reinviteRequest.addHeader(PhConstants.SESSION_EXPIRE_HEADER,
						timerValue + PhConstants.SEMI_COLON_STR + PhConstants.REFRESHER_TAG + PhConstants.EQUALS_STR
								+ PhConstants.REFRESHER_UAS);
			
			// Set Supported
			reinviteRequest.addHeader(PhConstants.SUPPORTED_HEADER,
					PhConstants.SUPPORTED_TIMER);
		}
		
		if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null){
			reinviteRequest.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
		}

		return reinviteRequest;
	}

	/**
	 * This method is used to create redirect response for sending out .e.g. LNP 
	 * kind of application
	 * @param sipRequest
	 * @param callData
	 * @param legData
	 * @return
	 * @throws Exception
	 */
	public static SipServletResponse createRedirectionResponse(
			SipServletRequest sipRequest, CallData callData, LegData legData)
			throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside createRedirectionResponse");
		}
		
		
		String respCode= SipProtocolConfig.getConfigData(SipProtocolConfig.REDIRECT_RESPONSE_CODE);
		
		int responseCode=SipServletResponse.SC_MOVED_TEMPORARILY;
		
		if(respCode!=null && !respCode.isEmpty()){
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::Response code from configuration is "
						+ respCode +" !!!");
			}		
			try {
				int responseRead = Integer.parseInt(respCode);
				if (responseRead >= 300 && responseCode < 400) {
					responseCode = responseRead;
				}
			} catch (Exception e) {
				logger.error(" invalid response code provided in config for redirection will pick default 302");
			}		
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::create redirect response using "
					+ responseCode +" response code !!!");
		}
		
		@SuppressWarnings("unchecked")
		List<TermRedirectionContact> redirectContacts = (List<TermRedirectionContact>) callData.get(CallDataAttribute.NP_REDIRECT_ADDRESS_LIST);
		
		List<MultiChoiceContact> mccList = (List<MultiChoiceContact>)callData
				.get(CallDataAttribute.P_MC_CONTACTS_LIST);
		
		SipServletResponse sipResponse = sipRequest
				.createResponse(responseCode);
		
		if ((redirectContacts == null || redirectContacts.isEmpty()) && (mccList==null || mccList.isEmpty())) {
			PhoneNumber destPhoneNum = (PhoneNumber) legData
					.get(LegDataAttributes.P_DESTINATION_NUMBER);

			String contactHdr = "sip:" + destPhoneNum.getAddress() + "@"
					+ SipProtocolUtil.getTerminatingIp(callData, legData) + ":"
					+ SipProtocolUtil.getTerminatingPort(callData, legData);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Send redirect response with contact  "
						+ contactHdr);
			}
			sipResponse.setHeader(PhConstants.CONTACT_HEADER, contactHdr);
		} else {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Add multiple choice contacts  to redirection response " + mccList);
			}
			if(mccList!=null){
				
				for (MultiChoiceContact mcc : mccList) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId+" mutiple choice contact is "+mcc);
					}
					Address contactAddr=PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipFactory().createAddress(mcc.getContactUri());
					sipResponse.addAddressHeader(PhConstants.CONTACT_HEADER,contactAddr,true);
				}
			}else{
			
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Add  redirect contacts  to redirection response " + redirectContacts);
				}
				for (TermRedirectionContact trc : redirectContacts) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Add redirection contact  "+ trc);
					}	
					
			//	int trcPort = (trc.getRedirectPort() == -1) ? 5060 : trc.getRedirectPort();
					
					Address contactAddr=PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipFactory().createAddress(trc.getRedirectContactUri());
							//"sip:"+trc.getRedirectNum()+"@"+trc.getRedirectAddr()+":"+trcPort);
					
//				if (trc.isQValueSet()) {
//					contactAddr.setQ(trc.getQValue());
//				}	
					sipResponse.addAddressHeader(PhConstants.CONTACT_HEADER,contactAddr,false);
			}
		}
		}
		
		ListIterator<Address> paiheaders = sipRequest
				.getAddressHeaders(PhConstants.PAI_HEADER);

		while (paiheaders.hasNext()) {
			Address paiAddr = paiheaders.next();
			sipResponse
					.addAddressHeader(PhConstants.PAI_HEADER, paiAddr, false);
		}
		
		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
		if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
			sipResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
		}
		
		SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		return sipResponse;
	}

	/**
	 * This method create the provisional response for the other leg from
	 * received provisional response.
	 * 
	 * @param sipRequest
	 *            represents an instance of SipServletRequest
	 * @param responseCode
	 *            represents integer value of response code
	 * @param sdpContent
	 *            represents an instance of MultipartBody
	 * @param sipResponse
	 * @param legData
	 *            legData for the leg on which provisional response needs to be
	 *            sent
	 * @return an instance of SipServletResponse
	 * @throws Exception
	 */
	public static SipServletResponse createProvisionalResponse(
			SipServletRequest sipRequest, int responseCode,
			MultipartBody sdpContent, LegData origLegData,
			SipServletResponse sipResponse) throws Exception {
		SipServletResponse peerProvisionlResponse = null;
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside createProvisionalResponse");
		}

		if (origLegData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY) == SignalingTypeEnum.SIGNALING_TYPE.SIP_T) {
			return SipIsupHelper.createProvisionalResponse(sipRequest,
					sipResponse);
		} else {

			/**
			 * reeta level-3 ring back tone issue we will send whatever the response coming from term if ringing alreday sent even then we will send again
			 * it if received from alternate destination
			 */
			boolean isRingingResponseSent = PhConstants.TRUE
					.equals(origLegData
							.get(LegDataAttributes.NP_IS_RINGING_SENT));

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: isRingingSent="
						+ isRingingResponseSent);
			}
			
			  if (responseCode == SipServletResponse.SC_CALL_QUEUED) {

                  if (logger.isDebugEnabled()) {
                      logger.debug("[PH] doProvisionalResponse(): response code 182 received ");
                  }
                  if (!Boolean.TRUE.equals(callData.get(CallDataAttribute.CALL_WAITING_ENABLED))) {

                      if (logger.isDebugEnabled()) {
                          logger.debug("[PH] doProvisionalResponse(): call waiting is not enabled so send 180 instead of 180 ");
                      }
                      peerProvisionlResponse = sipRequest.createResponse(SipServletResponse.SC_RINGING);
                  }
              } 
			  
			  if ((responseCode == SipServletResponse.SC_RINGING)
					&& isRingingResponseSent) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Ringing already sent so create session in progress");
				}
				peerProvisionlResponse = sipRequest
						.createResponse(SipServletResponse.SC_SESSION_PROGRESS);
			} else  if(peerProvisionlResponse==null){
				peerProvisionlResponse = sipRequest
						.createResponse(responseCode);
				if (responseCode == SipServletResponse.SC_RINGING) {
					origLegData.set(
							LegDataAttributes.NP_IS_RINGING_SENT,
							PhConstants.TRUE);
				}
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Set SDP only as signaing type is only sip");
			}

			if (sipResponse.getContent() != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: SDP received from other leg is set");
				}

				peerProvisionlResponse.setContent(sdpContent.getContent(),
						sdpContent.getContentType());
			}else{
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: NO SDP received from other leg");
				}
				
			}
			
//			if (sdpContent != null) {
//				if (logger.isDebugEnabled()) {
//					logger.debug(origLegCallId
//							+ ":: SDP received from other leg is set");
//				}
//
//				peerProvisionlResponse.setContent(sdpContent.getContent(),
//						sdpContent.getContentType());
//			}
		}
		if (peerProvisionlResponse != null) {
			
			SipProtocolUtil.copyHeaders(callData, sipResponse,
					peerProvisionlResponse);
		}
		
//		String requireHeader = (String) sipResponse.getHeader(PhConstants.REQUIRE_HEADER);
//		peerProvisionlResponse.setHeader(PhConstants.REQUIRE_HEADER, requireHeader);
//		
		 String alertInfo = (String) peerProvisionlResponse.getHeader(PhConstants.ALERT_INFO_HEADER);
        
		 if (alertInfo != null && alertInfo.contains("urn:alert:service:call-waiting") &&
                 callData != null && Boolean.TRUE.equals(callData.get(CallDataAttribute.CALL_WAITING_ENABLED))) {
             // add call waiting info to response
        	 peerProvisionlResponse.addHeader(PhConstants.ALERT_INFO_HEADER, alertInfo);
         }
		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
		if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
			peerProvisionlResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
		}
		return peerProvisionlResponse;
	}

	public static SipServletRequest createUpdate(SipSession sipSession,
			MultipartBody content,CallData callData) throws Exception {
		SipServletRequest updateReq = sipSession
				.createRequest(PhConstants.UPDATE_REQUEST);

		updateReq.setContent(content.getContent(), content.getContentType());
		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
		if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
			updateReq.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
		}
		
		SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		return updateReq;
	}

	/**
	 * This method create success response for the session refresh Update
	 * received
	 * 
	 * @param sipRequest
	 *            represents an instance of SipServletRequest
	 * @return an instance of SipServletResponse
	 * @throws Exception
	 */
	public static SipServletResponse createSuccessResponseUpdate(
			SipServletRequest sipRequest) throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Creating success response for UPDATE");
		}
		LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
				sipRequest.getSession(), callData);

		SipServletResponse successResponse = sipRequest
				.createResponse(SipServletResponse.SC_OK);
		// successResponse.setContent(sipRequest.getContent(),
		// sipRequest.getContentType());
		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
		if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
			successResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
		}

		ListIterator<String> supportedHeaderIter = sipRequest
				.getHeaders(PhConstants.SUPPORTED_HEADER);
		if (supportedHeaderIter != null) {
			String supportedHeaderUpdate = null;
			while (supportedHeaderIter.hasNext()) {
				supportedHeaderUpdate = supportedHeaderIter.next();
				if (supportedHeaderUpdate != null
						&& supportedHeaderUpdate
								.contains(PhConstants.SUPPORTED_TIMER)) {
					String timerValue = (String) legData
							.get(LegDataAttributes.P_SESSION_EXPIRE_TIME);

					if (timerValue == null || timerValue.isEmpty()) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Session refresh is not supported for the call");
							logger.debug(origLegCallId
									+ ":: Dont add header related to session refresh");
						}
						return successResponse;
					}

					// Set Session-Expires
					successResponse.addHeader(
							PhConstants.SESSION_EXPIRE_HEADER, timerValue
									+ "; " + PhConstants.REFRESHER_TAG + "="
									+ PhConstants.REFRESHER_UAC);

					/*
					 * As per rfc 4028 Page 15, If the UAS sets the refresher
					 * parameter = "uac", then UAS must place a Require header
					 * into the 200 response with the value timer.
					 */

					// Set Require header
					successResponse.addHeader(PhConstants.REQUIRE_HEADER,
							PhConstants.SUPPORTED_TIMER);
					break;
				}
			}
		}
		
		SipProtocolUtil.incrementNetworkTransactions(callData, 1);
	
		return successResponse;
	}

	/**
	 * This method is used to create success response for bye
	 * @param sipRequest
	 * @return
	 * @throws Exception
	 */
	public static SipServletResponse createSuccessResponseBye(
			SipServletRequest sipRequest) throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
				sipRequest.getSession(), callData);

		if (SignalingTypeEnum.SIGNALING_TYPE.SIP_T.equals(legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY))) {
			SipServletResponse successResponse = SipIsupHelper
					.createSuccessResponseBye(sipRequest);
			
			if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null){
				successResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
			}
			return successResponse;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Creating success response for BYE");
		}
		SipServletResponse sipResponse = sipRequest
				.createResponse(SipServletResponse.SC_OK);
		
		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
		if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
			sipResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
		}
		
		SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		return sipResponse;
		
		
	}
	
	/**
	 * This method creates the Ack request for the invite request provided as
	 * argument in this method
	 * 
	 * @param sipResponse
	 *            represents an instance of SipServletResponse
	 * @param callData instance of calldata object for this call
	 * @return an instance of SipServletRequest
	 * @throws Exception
	 */
	public static SipServletRequest createAckRequest(String origLegCallId,
			SipServletResponse sipResponse,CallData callData) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating ACK request");
		}
	
		SipServletRequest ackRequest = sipResponse.createAck();
			Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
			if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
				ackRequest.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
			}
			
			SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		return ackRequest;
	}
	
	
	/**
	 * This method is used to create a response for a specific response code
	 * @param origLegCallId
	 * @param sipRequest
	 * @param statusCode
	 * @param callData
	 * @return
	 */
	public static SipServletResponse createResponse(String origLegCallId,SipServletRequest sipRequest,int statusCode,CallData callData){
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating Sip Response with status code "+statusCode);
		}
	
		SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		
		SipServletResponse sipResponse = sipRequest.createResponse(statusCode);
		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
			if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
				sipResponse.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
			}
			
			SipProtocolUtil.incrementNetworkTransactions(callData, 1);
		return sipResponse;
		
	}
	
	/**
	 * 
	 * @param origLegCallId
	 * @param sipRequest
	 * @param statusCode
	 * @param callData
	 * @return
	 */
	public static SipServletRequest createRequest(String origLegCallId,SipSession sipSession,String requestMethod,CallData callData){
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "Creating Sip request  "+requestMethod);
		}
	
		SipServletRequest sipRequest = sipSession.createRequest(requestMethod);
		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
			if(callData.get(CallDataAttribute.P_CHARGE_VECTOR)!=null && needToAddPCHargeVector){
				sipRequest.setHeader(PhConstants.P_CHARGE_VECTOR, (String)callData.get(CallDataAttribute.P_CHARGE_VECTOR));
			}
			
			SipProtocolUtil.incrementNetworkTransactions(callData, 1);
			
		return sipRequest;
		
	}
}
