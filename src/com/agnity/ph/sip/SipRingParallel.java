/****
 * Copyright (c) 2013 Agnity, Inc. All rights reserved.
 * <p>
 * This is proprietary source code of Agnity, Inc.
 * Agnity, Inc. retains all intellectual property rights associated
 * with this source code. Use is subject to license terms.
 * <p>
 * This source code contains trade secrets owned by Agnity, Inc.
 * Confidentiality of this computer program must be maintained at
 * all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.ph.sip;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;
import javax.servlet.sip.SipSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.InviteAttributes;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.MultipartBody;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;

/**
 * This Action will send INVITE requests to multiple destination in parallel.
 * @author Reeta Aggarwal
 *
 */
public class SipRingParallel{

    /**
     *
     */
    private static final long serialVersionUID = 5114580199832371945L;

    private static Logger logger = Logger.getLogger(SipRingParallel.class);

    /**
     * Constructor for this action.
     * @param destinations list of destination for parallel ringing.
     * @param callData the calldata for the action.
     */
    
    
    /**
	 * This method is used to connect muliple destinationin parallel
	 * @param appSession
	 * @param callData
	 * @param action
	 * @param isParallel
	 * @throws Exception
	 */
	public static void  connectParallel(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: connectParallel, in parallel");
		}

		 LegData legData = (LegData) callData
					.get(CallDataAttribute.valueOf(action.getLeg()));
		 
		@SuppressWarnings("unchecked")
		List<InviteAttributes> destinations = (List<InviteAttributes>) legData.get(LegDataAttributes.FORKING_INVITE_ATTRIBUTES_LIST);

        if (logger.isDebugEnabled()) {
            logger.debug("[PH] Inside connectParallel()...");
        }
      
        List<SipServletRequest> parallelSipReq = new ArrayList<SipServletRequest>();
      
        boolean termExist = false;
			if (destinations != null) {
				for (InviteAttributes inviteAttribute : destinations) {
					if (inviteAttribute != null) {

						legData.set(LegDataAttributes.INVITE_ATTRIBUTES,
								inviteAttribute);
						
						SipProtocolHelper.sendInitialInviteToTerm(appSession, true,
								callData, action);
	
						termExist = true;
						parallelSipReq.add(SipProtocolUtil.getInitialInvite(
								appSession, legData));
						
						Event event = new Event(Event.EventType.EVENT_ROUTE_NEXT,
								Protocol.SIP, action.getLeg());
						
						ProtocolRouter.getInstance().execute(event, callData,
								PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
					}
				}
			}
        if (!termExist) {
            logger.error("[PH] No contact binding available for the User. Rejecting the call with 480 response.");
            try {
                
                ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				
				Event event = new Event(EventType.EVENT_FAILURE,
								Protocol.SIP, action.getLeg());
					
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
            } catch (IOException e) {
                logger.error("Exception in RingParallelAction:", e);
            }
        } else {
            if (logger.isDebugEnabled()) {
                logger.debug("[PH] Save the list of parallel calls");
            }
            appSession.setAttribute(PhConstants.PEER_REQUEST_LIST, parallelSipReq);
            
        }
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] Exiting connectParallel()...");
        }
    
	}

        /**
         * This method is used to handle cancel request
         * @param request
         * @param callData
         */
		public static void handleCancelRequest(SipServletRequest request,
				com.agnity.mphdata.common.CallData callData) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] handleCancelRequest() entered.");
			}
			try {
				if (request.getApplicationSession().getAttribute(
						PhConstants.PEER_REQUEST_LIST) != null){
					handleCancelForParallelCallLegs(request);
				}else {
	
					LegData legData = (LegData) callData
							.get(CallDataAttribute.P_LEG2);
				
					SipServletRequest termInvite = SipProtocolUtil
							.getInitialInvite(request.getApplicationSession(),
									legData);
					if (termInvite != null) {
						termInvite.createCancel().send();
					}
				}
				
				   ServiceInterface serviceHandler = PhUtilityServices
							.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
					
					Event event = new Event(EventType.EVENT_DISCONNECT,
									Protocol.SIP, CallDataAttribute.P_LEG1.name());
						
					ProtocolRouter.getInstance().execute(event, callData,
							serviceHandler);
			} catch (Exception e) {
				logger.error("[PH] An Exception occured during creating cancel" + e);
			}
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] handleCancelRequest() exit.");
			}
			
			
		}

		/**
		 * This method is used to handle success response from a leg
		 * @param origLegCallId
		 * @param response
		 * @param peerRequest
		 * @param callData
		 * @param legData
		 */
	public static void handleSuccessResponse(String origLegCallId, SipServletResponse response,
			SipServletRequest peerRequest, CallData callData, LegData legData) {
		
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] Inside handleSuccessResponse() for method:"
					+ response.getMethod());
		}
		try {

			MultipartBody sdpContent = (MultipartBody) legData
					.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
			
			legData.set(LegDataAttributes.P_SESSION_ID,
					response.getSession().getId());
			
			legData.set(LegDataAttributes.P_CALL_ID,
					response.getCallId());
			
			
            SipProtocolUtil.stopTimer(response.getApplicationSession(), PhConstants.NO_ANSWER_TIMER);
           
			cancelOtherCallLegs(response.getRequest());
			
			/*
			 * instaed will call 
			 */
//			SipServletResponse peerResponse = SipProtocolMessageCreator
//					.createSuccessResponseInvite(peerRequest, sdpContent);
//			peerResponse.send();
//			
			SipProtocolMessageCreator.createAckRequest(origLegCallId, response, callData).send();
		
			SipProtocolUtil.startMaxCallDurationTimer(origLegCallId, callData, response.getApplicationSession());
			Event event = new Event(Event.EventType.EVENT_SUCCESS,
					Protocol.SIP, CallDataAttribute.P_LEG2.name());
			
			ProtocolRouter.getInstance().execute(event, callData,
					PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
		} catch (Exception e) {
			logger.error("[PH] An Exception occured during creating peer response"
					+ e);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] Success response sent for INVITE");
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] Exitting handleSuccessResponse()");
		}

	}


	/**
	 * This method is used to handle error response
	 * @param response
	 * @param callData
	 * @throws Exception
	 */
    @SuppressWarnings("unchecked")
    public static  void handleErrorResponse(SipServletResponse response,CallData callData) throws Exception {
        int responseCode = response.getStatus();

        if (logger.isDebugEnabled()) {
            logger.debug("[PH] Inside handleErrorResponse() for method:" + response.getMethod() + " response code"
                                 + responseCode);
        }

        SipServletRequest peerRequest = SipProtocolUtil
				.getOrigInitialInvite(response.getApplicationSession(),
						callData);
        List<SipServletRequest> peerReqList = (List<SipServletRequest>) peerRequest.getApplicationSession()
                .getAttribute(PhConstants.PEER_REQUEST_LIST);
        if (peerReqList.size() > 1) {
            // ignore the response
            peerReqList.remove(response.getRequest());
        } else {
            SipServletResponse peerResponse = null;
            if (responseCode != SipServletResponse.SC_REQUEST_TERMINATED){
                peerResponse = peerRequest.createResponse(response.getStatus());
                try {
                    peerResponse.send();
                } catch (IOException e) {
                    logger.error("[PH] An Exception occured during creating peer response" + e);
                }
                
                ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				
				Event event = new Event(EventType.EVENT_FAILURE,
								Protocol.SIP, CallDataAttribute.P_LEG2.name());
					
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] Exitting handleErrorResponse()");
        }
    }

    /**
     * This method is used to handle cancel of parallel legs
     * @param request
     */
    @SuppressWarnings("unchecked")
    private static  void handleCancelForParallelCallLegs(SipServletRequest request) {
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] handleCancelForParallelCallLegs() entered.");
        }

        List<SipServletRequest> peerReqList = (List<SipServletRequest>) request.getApplicationSession().getAttribute(PhConstants.PEER_REQUEST_LIST);

        if (peerReqList != null) {
            Iterator<SipServletRequest> itr = peerReqList.iterator();
            while (itr.hasNext()) {
                SipServletRequest sipRequest = itr.next();
                try {
                    if (sipRequest.getSession().getState() == SipSession.State.INITIAL ||
                            sipRequest.getSession().getState() == SipSession.State.EARLY) {
                        sipRequest.createCancel().send();
                    } else if (sipRequest.getSession().getState() == SipSession.State.CONFIRMED) {
                        sipRequest.getSession().createRequest(PhConstants.BYE_REQUEST).send();
                    }
                } catch (Exception e) {
                    logger.error("[PH] An Exception occured during creating cancel" + e);
                }

            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("[PH] handleCancelForParallelCallLegs() exit..");
        }
    }

    /**
     * this method is used to cancel other legs
     * @param peer_request
     */
    @SuppressWarnings("unchecked")
    private static void cancelOtherCallLegs(SipServletRequest peer_request) {

        List<SipServletRequest> peerReqList = (List<SipServletRequest>) peer_request.getApplicationSession().getAttribute(PhConstants.PEER_REQUEST_LIST);

        if (peerReqList != null) {
            boolean isRemoved = peerReqList.remove(peer_request);

            if (logger.isDebugEnabled()) {
                logger.debug("[PH] cancelOtherCallLegs() entered. isRemoved: " + isRemoved);
            }
            Iterator<SipServletRequest> itr = peerReqList.iterator();
            while (itr.hasNext()) {
                SipServletRequest sipRequest = itr.next();
                try {
                    if (sipRequest.getSession().getState() == SipSession.State.INITIAL ||
                            sipRequest.getSession().getState() == SipSession.State.EARLY) {
                        sipRequest.createCancel().send();
                    } else if (sipRequest.getSession().getState() == SipSession.State.CONFIRMED) {
                        sipRequest.getSession().createRequest(PhConstants.BYE_REQUEST).send();
                    }
                } catch (Exception e) {
                    logger.error("[PH] cancelOtherCallLegs(): An Exception occured during cancel" + e);
                }

            }
            if (logger.isDebugEnabled()) {
                logger.debug("[PH] cancelOtherCallLegs() exiting....");
            }
        }
    }

    /**
     * This method is used to handle timeout
     * @param timer
     * @param callData
     */
    public static void handleTimeout(ServletTimer timer,CallData callData) {
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] handleTimeout() entered");
        }
        SipServletRequest origInvite = SipProtocolUtil
				.getOrigInitialInvite(timer.getApplicationSession(),
						callData);
        
        LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG2);
		SipServletRequest termInvite = SipProtocolUtil
				.getInitialInvite(timer.getApplicationSession(),
						legData);
        /*
         * cancel term call legs
         */
        handleCancelForParallelCallLegs(termInvite);

        if (logger.isDebugEnabled()) {
            logger.debug("[PH] sending 408 response on orig call leg ..");
        }
        /*
         * send failure response to orig call leg
         */
        try {
        	origInvite.createResponse(SipServletResponse.SC_REQUEST_TIMEOUT).send();
        } catch (Exception e) {
            logger.error("[PH] An Exception occured during creating peer response" + e);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("[PH] handleTimeout() exit");
        }
    }


}
