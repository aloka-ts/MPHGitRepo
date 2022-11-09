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

package com.agnity.ph.sip.conference;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;
import javax.servlet.sip.SipSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.sbb.ConferenceController;
import com.baypackets.ase.sbb.ConferenceParticipant;
import com.baypackets.ase.sbb.ConnectException;
import com.baypackets.ase.sbb.MediaServer;
import com.baypackets.ase.sbb.MediaServerException;
import com.baypackets.ase.sbb.MediaServerSelector;
import com.baypackets.ase.sbb.MsConferenceSpec;
import com.baypackets.ase.sbb.MsRootSpec;
import com.baypackets.ase.sbb.MsSessionController;
import com.baypackets.ase.sbb.MsVideoConferenceSpec;
import com.baypackets.ase.sbb.SBBFactory;


/**
 *
 * This class will be used to handle Invite request for a conference join.
 * @author Amit Baxi
 *
 */
public class ConferenceHandler {

    private static Logger logger = Logger.getLogger(ConferenceHandler.class);

    private static ConferenceHandler m_conferenceHandler = new ConferenceHandler();

    private static Map<String, String> confIdToAppSessionIdMap;

    static {
        confIdToAppSessionIdMap = new HashMap<String, String>();

    }

    private ConferenceHandler() {
    }

    /**
     * This method returns instance of this conference handler.
     * @return
     */
    public static ConferenceHandler getInstance() {
        return m_conferenceHandler;
    }

    public static Map<String, String> getConfIdToAppSessionIdMap() {
        return confIdToAppSessionIdMap;
    }

    /**
     * This method will be used to process sip request for a conference.
     * @param request
     */
    public void handleConferenceRequest(SipServletRequest request) {
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] handleConferenceRequest(): enter");
        }
        String confId = request.getHeader(PhConstants.X_HOST_CONF_HEADER);
        if (confId != null) {
            if (!isValidConferenceId(confId)) {
                try {
                    if (logger.isDebugEnabled()) {
                        logger.debug("[PH] Sendign 400 Bad Request as conf id missing or invalid");
                    }
                    SipServletResponse response = request.createResponse(SipServletResponse.SC_BAD_REQUEST);
                    response.addHeader(PhConstants.REASON_HEADER, "SIP;cause=" + SipServletResponse.SC_BAD_REQUEST + ";text=\"Invalid or Missing Conf Id \"");
                    response.send();
                    return;
                } catch (Exception e) {
                    logger.error("[PH] problem in connecting with MS", e);
                }
            }
            if (confIdToAppSessionIdMap.containsKey(confId)) {
                try {
                    if (logger.isDebugEnabled()) {
                        logger.debug("[PH] Sendign 500 Internal Server Error as Conference already exists with given Id");
                    }
                    SipServletResponse response = request.createResponse(SipServletResponse.SC_SERVER_INTERNAL_ERROR);
                    response.addHeader(PhConstants.REASON_HEADER, "SIP;cause=" + SipServletResponse.SC_SERVER_INTERNAL_ERROR + ";text=\"Conference already exists with given Id\"");
                    response.send();
                    return;
                } catch (Exception e) {
                    logger.error("[PH] problem in connecting with MS", e);
                }
            }
            SipApplicationSession appSession = request.getApplicationSession();

            appSession.setAttribute(PhConstants.ATTRIB_HOST_SESSION, true);
            request.getApplicationSession().setAttribute(PhConstants.ATTRIB_PARTICIPANT_INVITE, request);
            createConf(appSession, request, confId);
        } else {
            connectParticipantToMS(request);
        }
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] handleConferenceRequest(): exit");
        }
    }

    private boolean isValidConferenceId(String confId) {
        if (confId == null || confId.trim().length() == 0) {
            return false;
        }
        return true;
    }

    public void createConf(SipApplicationSession appSession, SipServletRequest request, String confId) {
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] createConf(): enter with confId:" + confId);
        }
        
        boolean b2bConfMode = (Boolean) appSession.getAttribute(PhConstants.CONF_B2BUA_MODE);
        ConferenceController confController = null;
        if (appSession != null) {
            SBBFactory sbbFactory = (SBBFactory) appSession.getAttribute(PhConstants.ATTRIB_SBB_FACTORY);

            if (sbbFactory != null) {
            	  CallData callData=SipProtocolUtil.getCallData(appSession);
            	ServletContext servletContext=PhUtilityServices.getInstance((String)callData.
            			get(CallDataAttribute.SERVICE_ID)).getServletContext();
            	confController = (ConferenceController) sbbFactory.getSBB(ConferenceController.class.getName(), confId, appSession, servletContext);
                if(b2bConfMode){ 
                	ConferenceParticipant confParticipant1 = (ConferenceParticipant) sbbFactory.
                			getSBB(ConferenceParticipant.class.getName(), "PARTICIPANT1", appSession, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServletContext());

                	ConferenceParticipant confParticipant2 = (ConferenceParticipant) sbbFactory.
                			getSBB(ConferenceParticipant.class.getName(), "PARTICIPANT2", appSession, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServletContext());
                	
                	 appSession.setAttribute(PhConstants.MEDIA_TYPE, isVideoEnabled(confParticipant1, request));
                	 appSession.setAttribute(PhConstants.ATTRIB_PARTICIPANT_COUNT, 0);
                }else{
                	ConferenceParticipant confParticipant = (ConferenceParticipant) sbbFactory.getSBB(ConferenceParticipant.class.getName(), request
                           .getCallId(), appSession, servletContext);
                	appSession.setAttribute(PhConstants.MEDIA_TYPE, isVideoEnabled(confParticipant, request));
                	
                }
            	
            }

            appSession.setAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID, confId);
            appSession.setAttribute(PhConstants.ATTRIB_PEER_JOINED, false);
            
            ConferenceEventListener confListener = new ConferenceEventListener();
            //controller.getApplicationSession().setAttribute("CONF_EVENT_LISTENER",confListener);
            boolean isVideo = (Integer) appSession.getAttribute(PhConstants.MEDIA_TYPE) == 1 ? true : false;
            confController.setEventListener(confListener);
            
            CallData callData = SipProtocolUtil.getCallData(appSession);
            LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
            
            try {
            	if(legData.get(LegDataAttributes.NP_CONF_SPEC) != null){
            		 MsConferenceSpec conferenceSpec = (MsConferenceSpec) legData.get(LegDataAttributes.NP_CONF_SPEC);
            		 confController.connect(conferenceSpec, PhConstants.MS_CAPABILITIES);
            	}else {
            		confController.connect(generateConferenceSpec(isVideo, confId), PhConstants.MS_CAPABILITIES);
            	}
            	confController.setAttribute(PhConstants.LEG_ID, CallDataAttribute.P_LEG1.name());
            	callData.set(
    					CallDataAttribute.P_LEG_ID_CONNECTED_TO_IVR, CallDataAttribute.P_LEG1.name());
            } catch (IllegalStateException e) {
                logger.error("[PH] An exception occurred:IllegalStateException", e);
            } catch (IllegalArgumentException e) {
                logger.error("[PH] An exception occurred:IllegalArgumentException", e);
            } catch (ConnectException e) {
                logger.error("[PH] An exception occurred:ConnectException", e);
            } catch (MediaServerException e) {
                logger.error("[PH] An exception occurred:MediaServerException", e);
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] createConf(): exit");
        }
    }

    private void connectParticipantToMS(SipServletRequest request) {
        String confId = request.getHeader(PhConstants.X_CONF_HEADER);
        connectParticipantToMS(request, confId, false);
    }

    private int isVideoEnabled(ConferenceParticipant confParticipant, SipServletRequest request) {
        ArrayList<String> mediaType = null;
        try {
            mediaType = confParticipant.getSupportedMediaTypes(request);
        } catch (MediaServerException e) {
            logger.error("[PH] Exception in gettting media type");
        }
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] Got the list from adaptor");
        }
        if (mediaType != null && mediaType.contains(PhConstants.STRING_VIDEO)) {
            if (logger.isDebugEnabled()) {
                logger.debug("[PH] Participant media type is video");
            }
            return 1;
        }
        return 0;
    }

    private MsConferenceSpec generateConferenceSpec(boolean isVideo, String confID) {
        // Prepare Conference Spec
        //String noOfParticipants=(String)Configuration.getInstance().getProperty(PhConstants.PROP_VIDEO_LAYOUT_SIZE);
        //int videoLayoutSize = 4;
        //if(noOfParticipants != null && !noOfParticipants.isEmpty()){
        //	videoLayoutSize = Integer.parseInt(noOfParticipants);
        //}

        MsConferenceSpec confSpec = new MsConferenceSpec();
        confSpec.setMaxActiveSpeakers(4); // TODO make it configurable
        confSpec.setNotifyActiveSpeaker(true); // TODO make it configurable
        confSpec.setNotificationInterval(20); // TODO make it configurable
        // confSpec.setConferenceId(confToBeStarted.getAccessNum());
        confSpec.setConferenceId(confID);
        confSpec.setDeleteConfFlag(MsConferenceSpec.DELETE_ON_NOCONTROL);
        if (isVideo) {
            if (logger.isDebugEnabled()) {
                logger.debug("[PH] ParticipantEventListener:generateConferenceSpec  Conference Type is AUDIO & VIDEO.");
            }
            MsVideoConferenceSpec videoConfSpec = new MsVideoConferenceSpec();
            videoConfSpec.setLayoutSize(4); // TODO make it configurable

            MsRootSpec rootSpec = new MsRootSpec();
            rootSpec.setRootSize(MsRootSpec.QVGA);
            videoConfSpec.setRootSpec(rootSpec);


            // it
            // configurable
            confSpec.setConferenceType(MsConferenceSpec.VIDEO_TYPE);
            confSpec.setMsVideoConferenceSpec(videoConfSpec);

        } else {
            if (logger.isDebugEnabled()) {
                logger.debug("[PH] ParticipantEventListener:generateConferenceSpec Conference Type is AUDIO only.");
            }
            confSpec.setConferenceType(MsConferenceSpec.AUDIO_TYPE);
        }
        return confSpec;
    }

    protected void connectParticipantToMS(SipServletRequest request, String confId, boolean b2bConfMode) {
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] connectParticipantToMS(): enter with confId:" + confId);
        }
        if (confId != null && !confIdToAppSessionIdMap.containsKey(confId)) {
            // Participant Invite for joining a existing conference
            try {
                if (logger.isDebugEnabled()){
                    logger.debug("[PH] Sending 503 for conf id trying");
                }
                SipServletResponse response = request.createResponse(SipServletResponse.SC_SERVICE_UNAVAILABLE);
                response.addHeader(PhConstants.REASON_HEADER, "SIP;cause=" + SipServletResponse.SC_SERVICE_UNAVAILABLE + ";text=\"Conference does not exists\"");
                response.send();
            } catch (Exception e) {
                logger.error("[PH] problem in connecting with MS", e);
            }

        } else {
        	SipApplicationSession appSession = request.getApplicationSession();
            CallData callData=SipProtocolUtil.getCallData(appSession);
        	SBBFactory sbbFactory = (SBBFactory) appSession.getAttribute(PhConstants.ATTRIB_SBB_FACTORY);
            if (sbbFactory == null) {

                logger.error("[PH] ConferenceHandler:connectParticipant SBBFactory not available in application session.");
                return;
            }
            
            
        	if(b2bConfMode){
        		 LegData peerLegData = (LegData) callData.get(CallDataAttribute.P_LEG2);
                 SipServletRequest peerLegInvite = SipProtocolUtil.getInitialInvite(appSession, peerLegData);

                 ConferenceParticipant confParticipant1 = (ConferenceParticipant) sbbFactory.getSBB(ConferenceParticipant.class.getName(), "PARTICIPANT1", appSession, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServletContext());

                 ConferenceParticipant confParticipant2 = (ConferenceParticipant) sbbFactory.getSBB(ConferenceParticipant.class.getName(), "PARTICIPANT2", appSession, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServletContext());
                 if (logger.isDebugEnabled()) {
                     logger.debug("[PH] ConferenceHandler:connectParticipant1 Conference Participant SBB obtained with Id = "
                                          + confParticipant1.getName());
                 }
                 
                 if (logger.isDebugEnabled()) {
                     logger.debug("[PH] ConferenceHandler:connectParticipant2 Conference Participant SBB obtained with Id = "
                                          + confParticipant2.getName());
                 }
                 appSession.setAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID, confId);
                 appSession.setAttribute(PhConstants.MEDIA_TYPE, isVideoEnabled(confParticipant1, request));

                 if (logger.isDebugEnabled()) {
                     logger.debug("[PH] ConferenceHandler:connectParticipant ParticipantName="
                                          + request.getFrom().getURI().toString());
                 }
                 // Set event listener
                 ParticipantEventListener partyListener = new ParticipantEventListener();
                 //request.getApplicationSession().setAttribute("PARTY_EVENT_LISTENER",partyListener);
                 
                 confParticipant1.setEventListener(partyListener);
                 confParticipant2.setEventListener(partyListener);

                 if (logger.isDebugEnabled()) {
                     logger.debug("[PH] ConferenceHandler:connectParticipant Connect Participant to Media Server");
                 }
                 try {
                     //Added for media server as per new implementation
                     MediaServerSelector msSelector = (MediaServerSelector) PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServletContext().getAttribute(MediaServerSelector.class.getName());
                     MediaServer ms = msSelector.selectByCapabilities(PhConstants.MS_CAPABILITIES);
                     if (ms == null) {
                         logger.error("[PH] No active media server available");
                         return;
                     } else{
                     	
                     	confParticipant1.setAttribute(MsSessionController.MS_INVITE_WITHOUT_SDP, PhConstants.TRUE);	
                     	SipSession origlegSipSession = request.getSession();
                     	origlegSipSession.setAttribute(PhConstants.DIALOG_STATE,
         						PhConstants.DIALOG_STATE_CONFIRMED);
                     	
                     	if(logger.isDebugEnabled()){
                     		logger.debug("origlegSipSession session state : " + origlegSipSession.getState());
                     	}
                     	if(origlegSipSession.getState() == javax.servlet.sip.SipSession.State.CONFIRMED){
                     		/*
             				 * Sometimes SAS throws runtime exception. So precautionary
             				 * adding following try catch block to ignore that exception
             				 */
             				try {
             					confParticipant1.addA(origlegSipSession);
             				} catch (Throwable e) {
             					logger.warn(" A already added in Orig MS Sip Session");
             				}
             				confParticipant1.activate(origlegSipSession);

             				confParticipant1.dialOut(ms);
                     	}else{
                     		if(logger.isDebugEnabled()){
                     			logger.debug("origlegSipSession is not in confirmed state.");
                     		}
                     		confParticipant1.connect(request, ms);
                     	}
         				
                     }
                 } catch (Exception e) {
                     logger.error("[PH] ConferenceHandler:connectParticipant An error occured while trying to connect participant to media server: "
                                          + e);
                 }
        	}else{
        		ConferenceParticipant confParticipant = (ConferenceParticipant) sbbFactory.
        				getSBB(ConferenceParticipant.class.getName(), request.getCallId(), appSession, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServletContext());
        		if (logger.isDebugEnabled()) {
        			logger.debug("[PH] ConferenceHandler:connectParticipant Conference Participant SBB obtained with Id = "
        					+ confParticipant.getName());
        		}

        		appSession.setAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID, confId);
        		appSession.setAttribute(PhConstants.MEDIA_TYPE, isVideoEnabled(confParticipant, request));

        		if (logger.isDebugEnabled()) {
        			logger.debug("[PH] ConferenceHandler:connectParticipant ParticipantName="
        					+ request.getFrom().getURI().toString());
        		}

        		// Set event listener
        		ParticipantEventListener partyListener = new ParticipantEventListener();
        		//request.getApplicationSession().setAttribute("PARTY_EVENT_LISTENER",partyListener);


        		confParticipant.setEventListener(partyListener);
        		if (logger.isDebugEnabled()) {
        			logger.debug("[PH] ConferenceHandler:connectParticipant Connect Participant to Media Server");
        		}

        		try {
        			//Added for media server as per new implementation
        			MediaServerSelector msSelector = (MediaServerSelector) PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServletContext().getAttribute(MediaServerSelector.class.getName());
        			MediaServer ms = msSelector.selectByCapabilities(PhConstants.MS_CAPABILITIES);
        			if (ms == null) {
        				logger.error("[PH] No active media server available");
        				return;
        			} else{

        				confParticipant.connect(request, ms);
        			}
        		} catch (Exception e) {
        			logger.error("[PH] ConferenceHandler:connectParticipant An error occured while trying to connect participant to media server: "
        					+ e);
        		}
        	}
            
        }
        if (logger.isDebugEnabled()) {
            logger.debug("[PH] connectParticipantToMS(): exit");
        }
    }

}
