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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;
import javax.servlet.sip.SipSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.sbb.ConferenceController;
import com.baypackets.ase.sbb.ConferenceParticipant;
import com.baypackets.ase.sbb.MediaServer;
import com.baypackets.ase.sbb.MediaServerSelector;
import com.baypackets.ase.sbb.MsConferenceSpec;
import com.baypackets.ase.sbb.MsRegionSpec;
import com.baypackets.ase.sbb.MsRootSpec;
import com.baypackets.ase.sbb.MsSessionController;
import com.baypackets.ase.sbb.MsVideoConferenceSpec;
import com.baypackets.ase.sbb.SBB;
import com.baypackets.ase.sbb.SBBEvent;
import com.baypackets.ase.sbb.SBBEventListener;
import com.baypackets.ase.sbb.SBBFactory;

/**
 *
 * This Class will work as a listener for conference controller sbb.
 * @author Amit Baxi
 *
 */
public class ConferenceEventListener implements SBBEventListener {

    private static final long serialVersionUID = -4017891851132101439L;
    private static Logger logger = Logger.getLogger(ConferenceEventListener.class);
    

    public int handleEvent(SBB sbb, SBBEvent event) {
        boolean isDebugEnabled = logger.isDebugEnabled();
        SipApplicationSession appSession = sbb.getApplicationSession();
        String confID = null;
        if (appSession.getAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID) != null) {
            confID = (String) appSession.getAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID);
        }
        if (isDebugEnabled) {
            logger.debug("[PH] handleEvent event is: " + event.getEventId() + "for SBB: " + sbb);
        }
        try {
            if (event.getEventId().equals(SBBEvent.EVENT_CONNECTED)) {
                if (confID != null) {
                    ConferenceHandler.getConfIdToAppSessionIdMap().put(confID, appSession.getId());
                    ConferenceController controller = (ConferenceController) sbb;
                    appSession.setAttribute(PhConstants.ATTRIB_CONF_CONTROLLER, controller);
                    if ((Integer) appSession.getAttribute(PhConstants.MEDIA_TYPE) == 1) {
                        String[] participants = (String[]) appSession.getAttribute(PhConstants.DISPLAY_ID);
                        if (participants == null) {
                            String[] participant = new String[PhConstants.FOUR_PARTY];
                            appSession.setAttribute(PhConstants.DISPLAY_ID, participant);
                        }
                        List<ConferenceParticipant> waitParty = new ArrayList<ConferenceParticipant>();
                        controller.getApplicationSession().setAttribute(PhConstants.WAITING_PARTY, waitParty);
                        appSession.setAttribute(PhConstants.NO_OF_VIDEO_PARTICIPATS, new AtomicInteger(0));
                    }
                    SipServletRequest request = (SipServletRequest) appSession.getAttribute(PhConstants.ATTRIB_PARTICIPANT_INVITE);
                    boolean b2bConfMode = (Boolean) appSession.getAttribute(PhConstants.CONF_B2BUA_MODE);
                    ConferenceHandler.getInstance().connectParticipantToMS(request, confID, b2bConfMode);
                }
            } else if (event.getEventId().equals(SBBEvent.EVENT_CONNECT_FAILED)) {
                SipServletRequest request = (SipServletRequest) appSession.getAttribute(PhConstants.ATTRIB_PARTICIPANT_INVITE);
                if (logger.isDebugEnabled()) {
                    logger.debug("[PH] Sendign 480 as unable to create conference on MS with conf id :" + confID);
                }
                SipServletResponse response = request.createResponse(SipServletResponse.SC_TEMPORARLY_UNAVAILABLE);
                response.addHeader(PhConstants.REASON_HEADER, "SIP;cause=" + SipServletResponse.SC_TEMPORARLY_UNAVAILABLE + ";text=\"Unable to create conference on media server\"");
                response.send();
                
                CallData callData = SipProtocolUtil.getCallData(appSession);
                String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
                ServiceInterface serviceHandler = PhUtilityServices
                		.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
                Event appEvent = new Event(EventType.EVENT_CONF_FAILURE, Protocol.SIP, legId);

           	 ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
            } else if (event.getEventId().equals(SBBEvent.EVENT_CONF_JOINED)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("[PH] Joined the Conference" + sbb.getName());
                }
                if ((Integer) appSession.getAttribute(PhConstants.MEDIA_TYPE) == 1) {
                    ConferenceController controller = (ConferenceController) sbb;
                    AtomicInteger participantNumber = (AtomicInteger) controller.getApplicationSession().getAttribute(
                            PhConstants.NO_OF_VIDEO_PARTICIPATS);
                    if (participantNumber != null) {
                        participantNumber.incrementAndGet();
                    }
                }
                
                
                boolean b2bConfMode = (Boolean) appSession.getAttribute(PhConstants.CONF_B2BUA_MODE);
                if(b2bConfMode){
                	boolean isPeerJoined = (Boolean) appSession.getAttribute(PhConstants.ATTRIB_PEER_JOINED);
                	/**
                	 * check added to connect B-party later,once conference is created and A party hase joined 
                	 * the conference.
                	 */
                	if(!isPeerJoined){ 
                		SBBFactory sbbFactory = (SBBFactory) appSession.getAttribute(PhConstants.ATTRIB_SBB_FACTORY);
                    	CallData callData=SipProtocolUtil.getCallData(appSession);
                    	
                    	LegData peerLegData = (LegData) callData.get(CallDataAttribute.P_LEG2);
                        SipServletRequest peerLegInvite = SipProtocolUtil.getInitialInvite(appSession, peerLegData);

                    	ConferenceParticipant confParticipant2 = (ConferenceParticipant) sbbFactory.getSBB
                    			(ConferenceParticipant.class.getName(), "PARTICIPANT2", appSession, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServletContext());
                    	
                    	if (logger.isDebugEnabled()) {
                            logger.debug("[PH] ConferenceHandler:connectParticipant2 Conference Participant SBB obtained with Id = "
                                                 + confParticipant2.getName());
                        }
                    	
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH] ConferenceHandler:connectParticipant Connect Participant to Media Server");
                        }
                        try {
                            //Added for media server as per new implementation
                            MediaServerSelector msSelector = (MediaServerSelector) PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServletContext().getAttribute(MediaServerSelector.class.getName());
                            MediaServer ms = msSelector.selectByCapabilities(PhConstants.MS_CAPABILITIES);
                            if (ms == null) {
                                logger.error("[PH] No active media server available");
                            } else{
                            	
                				confParticipant2.setAttribute(MsSessionController.MS_INVITE_WITHOUT_SDP, PhConstants.TRUE);	
                            	SipSession termSipSession = peerLegInvite.getSession();
                            	termSipSession.setAttribute(PhConstants.DIALOG_STATE,
                						PhConstants.DIALOG_STATE_CONFIRMED);
                            	
                            	if(logger.isDebugEnabled()){
                            		logger.debug("termSipSession session state : " + termSipSession.getState());
                            	}
                            	if(termSipSession.getState() == javax.servlet.sip.SipSession.State.CONFIRMED){
                            		/*
                    				 * Sometimes SAS throws runtime exception. So precautionary
                    				 * adding following try catch block to ignore that exception
                    				 */
                    				try {
                    					confParticipant2.addA(termSipSession);
                    				} catch (Throwable e) {
                    					logger.warn(" A already added in Orig MS Sip Session");
                    				}
                    				confParticipant2.activate(termSipSession);
                    				confParticipant2.dialOut(ms);
                    				
                    				appSession.setAttribute(PhConstants.ATTRIB_PEER_JOINED, true);
                            	}else{
                            		if(logger.isDebugEnabled()){
                            			logger.debug("termSipSession is not in confirmed state.");
                            		}
                            		confParticipant2.connect(peerLegInvite, ms);
                            	}
                				
                            }
                        } catch (Exception e) {
                            logger.error("[PH] ConferenceHandler:connectParticipant An error occured while trying to connect participant to media server: "
                                                 + e);
                        }   
                	}
                    
                	int participantCount = (Integer) appSession.getAttribute(PhConstants.ATTRIB_PARTICIPANT_COUNT);
                	participantCount++;
                    /* ConferenceController controller = (ConferenceController) sbb;
                     int noOfParticipants = Iterators.size(controller.getParticipants());
                     if(logger.isDebugEnabled()){
                     	logger.debug("[PH] Number of participants : " + noOfParticipants);
                     }*/
                 		
                     if(participantCount == 2){
                    	 
                    	 if(logger.isDebugEnabled()){
                    		 logger.debug(":: Inform service for successful call conference");
                    	 }
                    	 CallData callData = SipProtocolUtil.getCallData(appSession);
                    	 String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
                    	 ServiceInterface serviceHandler = PhUtilityServices
                    			 .getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
                    	 Event appEvent = new Event(EventType.EVENT_CONF_SUCCESS, Protocol.SIP, legId);

                    	 ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
                     }
                     
                     appSession.setAttribute(PhConstants.ATTRIB_PARTICIPANT_COUNT, participantCount);
                }
               
            	
            } else if (event.getEventId().equals(SBBEvent.EVENT_CONF_JOIN_FAILED)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("[PH] Join Conference failed.");
                }
                
                CallData callData = SipProtocolUtil.getCallData(appSession);
                String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
                ServiceInterface serviceHandler = PhUtilityServices
                		.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
                Event appEvent = new Event(EventType.EVENT_CONF_FAILURE, Protocol.SIP, legId);

                ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
            } else if (event.getEventId().equals(SBBEvent.EVENT_CONF_UNJOINED) || event.getEventId().equals(SBBEvent.EVENT_CONF_UNJOIN_FAILED)) {

                if (logger.isDebugEnabled()) {
                    logger.debug("[PH] Unjoined the Conference" + sbb.getName());
                }
                if (sbb.getApplicationSession().getAttribute(PhConstants.ATTRIB_CONF_HOST_UNJOINED) != null) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("[PH] Host Unjoined the Conference, confID:: " + confID);
                    }

                    this.removeConference(confID, appSession);

                    ((ConferenceController) sbb).disconect();

                    ConferenceParticipant hostConfParticipant = (ConferenceParticipant) sbb.getApplicationSession().getAttribute(PhConstants.ATTRIB_HOST_SBB);
                    if (hostConfParticipant != null) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH] Disconnecting host from mediaserver");
                        }
                        hostConfParticipant.disconnectMediaServer();
                    }
                } else {
                    if (logger.isDebugEnabled()) {
                        logger.debug("[PH] Participant Unjoined the Conference, confID:: " + confID);
                    }

                    Iterator<ConferenceParticipant> confPartyItr = ((ConferenceController) sbb).getUnjoinedParticipants(event);
                    if (confPartyItr != null) {
                        while (confPartyItr.hasNext()) {
                            ConferenceParticipant confParty = confPartyItr.next();
                            if (logger.isDebugEnabled()) {
                                logger.debug("[PH] Disconnecting participant from mediaserver");
                            }
                            try {
                                confParty.disconnectMediaServer();
                            } catch (Exception e) {
                                logger.error("Exception in disconnect participant from media server", e);
                            }
                        }
                    }
                }
                
                CallData callData = SipProtocolUtil.getCallData(appSession);
                
                callData.set(CallDataAttribute.P_RECORD_END_TIME, new Date());
                
                if ((Integer) appSession.getAttribute(PhConstants.MEDIA_TYPE) == 1) {
                    ConferenceController controller = (ConferenceController) sbb;
                    AtomicInteger participantNumber = (AtomicInteger) controller.getApplicationSession().getAttribute(
                            PhConstants.NO_OF_VIDEO_PARTICIPATS);
                    if (participantNumber != null) {
                        int number = participantNumber.decrementAndGet();
                        if (isDebugEnabled) {
                            logger.debug("[PH] Number of participants after unjoining this is " + number);
                        }
                        if (number <= 4) {
                            String[] participants = (String[]) controller.getApplicationSession().getAttribute(PhConstants.DISPLAY_ID);
                            if (participants != null) {
                                synchronized (participants) {
                                    List<String> activeDisplayList = new ArrayList<String>();
                                    List<String> zeroDisplayList = new ArrayList<String>();
                                    for (int i = 0; i < participants.length; i++) {
                                        if (participants[i] != null) {
                                            activeDisplayList.add(String.valueOf(i + 1));
                                        } else {
                                            zeroDisplayList.add(String.valueOf(i + 1));
                                        }
                                    }
                                    if (isDebugEnabled) {
                                        logger.debug("[PH] Contracting a nine party layout to a four party layout video conference id " + confID + " is "
                                                             + participantNumber);
                                        logger.debug("[PH] activeDisplayList ::" + activeDisplayList);
                                        logger.debug("[PH] zeroDisplayList ::" + zeroDisplayList);
                                    }
                                    controller.getApplicationSession().setAttribute(PhConstants.UPDATE_CONF_SIZE, "true");
                                    controller.updateConference(getModifyConferenceSpec(confID, activeDisplayList, zeroDisplayList));
                                }
                            }
                        }
                    }
                }
            } else if (event.getEventId().equals(SBBEvent.EVENT_CONF_STREAM_UNJOINED)) {
            	
            	if (logger.isDebugEnabled()) {
                    logger.debug("[PH] EVENT_CONF_STREAM_UNJOINED.");
                }
            } else if (event.getEventId().equals("NOMEDIA_CONF")) {
                ((ConferenceController) sbb).destroyConference();
            } else if (event.getEventId().equals(SBBEvent.EVENT_CONF_UPDATED)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("[PH] Conference successfully updated.");
                }
                ConferenceController controller = (ConferenceController) sbb;
                Object object = controller.getApplicationSession().getAttribute(PhConstants.WAITING_PARTY);
                if (object instanceof List<?>) {
                    List<ConferenceParticipant> waitParty = (List<ConferenceParticipant>) object;
                    if (waitParty != null) {
                        for (ConferenceParticipant conferenceParticipant : waitParty) {
                            String joiningMode = ConferenceController.MODE_LISTEN_AND_TALK;
                            if (!conferenceParticipant.getDisplayRegionId().equalsIgnoreCase("-1")) {
                                joiningMode = ConferenceController.MODE_LISTEN_AND_TALK_VIDEO_IN_OUT;
                            }
                            if (logger.isDebugEnabled()) {
                                logger.debug("[PH] Joining waitng update conference participant to conference");
                            }
                            controller.join(new ConferenceParticipant[]{conferenceParticipant}, new String[]{joiningMode});
                        }
                        waitParty.clear();
                        controller.getApplicationSession().removeAttribute(PhConstants.UPDATE_CONF);
                    }
                }
            } else if (event.getEventId().equals(SBBEvent.EVENT_CONF_UPDATE_FAILED)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("[PH] Update Conference failed.");
                }
            } else if (event.getEventId().equals(SBBEvent.EVENT_CONF_DESTROYED)
                    || event.getEventId().equals(SBBEvent.EVENT_CONF_DESTROY_FAILED)) {

                ConferenceController controller = (ConferenceController) sbb;
                if (isDebugEnabled) {
                    logger.debug("[PH] Disconnecting conference with app session id" + controller.getApplicationSession().getId());
                }

                controller.disconect();
                removeConference(confID, appSession);
            } else if (event.getEventId().equals(SBBEvent.EVENT_DISCONNECTED) || event.getEventId().equals(
                    SBBEvent.EVENT_DISCONNECT_FAILED)) {
            	
            	CallData callData = SipProtocolUtil.getCallData(appSession);
            	
            	callData.set(CallDataAttribute.P_RECORD_END_TIME, new Date());
                if (logger.isDebugEnabled()) {
                    logger.debug("[PH] Conference controller disconnected");
                }
                removeConference(confID, appSession);
                boolean b2bConfMode = (Boolean) appSession.getAttribute(PhConstants.CONF_B2BUA_MODE);
                if(b2bConfMode){
                	
                	 String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
                	 if(logger.isDebugEnabled()){
                		 logger.debug(":: BYE received from legId : " + legId);
                	 }
                	 ServiceInterface serviceHandler = PhUtilityServices
                			 .getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
                	 Event appEvent = new Event(EventType.EVENT_DISCONNECT, Protocol.SIP, legId);

                	 ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
                }
            } else if (event.getEventId().equals(MsSessionController.EVENT_PLAY_COMPLETED) || event.getEventId().equals(
                    MsSessionController.EVENT_PLAY_FAILED)) {
            	  if (logger.isDebugEnabled()) {
                      logger.debug("[PH] Conference ply completed/play failed");
                  }
            	  
            	  if(event.getEventId().equals(MsSessionController.EVENT_PLAY_COMPLETED)){
            		  if(logger.isDebugEnabled()){
                 		 logger.debug(":: Inform service for successful call conference");
                 	 }
                 	 CallData callData = SipProtocolUtil.getCallData(appSession);
                 	 String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
                 	 ServiceInterface serviceHandler = PhUtilityServices
                 			 .getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
                 	 Event appEvent = new Event(EventType.EVENT_PLAY_SUCCESS, Protocol.SIP, legId);

                 	 ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
            	  }
            }

        } catch (Exception e) {
            logger.error("An exception while handling event." + e, e);
        }
        return SBBEventListener.CONTINUE;
    }

    protected void removeConference(String confID, SipApplicationSession appSession) {
        if (confID != null && appSession != null) {
            if (logger.isDebugEnabled()) {
                logger.debug("[PH] Removing conference from map:" + confID);
            }
            ConferenceHandler.getConfIdToAppSessionIdMap().remove(confID);
            appSession.removeAttribute(com.agnity.ph.common.PhConstants.ATTRIB_PARTICIPANT_CONFID);
        }
    }

    private MsConferenceSpec getModifyConferenceSpec(String confID, List<String> activeDisplayList, List<String> zeroDisplayList) {
        // Prepare Conference Spece for modify conference video layout
        MsConferenceSpec confSpec = new MsConferenceSpec();
        int activeDisplayListSize = activeDisplayList.size();
        confSpec.setMaxActiveSpeakers(activeDisplayList.size());
        confSpec.setNotifyActiveSpeaker(true);
        confSpec.setNotificationInterval(20);
        confSpec.setConferenceId(confID);
        confSpec.setDeleteConfFlag(MsConferenceSpec.DELETE_ON_NOCONTROL);

        MsVideoConferenceSpec videoConfSpec = new MsVideoConferenceSpec();

        int num_row = (int) Math.ceil(Math.sqrt(activeDisplayListSize));
        for (int i = 0; i < activeDisplayListSize; i++) {
            Double left = ((i % num_row) * 100) / (num_row * 1.0000);
            Double top = ((((int) (i / num_row))) * 100) / (num_row * 1.0000);
            DecimalFormat decimalFormat = new DecimalFormat("####.####");
            left = Double.valueOf(decimalFormat.format(left));
            top = Double.valueOf(decimalFormat.format(top));
            MsRegionSpec region = new MsRegionSpec();
            region.setId(activeDisplayList.get(i));
            region.setTop(top);
            region.setLeft(left);
            region.setRelativesize("1/" + num_row);
            videoConfSpec.addRegion(region);
        }

        for (String displayId : zeroDisplayList) {
            MsRegionSpec region = new MsRegionSpec();
            region.setId(displayId);
            region.setTop(0.0);
            region.setLeft(0.0);
            region.setRelativesize("0");
            videoConfSpec.addRegion(region);
        }

        MsRootSpec rootSpec = new MsRootSpec();
        rootSpec.setRootSize(MsRootSpec.QVGA);
        videoConfSpec.setRootSpec(rootSpec);
        confSpec.setConferenceType(MsConferenceSpec.VIDEO_TYPE);
        confSpec.setMsVideoConferenceSpec(videoConfSpec);

        return confSpec;
    }

    public void activate(SBB sbb) {
        sbb.getServletContext().log("activate called on Event Listener");
    }
}

