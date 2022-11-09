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

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipServletRequest;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.CallData;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.sbb.ConferenceController;
import com.baypackets.ase.sbb.ConferenceParticipant;
import com.baypackets.ase.sbb.MsConferenceSpec;
import com.baypackets.ase.sbb.MsRootSpec;
import com.baypackets.ase.sbb.MsSessionController;
import com.baypackets.ase.sbb.MsVideoConferenceSpec;
import com.baypackets.ase.sbb.SBB;
import com.baypackets.ase.sbb.SBBEvent;
import com.baypackets.ase.sbb.SBBEventListener;


/**
 * This Class will work as a listener for conference participant sbb. 
 * @author Amit Baxi
 *
 */
public class ParticipantEventListener implements SBBEventListener {

    private static final long serialVersionUID = 3856944400650693023L;
    private static Logger logger = Logger.getLogger(ParticipantEventListener.class);

    public int handleEvent(SBB sbb, SBBEvent event) {

        boolean isDebugEnabled = logger.isDebugEnabled();
        if (isDebugEnabled) {
            logger.debug("[PH] handleEvent event is: " + event.getEventId() + "for SBB: " + sbb);
        }
        SipApplicationSession appSession = sbb.getApplicationSession();
        try {
            if (event.getEventId().equals(SBBEvent.EVENT_CONNECTED)) {

                String confID = (String) appSession.getAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID);
                if (ConferenceHandler.getConfIdToAppSessionIdMap().containsKey(confID)) {
                    String joiningMode = ConferenceController.MODE_LISTEN_AND_TALK;
                    ConferenceParticipant confparticipant = (ConferenceParticipant) sbb;
                    String appSessionId = ConferenceHandler.getConfIdToAppSessionIdMap().get(confID);
                    
                    CallData callData=SipProtocolUtil.getCallData(appSession); 
                    ConferenceController controller = (ConferenceController) SipProtocolUtil.getAppSession(appSessionId,(String)callData.get(CallDataAttribute.SERVICE_ID))
                            .getAttribute(PhConstants.ATTRIB_CONF_CONTROLLER);
                    if (isDebugEnabled){
                        logger.debug("[PH]conference app session id" + controller.getApplicationSession().getId());
                    }
                    if (controller != null) {
                        if ((Integer) appSession.getAttribute(PhConstants.MEDIA_TYPE) == 1) {
                            String[] participants = (String[]) controller.getApplicationSession().getAttribute(PhConstants.DISPLAY_ID);
                            if (participants != null) {
                                synchronized (participants) {
                                    if (isDebugEnabled) {
                                        logger.debug("[PH] participants display regions layout size :: " + participants.length);
                                    }
                                    joiningMode = ConferenceController.MODE_LISTEN_AND_TALK_VIDEO_IN_OUT;
                                    int displayID = -1;
                                    for (int i = 0; i < participants.length; i++) {
                                        if (participants[i] == null) {
                                            participants[i] = confparticipant.getId();
                                            displayID = i + 1;
                                            break;
                                        }
                                    }
                                    confparticipant.setDisplayRegionId(String.valueOf(displayID));
                                    if (isDebugEnabled) {
                                        logger.debug("[PH] conference joing in video mode by participant " + confparticipant.getId()
                                                             + " with display id " + displayID);
                                    }
                                    AtomicInteger participantNumber = (AtomicInteger) controller.getApplicationSession().getAttribute(
                                            PhConstants.NO_OF_VIDEO_PARTICIPATS);
                                    if (participantNumber != null) {
                                        synchronized (participantNumber) {
                                            if (isDebugEnabled) {
                                                logger.debug("[PH] Number of participants joined in video mode is " + participantNumber);
                                            }
                                            if (participantNumber.get() > 3) {
                                                if (isDebugEnabled) {
                                                    logger.debug("[PH] Number of Participants joined in conference id " + confID + " is "
                                                                         + participantNumber);
                                                }
                                                if (participants.length == PhConstants.FOUR_PARTY) {
                                                    if (isDebugEnabled) {
                                                        logger.debug("[PH] Expanding a four party layout to a nine party layout video conference id "
                                                                             + confID);
                                                    }
                                                    String[] newParticipants = new String[PhConstants.NINE_PARTY];
                                                    System.arraycopy(participants, 0, newParticipants, 0, PhConstants.FOUR_PARTY);
                                                    participants = newParticipants;
                                                    controller.getApplicationSession().setAttribute(PhConstants.DISPLAY_ID, participants);
                                                    controller.getApplicationSession().setAttribute(PhConstants.UPDATE_CONF, "true");
                                                    controller.updateConference(getModifyConferenceSpec(confID, PhConstants.NINE_PARTY));
                                                    Object object = controller.getApplicationSession()
                                                            .getAttribute(PhConstants.WAITING_PARTY);
                                                    if (object instanceof List<?>) {
                                                        List<ConferenceParticipant> waitParty = (List<ConferenceParticipant>) object;
                                                        if (waitParty != null) {
                                                            synchronized (waitParty) {
                                                                if (participantNumber.get() == PhConstants.FOUR_PARTY) {
                                                                    participants[4] = confparticipant.getId();
                                                                    confparticipant.setDisplayRegionId(String.valueOf(5));
                                                                }
                                                                waitParty.add((ConferenceParticipant) sbb);
                                                            }
                                                        }
                                                        if (isDebugEnabled) {
                                                            logger.debug("[PH] Waiting this party for completing conference update");
                                                        }
                                                        controller.getApplicationSession().setAttribute(PhConstants.WAITING_PARTY, waitParty);
                                                    }
                                                } else {
                                                    String confUpdateSize = (String) controller.getApplicationSession().getAttribute(PhConstants.UPDATE_CONF_SIZE);
                                                    if (confUpdateSize != null) {
                                                        controller.getApplicationSession().setAttribute(PhConstants.UPDATE_CONF, "true");
                                                        controller.updateConference(getModifyConferenceSpec(confID, PhConstants.NINE_PARTY));
                                                        controller.getApplicationSession().removeAttribute(PhConstants.UPDATE_CONF_SIZE);
                                                        Object object = controller.getApplicationSession()
                                                                .getAttribute(PhConstants.WAITING_PARTY);
                                                        if (object instanceof List<?>) {
                                                            List<ConferenceParticipant> waitParty = (List<ConferenceParticipant>) object;
                                                            if (waitParty != null) {
                                                                synchronized (waitParty) {
                                                                    waitParty.add((ConferenceParticipant) sbb);
                                                                }
                                                            }
                                                            if (isDebugEnabled) {
                                                                logger.debug("[PH] Waiting this party for completing conference update");
                                                            }
                                                            controller.getApplicationSession().setAttribute(PhConstants.WAITING_PARTY, waitParty);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        String confUpdate = (String) controller.getApplicationSession().getAttribute(PhConstants.UPDATE_CONF);
                        if (confUpdate == null) {
                            controller.join(new ConferenceParticipant[]{confparticipant}, new String[]{joiningMode});
                            if (isDebugEnabled) {
                                logger.debug("[PH] sending join for other parties");
                            }
                        } else {
                            if (isDebugEnabled) {
                                logger.debug("[PH] Waiting other party for completing conference update");
                            }
                            Object object = controller.getApplicationSession().getAttribute(PhConstants.WAITING_PARTY);
                            if (object instanceof List<?>) {
                                List<ConferenceParticipant> waitParty = (List<ConferenceParticipant>) object;
                                if (waitParty != null) {
                                    synchronized (waitParty) {
                                        if (waitParty != null) {
                                            waitParty.add(confparticipant);
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                	 if (isDebugEnabled) {
                         logger.debug("[PH] Confrence app sesison not found.");
                     }
                }
            }

            if (event.getEventId().equals(SBBEvent.EVENT_CONNECT_FAILED)) {
                if (isDebugEnabled) {
                    logger.debug("[PH] Connect to MS failed for participant.");
                }
                if (appSession.getAttribute(PhConstants.ATTRIB_HOST_SESSION) != null) {
                    logger.error("Host was unable to connect to media server so destroying conference");
                    String confID = (String) sbb.getApplicationSession().getAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID);
                    if (ConferenceHandler.getConfIdToAppSessionIdMap().containsKey(confID)) {
                        String appSessionId = ConferenceHandler.getConfIdToAppSessionIdMap().get(confID);
                        
                        CallData callData=SipProtocolUtil.getCallData(appSession);
                        
                        ConferenceController controller = (ConferenceController) SipProtocolUtil.getAppSession(appSessionId,(String)callData.get(CallDataAttribute.SERVICE_ID)).getAttribute(PhConstants.ATTRIB_CONF_CONTROLLER);
                        if (controller != null) {
                            if (isDebugEnabled) {
                                logger.debug("[PH] Destroying conference with app session id" + controller.getApplicationSession().getId());
                            }
                            controller.destroyConference();
                        }
                    }
                }
            }

            if (event.getEventId().equals(SBBEvent.EVENT_DISCONNECTED) || event.getEventId().equals(SBBEvent.EVENT_DISCONNECT_FAILED)) {

            	 if (isDebugEnabled) {
                     logger.debug("[PH] EVENT_DISCONNECTED/EVENT_DISCONNECT_FAILED");
                 }
            }

            if (event.getEventId().equals(SBBEvent.EVENT_SIG_IN_PROGRESS)) {
                SipServletRequest request = (SipServletRequest) event.getMessage();
                if (request.getMethod().equals(PhConstants.INFO_REQUEST)) {
//                    try {
//                        //request.createResponse(200).send();
//                    } catch (Exception e) {
//                        logger.error("[PH] An exception occured while sending 200 OK for extra INFO received.");
//                    }
                    return SBBEventListener.NOOP;

                } else if (request.getMethod().equals(PhConstants.BYE_REQUEST)) {

                    if (sbb.getA().getId().equals(event.getMessage().getSession().getId())) {

                        String confID = (String) appSession.getAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID);
                        if (confID != null) {
                            String appSessionId = ConferenceHandler.getConfIdToAppSessionIdMap().get(confID);
                            
                            CallData callData=SipProtocolUtil.getCallData(appSession);
                            SipApplicationSession controllerSession = SipProtocolUtil.getAppSession(appSessionId,(String)callData.get(CallDataAttribute.SERVICE_ID));
                            ConferenceParticipant confParticipant = (ConferenceParticipant) sbb;

                            if (controllerSession != null && controllerSession.getAttribute(PhConstants.ATTRIB_CONF_CONTROLLER) != null) {

                                ConferenceController controller = (ConferenceController) controllerSession.getAttribute(PhConstants.ATTRIB_CONF_CONTROLLER);
                                sbb.removeA();
                                if (appSession.getAttribute(PhConstants.ATTRIB_HOST_SESSION) != null) {
                                    if (isDebugEnabled) {
                                        logger.debug("[PH] host is unjoining from conference");
                                    }
                                    controller.getApplicationSession().setAttribute(com.agnity.ph.common.PhConstants.ATTRIB_CONF_HOST_UNJOINED, true);
                                    controller.getApplicationSession().setAttribute(PhConstants.ATTRIB_HOST_SBB, sbb);
                                }
                                if ((Integer) appSession.getAttribute(PhConstants.MEDIA_TYPE) == 1) {
                                    String[] participants = (String[]) controller.getApplicationSession()
                                            .getAttribute(PhConstants.DISPLAY_ID);
                                    if (participants != null) {

                                        synchronized (participants) {
                                            for (int i = 0; i < participants.length; i++) {
                                                if (participants[i] == confParticipant.getId()) {
                                                    if (isDebugEnabled) {
                                                        logger.debug("[PH] Setting blank participant for display id :: " + (i + 1));
                                                    }
                                                    participants[i] = null;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                                controller.unjoin(new ConferenceParticipant[]{confParticipant});
                                if (isDebugEnabled){
                                    logger.debug("[PH] sending unjoin only if party is in conference");
                                }
                                
                                		
                            } else {
                                if (isDebugEnabled){
                                    logger.debug("[PH] party is not in conf, so only disconnecting with media server");
                                }
                                sbb.removeA();
                                confParticipant.disconnectMediaServer();
                            }

                        }
                    } else if (sbb.getB().getId().equals(event.getMessage().getSession().getId())) {
                        if (isDebugEnabled){
                            logger.debug("[PH] SIG IN progress sending CONTINUE for MediaServer BYE");
                        }

                        //destroy conference case


                        return SBBEventListener.CONTINUE;
                    }
                }
                if (isDebugEnabled){
                    logger.debug("[PH] SIG IN progress sending NOOP");
                }
                return SBBEventListener.NOOP;
            }
            

            // join event
            if (event.getEventId().equals(MsSessionController.EVENT_PLAY_COMPLETED) || event.getEventId().equals(
                    MsSessionController.EVENT_PLAY_FAILED)) {

            	 if (isDebugEnabled){
                     logger.debug("[PH] EVENT_PLAY_COMPLETED/EVENT_PLAY_FAILED");
                 }
            }
            
            if(event.getEventId().equals(SBBEvent.EVENT_CONNECT_PROGRESS)){
            	if(logger.isDebugEnabled()){
            		logger.debug("[PH] EVENT_CONNECT PROGRESS received ");
            	}
            	
            	return SBBEventListener.NOOP;
            }
            
        } catch (Exception e) {
            logger.error("An exception occured in handling of event: " + e.getMessage(), e);
            e.printStackTrace();
        }
        return SBBEventListener.CONTINUE;
    }

    private MsConferenceSpec getModifyConferenceSpec(String confID, int noOfRegion) {
        // Prepare Conference Spece for modify conference video layout
        MsConferenceSpec confSpec = new MsConferenceSpec();
        confSpec.setMaxActiveSpeakers(noOfRegion);
        confSpec.setNotifyActiveSpeaker(true);
        confSpec.setNotificationInterval(20);
        confSpec.setConferenceId(confID);
        confSpec.setDeleteConfFlag(MsConferenceSpec.DELETE_ON_NOCONTROL);

        MsVideoConferenceSpec videoConfSpec = new MsVideoConferenceSpec();
        videoConfSpec.setLayoutSize(noOfRegion);
        MsRootSpec rootSpec = new MsRootSpec();
        rootSpec.setRootSize(MsRootSpec.QVGA);
        videoConfSpec.setRootSpec(rootSpec);
        confSpec.setConferenceType(MsConferenceSpec.VIDEO_TYPE);
        confSpec.setMsVideoConferenceSpec(videoConfSpec);

        return confSpec;
    }

    @Override
    public void activate(SBB arg0) {
        // TODO Auto-generated method stub
    }
}