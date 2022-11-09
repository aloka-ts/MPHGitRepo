/*******************************************************************************
 * Copyright (c) 2011 Agnity, Inc. All rights reserved.
 * <p>
 * This is proprietary source code of Agnity, Inc.
 * <p>
 * Agnity, Inc. retains all intellectual property rights associated
 * with this source code. Use is subject to license terms.
 * <p>
 * This source code contains trade secrets owned by Agnity, Inc.
 * Confidentiality of this computer program must be maintained at
 * all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.ph.inapcs1scf;

import com.agnity.mphdata.common.*;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.genband.tcap.provider.TcapSession;
import jain.protocol.ss7.tcap.TcapConstants;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * This class is used to define the Functional state Machine of INAP protocol.it
 * maintains the mapping of various INAP incoming messages with the possible
 * INAP protocol states.so when an INAP message is received from network it
 * validates it against current state of call. if the event is not valid for the
 * current state ,the validation fails and the error response is sent by protocol handler.
 */
public class InapCS1ScfProtocolFSMHandler {

    private static Logger logger = Logger.getLogger(InapCS1ScfProtocolFSMHandler.class);

    /**
     * This map will store mapping of call state to possible events in state
     */
    private static Map<InapCallStates, Set<InapCS1ScfProtocolEvent>> inapValidEventsMap = new HashMap<InapCallStates, Set<InapCS1ScfProtocolEvent>>();

    /**
     * Create a Map which specifies what all events are valid in which state.
     * INIT state - BEGIN and IDP are expected
     * CONN_IN_PROGRESS -
     * CONNECTED - TERMINATED -
     */
    static {
        LinkedHashSet<InapCS1ScfProtocolEvent> initInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        initInapEvent.add(InapCS1ScfProtocolEvent.BEGIN);
        initInapEvent.add(InapCS1ScfProtocolEvent.IDP);
        inapValidEventsMap.put(InapCallStates.INIT, initInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> connInProgInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.CONTINUE);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.AT_TIMEOUT);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.END);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.UABORT);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.PABORT);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.NOTICE);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.ERB);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.ERB_ANS);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.ERB_BUSY);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.ERB_NOANS);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.ERB_ABANDON);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.ERB_DISCONNECT);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.ERB_ROUTESELECTFAILURE);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.RETURNERROR);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.RETURNRESULT);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.UREJECT);
        connInProgInapEvent.add(InapCS1ScfProtocolEvent.RRBCSM_TIMEOUT);
        inapValidEventsMap.put(InapCallStates.TERM_CONNECT_IN_PROGRESS, connInProgInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> connectedInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        connectedInapEvent.add(InapCS1ScfProtocolEvent.CONTINUE);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.AT_TIMEOUT);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.END);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.UABORT);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.PABORT);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.NOTICE);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.CDR_TIMEOUT);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.ERB);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.ERB_DISCONNECT);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.RETURNERROR);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.RETURNRESULT);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.UREJECT);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.ACR);
        connectedInapEvent.add(InapCS1ScfProtocolEvent.ENC);
        inapValidEventsMap.put(InapCallStates.TERM_CONNECTED, connectedInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> terminatedInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        terminatedInapEvent.add(InapCS1ScfProtocolEvent.ACR);
        terminatedInapEvent.add(InapCS1ScfProtocolEvent.END);
        terminatedInapEvent.add(InapCS1ScfProtocolEvent.ENC);
        inapValidEventsMap.put(InapCallStates.TERMINATED, terminatedInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> serviceInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        serviceInapEvent.add(InapCS1ScfProtocolEvent.CONTINUE);
        serviceInapEvent.add(InapCS1ScfProtocolEvent.AT_TIMEOUT);
        serviceInapEvent.add(InapCS1ScfProtocolEvent.END);
        serviceInapEvent.add(InapCS1ScfProtocolEvent.ERB);
        serviceInapEvent.add(InapCS1ScfProtocolEvent.UABORT);
        serviceInapEvent.add(InapCS1ScfProtocolEvent.PABORT);
        serviceInapEvent.add(InapCS1ScfProtocolEvent.NOTICE);
        inapValidEventsMap.put(InapCallStates.SERVICE_LOGIC, serviceInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> handOffInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        handOffInapEvent.add(InapCS1ScfProtocolEvent.CONTINUE);
        handOffInapEvent.add(InapCS1ScfProtocolEvent.END);
        handOffInapEvent.add(InapCS1ScfProtocolEvent.ERB);
        handOffInapEvent.add(InapCS1ScfProtocolEvent.UABORT);
        handOffInapEvent.add(InapCS1ScfProtocolEvent.PABORT);
        handOffInapEvent.add(InapCS1ScfProtocolEvent.NOTICE);
        handOffInapEvent.add(InapCS1ScfProtocolEvent.RETURNERROR);
        handOffInapEvent.add(InapCS1ScfProtocolEvent.RETURNRESULT);
        handOffInapEvent.add(InapCS1ScfProtocolEvent.UREJECT);
        handOffInapEvent.add(InapCS1ScfProtocolEvent.CORRELATION_TIMEOUT);
        inapValidEventsMap.put(InapCallStates.HANDOFF, handOffInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> assistInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        assistInapEvent.add(InapCS1ScfProtocolEvent.CONTINUE);
        assistInapEvent.add(InapCS1ScfProtocolEvent.AT_TIMEOUT);
        assistInapEvent.add(InapCS1ScfProtocolEvent.END);
        assistInapEvent.add(InapCS1ScfProtocolEvent.ERB);
        assistInapEvent.add(InapCS1ScfProtocolEvent.UABORT);
        assistInapEvent.add(InapCS1ScfProtocolEvent.PABORT);
        assistInapEvent.add(InapCS1ScfProtocolEvent.NOTICE);
        assistInapEvent.add(InapCS1ScfProtocolEvent.RETURNERROR);
        assistInapEvent.add(InapCS1ScfProtocolEvent.RETURNRESULT);
        assistInapEvent.add(InapCS1ScfProtocolEvent.CORRELATION_TIMEOUT);
        assistInapEvent.add(InapCS1ScfProtocolEvent.UREJECT);
        inapValidEventsMap.put(InapCallStates.ASSIST, assistInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> terminationInProgresInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        terminationInProgresInapEvent.add(InapCS1ScfProtocolEvent.CONTINUE);
        terminationInProgresInapEvent.add(InapCS1ScfProtocolEvent.END);
        terminationInProgresInapEvent.add(InapCS1ScfProtocolEvent.AT_TIMEOUT);
        terminationInProgresInapEvent.add(InapCS1ScfProtocolEvent.ACR);
        terminationInProgresInapEvent.add(InapCS1ScfProtocolEvent.ENC);
        inapValidEventsMap.put(InapCallStates.TERMINATION_IN_PROGRESS, terminationInProgresInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> msConnectInProgresInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        msConnectInProgresInapEvent.add(InapCS1ScfProtocolEvent.BEGIN);
        msConnectInProgresInapEvent.add(InapCS1ScfProtocolEvent.CONTINUE);
        msConnectInProgresInapEvent.add(InapCS1ScfProtocolEvent.END);
        msConnectInProgresInapEvent.add(InapCS1ScfProtocolEvent.ARI);
        msConnectInProgresInapEvent.add(InapCS1ScfProtocolEvent.AT_TIMEOUT);
        inapValidEventsMap.put(InapCallStates.MS_CONNECT_INPROGRESS, msConnectInProgresInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> termConnectedACRInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        termConnectedACRInapEvent.add(InapCS1ScfProtocolEvent.PABORT);
        termConnectedACRInapEvent.add(InapCS1ScfProtocolEvent.UABORT);
        termConnectedACRInapEvent.add(InapCS1ScfProtocolEvent.CONTINUE);
        termConnectedACRInapEvent.add(InapCS1ScfProtocolEvent.END);
        termConnectedACRInapEvent.add(InapCS1ScfProtocolEvent.NOTICE);
        termConnectedACRInapEvent.add(InapCS1ScfProtocolEvent.UREJECT);
        termConnectedACRInapEvent.add(InapCS1ScfProtocolEvent.RETURNERROR);
        termConnectedACRInapEvent.add(InapCS1ScfProtocolEvent.RETURNRESULT);
        termConnectedACRInapEvent.add(InapCS1ScfProtocolEvent.AT_TIMEOUT);
        inapValidEventsMap.put(InapCallStates.TERM_CONNECTED_ACR, termConnectedACRInapEvent);

        LinkedHashSet<InapCS1ScfProtocolEvent> userInterInProgInapEvent = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.PABORT);
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.UABORT);
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.CONTINUE);
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.END);
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.NOTICE);
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.UREJECT);
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.RETURNERROR);
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.RETURNRESULT);
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.AT_TIMEOUT);
        assistInapEvent.add(InapCS1ScfProtocolEvent.ERB);
        userInterInProgInapEvent.add(InapCS1ScfProtocolEvent.SRR);
        inapValidEventsMap.put(InapCallStates.USER_INTREACTION_IN_PROGRESS, userInterInProgInapEvent);
        inapValidEventsMap.put(InapCallStates.MS_PLAY, userInterInProgInapEvent);
        inapValidEventsMap.put(InapCallStates.MS_PLAYCOLLECT, userInterInProgInapEvent);
        
        LinkedHashSet<InapCS1ScfProtocolEvent> outgoingDialogue = new LinkedHashSet<InapCS1ScfProtocolEvent>();
        outgoingDialogue.add(InapCS1ScfProtocolEvent.PABORT);
        outgoingDialogue.add(InapCS1ScfProtocolEvent.UABORT);
        outgoingDialogue.add(InapCS1ScfProtocolEvent.CONTINUE);
        outgoingDialogue.add(InapCS1ScfProtocolEvent.END);
        outgoingDialogue.add(InapCS1ScfProtocolEvent.NOTICE);
        outgoingDialogue.add(InapCS1ScfProtocolEvent.UREJECT);
        outgoingDialogue.add(InapCS1ScfProtocolEvent.RETURNERROR);
        outgoingDialogue.add(InapCS1ScfProtocolEvent.RETURNRESULT);
        outgoingDialogue.add(InapCS1ScfProtocolEvent.AT_TIMEOUT);
        outgoingDialogue.add(InapCS1ScfProtocolEvent.CONNECT);
        inapValidEventsMap.put(InapCallStates.OTG_INITIATE_CALL, outgoingDialogue);
        
    }

    /**
     * This method validates the INAP event against current state in call data
     *
     * @return true if valid event. false in case of error or invalid event
     */
    public static boolean validateFSMState(InapCS1ScfProtocolEvent inapProtocolEvent, TcapSession tcapSession) {
        if (logger.isDebugEnabled()) {
            logger.debug("[PH]:: validateFSMState Enter");
        }
        CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
        int dialogueId = tcapSession.getDialogueId();

        if (callData == null) {
            if (logger.isDebugEnabled()) {
                logger.debug("[PH]:: Call data is null return false ");
            }

            //create dummy call data for error handling
            callData = new CallData();

            callData.set(CallDataAttribute.P_PROTOCOL, Protocol.ITUINAPCS1_SCF);
            callData.set(CallDataAttribute.P_DIALOG_ID, dialogueId);
            callData.set(CallDataAttribute.NP_FT_CALL, true);
            callData.set(CallDataAttribute.P_ATTEMPTED_IND, 4);

            tcapSession.setAttribute(CallData.CALL_DATA, callData);
            if (logger.isDebugEnabled()) {
                logger.debug("[PH]:: New call data created and set in TCAP session");
            }
            return false;
        }

        LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
        if (legData == null) {
            logger.error("[PH]:: validateFSMState - LegData is NULL for Dialogue Id :" + tcapSession.getDialogueId());
            return false;
        }

        if (logger.isDebugEnabled()) {
            logger.debug("[PH]:: Inside validateFSMState for event[" + inapProtocolEvent + "]");
        }

        // fetch current call state
        InapCallStates currCallState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

        if (logger.isDebugEnabled()) {
            logger.debug("[PH]:: curr call state is [" + currCallState + "]");
        }

        //  check if valid message
        //  For FT call and Non timeout events mark event as invalid
        if ((PhConstants.TRUE.equals(callData.get(CallDataAttribute.NP_FT_CALL)) &&
                (currCallState != InapCallStates.TERM_CONNECTED) &&
                (inapProtocolEvent != InapCS1ScfProtocolEvent.AT_TIMEOUT &&
                        inapProtocolEvent != InapCS1ScfProtocolEvent.CORRELATION_TIMEOUT &&
                        inapProtocolEvent != InapCS1ScfProtocolEvent.CDR_TIMEOUT))) {
            if (logger.isInfoEnabled()) {
                logger.info("[PH]:: Event received after SAS failover");
                logger.info("[PH]:: Call is not in connected state invalid state after FT; we should drop a transient call");
            }

            // for this scenario set failed call set attempted call ind for this scenario
            callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
            return false;
        }

        //fetch valid events for current call state
        Set<InapCS1ScfProtocolEvent> validEvents = inapValidEventsMap.get(currCallState);

        if (logger.isDebugEnabled()) {
            logger.debug("[PH]:: Possible events for curr call state are [" + validEvents + "]");
        }

        // fetch dialogue primitive
        int rxDialogType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);

        // match current event to valid event list
        if (validEvents.contains(inapProtocolEvent)) {
            if (logger.isDebugEnabled()) {
                logger.debug("[PH]:: current event is allowed for given call state; checking event specific preconditions::");
            }
            boolean status = true;

			 /*
             * event specific checks.. like IDP in begin erb as per armed events.Currently ERB is
			 * not coming as individual events so individual ERBs validation
			 * code will not be invoked
			 */
            switch (inapProtocolEvent) {
                case IDP: {
                    if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: IDP not part of begin dialouge. Last dialog recived::[" + rxDialogType + "]");
                        }
                        status = false;
                    }
                    break;
                }
                case ERB_ROUTESELECTFAILURE: {
                    // Here null should be replaced with legid for which ERB is received
                    Set<Action.ERB_TYPE> erbSet = InapCS1ScfProtocolUtil.getErbSetByApplication(callData, null);
                    if (erbSet == null) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
                        }
                        status = false;
                    } else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ROUTESELECTFAILURE))) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set doesn't contain ERB_ROUTESELECTFAILURE");
                        }
                        status = false;
                    }
                    if (!status) {
                        tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
                    }
                    break;
                }
                case ERB_BUSY: {
                    Set<Action.ERB_TYPE> erbSet = InapCS1ScfProtocolUtil.getErbSetByApplication(callData, null);
                    if (erbSet == null) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
                        }
                        status = false;
                    } else if (!(erbSet.contains(Action.ERB_TYPE.ERB_BUSY))) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set doesn't contain ERB_BUSY");
                        }
                        status = false;
                    }
                    if (!status) {
                        tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
                    } else {
                        switch (rxDialogType) {
                            case TcapConstants.PRIMITIVE_CONTINUE:
                            case TcapConstants.PRIMITIVE_END:
                                break;
                            default: {
                                if (logger.isDebugEnabled()) {
                                    logger.debug("[PH]:: ERB_BUSY not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
                                }
                                status = false;
                                break;
                            }
                        }
                    }
                    break;
                }
                case ERB_NOANS: {
                    Set<Action.ERB_TYPE> erbSet = InapCS1ScfProtocolUtil.getErbSetByApplication(callData, null);
                    if (erbSet == null) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
                        }
                        status = false;
                    } else if (!(erbSet.contains(Action.ERB_TYPE.ERB_NO_ANSWER))) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set doesn't contain ERB_NOANS");
                        }
                        status = false;
                    }

                    if (!status) {
                        tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
                    } else {

                        switch (rxDialogType) {
                            case TcapConstants.PRIMITIVE_CONTINUE:
                            case TcapConstants.PRIMITIVE_END:
                                break;
                            default: {
                                if (logger.isDebugEnabled()) {
                                    logger.debug("[PH]:: ERB_NOANS not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
                                }
                                status = false;
                                break;
                            }
                        }
                    }
                    break;
                }
                case ERB_ANS: {
                    Set<Action.ERB_TYPE> erbSet = InapCS1ScfProtocolUtil.getErbSetByApplication(callData, null);
                    if (erbSet == null) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
                        }
                        status = false;
                    } else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ANSWER))) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set doesn't contain ERB_ANS");
                        }
                        status = false;
                    }
                    if (!status) {
                        tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
                    } else {
                        switch (rxDialogType) {
                            case TcapConstants.PRIMITIVE_CONTINUE:
                                break;
                            default: {
                                if (logger.isDebugEnabled()) {
                                    logger.debug("[PH]:: ERB_ANS not part of Continue dialouge. Last dialog recived::[" + rxDialogType + "]");
                                }
                                status = false;
                                break;
                            }
                        }
                    }
                    break;
                }
                case ERB_DISCONNECT: {
                    Set<Action.ERB_TYPE> erbSet = InapCS1ScfProtocolUtil.getErbSetByApplication(callData, null);
                    if (erbSet == null) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
                        }
                        status = false;
                    } else if (!(erbSet.contains(Action.ERB_TYPE.ERB_DISCONNECT))) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set doesn't contain ERB_DISCONNECT");
                        }
                        status = false;
                    }
                    if (!status) {
                        tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
                    } else {
                        switch (rxDialogType) {
                            case TcapConstants.PRIMITIVE_CONTINUE:
                            case TcapConstants.PRIMITIVE_END:
                                break;
                            default: {
                                if (logger.isDebugEnabled()) {
                                    logger.debug("[PH]:: ERB_DISCONNECT not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
                                }
                                status = false;
                                break;
                            }
                        }
                    }
                    break;
                }
                case ERB_ABANDON: {
                    Set<Action.ERB_TYPE> erbSet = InapCS1ScfProtocolUtil.getErbSetByApplication(callData, null);
                    if (erbSet == null) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
                        }
                        status = false;
                    } else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ABANDON))) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set doesn't contain ERB_OABANDON");
                        }
                        status = false;
                    }
                    if (!status) {
                        tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
                    } else {
                        switch (rxDialogType) {
                            case TcapConstants.PRIMITIVE_CONTINUE:
                            case TcapConstants.PRIMITIVE_END:
                                break;
                            default: {
                                if (logger.isDebugEnabled()) {
                                    logger.debug("[PH]:: ERB_ABANDON not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
                                }
                                status = false;
                                break;
                            }
                        }
                    }
                    break;
                }
                case ERB_MIDCALL: {
                    Set<Action.ERB_TYPE> erbSet = InapCS1ScfProtocolUtil.getErbSetByApplication(callData, null);
                    if (erbSet == null) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
                        }
                        status = false;
                    } else if (!(erbSet.contains(Action.ERB_TYPE.ERB_MIDCALL))) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Erb set doesn't contain ERB_MIDCALL");
                        }
                        status = false;
                    }
                    if (!status) {
                        tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
                    } else {
                        switch (rxDialogType) {
                            case TcapConstants.PRIMITIVE_CONTINUE:
                            case TcapConstants.PRIMITIVE_END:
                                break;
                            default: {
                                if (logger.isDebugEnabled()) {
                                    logger.debug("[PH]:: ERB_MIDCALL not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
                                }
                                status = false;
                                break;
                            }
                        }
                    }
                    break;
                }
                case ACR: {
                    if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: ACR not part of Continue dialouge. Last dialog recived::[" + rxDialogType + "]");
                        }
                        status = false;
                    }
                    break;
                }
                case SRR: {
                    if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: SRR not part of Continue dialouge. Last dialog recived::[" + rxDialogType + "]");
                        }
                        status = false;
                    }
                    break;
                }
                case ENC: {
                    if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: ENC not part of Continue dialouge. Last dialog recived::[" + rxDialogType + "]");
                        }
                        status = false;
                    }
                    break;
                }
                case END: {
                    if (rxDialogType != TcapConstants.PRIMITIVE_END) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: END is not part of END dialouge. Last dialog recived::[" + rxDialogType + "]");
                        }
                        status = false;
                    }
                    break;
                }
                case CONNECT: {
                    if (rxDialogType != TcapConstants.PRIMITIVE_END) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("[PH]:: Outgoing Call, Connect not rxed in ENDDIalogue.[" + rxDialogType + "]");
                        }
                        status = false;
                    }
                    break;
                }
                default: {
                    // status should remain as true;
                    status = true;
                    break;
                }
            }
            if (logger.isDebugEnabled()) {
                logger.debug("[PH]:: Return with validation status::[" + status + "]");
            }
            return status;
        } else {
            if (logger.isDebugEnabled()) {
                logger.debug("[PH]:: event is NOT allowed for cuee call state return false");
            }
            return false;
        }
    }

    public static enum AbortInfoEnum {

        NO_REASON_GIVEN(1),
        APPLICATION_TIMER_EXPIRED(2),
        PROTOCOL_PROHIBITED_SIGNAL_RECIEVED(3),
        ABNORMAL_PROCESSING(4),
        CONGESTION(5),
        AC_NEGOTIATION_FAILED(6),
        UNRECOGNIZED_EXTENSION_PARAMETER(7),
        
        //BNS
        BNS_APPLICATION_ERROR(8),
        LIDB_PROTOCOL_ERROR(9),
        //GN
        GN_RESPONSE_ERROR(10),
        PROVIDE_INSTRUCTION_PARSE_ERROR(11);

        private int code;

        private AbortInfoEnum(int i) {
            this.code = i;
        }

        public static AbortInfoEnum fromInt(int num) {
            AbortInfoEnum abortInfo = NO_REASON_GIVEN;
            switch (num) {
                case 1: {
                    abortInfo = NO_REASON_GIVEN;
                    break;
                }
                case 2: {
                    abortInfo = APPLICATION_TIMER_EXPIRED;
                    break;
                }
                case 3: {
                    abortInfo = PROTOCOL_PROHIBITED_SIGNAL_RECIEVED;
                    break;
                }
                case 4: {
                    abortInfo = ABNORMAL_PROCESSING;
                    break;
                }
                case 5: {
                    abortInfo = CONGESTION;
                    break;
                }
                case 6: {
                    abortInfo = AC_NEGOTIATION_FAILED;
                    break;
                }
                case 7: {
                    abortInfo = UNRECOGNIZED_EXTENSION_PARAMETER;
                    break;
                }
                default: {
                    abortInfo = NO_REASON_GIVEN;
                    break;
                }
            }
            return abortInfo;
        }

        public int getCode() {
            return code;
        }
    }
}
