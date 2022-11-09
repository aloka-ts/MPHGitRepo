package com.agnity.ph.inapcs1scf.flowhelper;

import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.src;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.InvokeReqEvent;
import jain.protocol.ss7.tcap.component.Operation;
import jain.protocol.ss7.tcap.component.Parameters;

import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.Logger;

import com.agnity.inapitutcs2.asngenerated.BCSMEvent;
import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolHelper;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolParser;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil;
import com.agnity.ph.inapcs1scf.InapCS1ScfRelReasonCode;
import com.agnity.ph.inapcs1scf.messagehelper.InapCS1ERBSetHelper;
import com.genband.tcap.provider.TcapSession;

/**
 * Created by ankitsinghal on 15/11/16.
 */
public class InapCS1BCSMHelper {
    private static Logger logger = Logger.getLogger(InapCS1BCSMHelper.class);
    /**
     * This method is called by the Protocol handler whenever an ENC is
     * received.
     *
     * @param invokeIndEvent represents the instance of InvokeIndEvent
     * @param tcapSession    represents the instance of TcapSession
     * @return an array of Action Objects
     * @throws Exception
     */
    public static Action[] processErb(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) throws Exception {
        CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);

        if (logger.isDebugEnabled()) {
            logger.debug("[PH]:: Inside processErb");
        }
        try {
            InapCS1ScfProtocolParser.parseErbcsm(invokeIndEvent, callData);
        } catch (ASNParsingException ape) {
            logger.error("[PH]:: ASN pasring Exception in ERB.", ape);
            callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS1ScfRelReasonCode.ERB_ASN_PARSING_FAIL);
            return InapCS1ScfProtocolHelper.getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
        } catch (ParameterOutOfRangeException pore) {
            logger.error("[PH]:: ParameterOutOfRangeException in ERB.", pore);
            callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS1ScfRelReasonCode.ERB_PARAM_OUT_OF_RANGE);
            return InapCS1ScfProtocolHelper.getOutOfRangeParamterAction(tcapSession, ASNParsingException.MESSAGE.ERB);

        }
        if (logger.isDebugEnabled()) {
            logger.debug("[PH]:: Exiting processErb() .....");
        }
        return null;
    }

    /**
     * This method is for creating INAP component indication event for arming
     * the events.
     *
     * @param tcapSession represents the instance of TcapSession
     * @param action      represents the instance of Action
     * @throws Exception
     */
    public static void sendRRBCSMForArming(TcapSession tcapSession, Action action, LinkedList<BCSMEvent> bcsmList) throws Exception {
        CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);

        if (logger.isDebugEnabled()) {
            logger.debug("[PH]:: Inside sendRRBCSMForArming");
        }

        List<LinkedList<byte[]>> encodedArmingLists = InapCS1ERBSetHelper.createRRBCSMForArming(callData, action, bcsmList);

        for (LinkedList<byte[]> armList : encodedArmingLists) {
            byte[] rrbeOpCode = {InapOpCodes.RRBE_BYTE};
            Operation rrbeOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rrbeOpCode);
            InvokeReqEvent rrbeInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rrbeOperation);
            rrbeInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
            rrbeInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, armList.getFirst()));
            rrbeInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
            InapCS1ScfProtocolHelper.sendComponentReq(rrbeInvokeReqEvent, callData);
        }
        tcapSession.setAttribute(PhConstants.LEG1_TRIGGERS_ARMED, PhConstants.TRUE);
    }

    /**
     * This methos is for creating INAP component idication event for disarming
     * the events.
     *
     * @param tcapSession represents the instance of TcapSession
     * @param action      represents the instance of Action
     * @throws Exception
     */
    public static void sendRRBCSMForDisarming(TcapSession tcapSession, Action action) throws Exception {
        CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
        if (logger.isDebugEnabled()) {
            logger.debug("[PH]:: Inside sendRRBCSMForDisarming");
        }

        byte[] rrbe = InapCS1ScfProtocolParser.createRRBCSMForDisarming(callData, action);
        byte[] rrbeOpCode = {InapOpCodes.RRBE_BYTE};

        Operation rrbeOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rrbeOpCode);

        InvokeReqEvent rrbeInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rrbeOperation);
        rrbeInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
        rrbeInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, rrbe));
        rrbeInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

        InapCS1ScfProtocolHelper.sendComponentReq(rrbeInvokeReqEvent, callData);
        tcapSession.setAttribute(PhConstants.LEG1_TRIGGERS_ARMED, PhConstants.FALSE);
    }
}
