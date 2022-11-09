package com.agnity.ph.inapcs2scf.messagehelper;

import com.agnity.inapitutcs2.asngenerated.*;
import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.inapitutcs2.operations.InapOperationsCoding;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.SS7ErbModeAndLegInfo;
import com.agnity.ph.sip.SipProtocolConfig;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil;
import com.agnity.ph.inapcs1scf.messagehelper.InapCS1ERBSetHelper;
import com.agnity.ph.inapcs2scf.InapCS2ScfProtocolUtil;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.log4j.Logger;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.leg1Type;
import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.leg2Type;
import static com.agnity.ph.sip.SipProtocolConfig.MAX_BCSM_EVENTS;

/**
 * Created by ankitsinghal on 03/10/16.
 */
public class InapCS2ERBSetHelper {

	private static Logger logger = Logger.getLogger(InapCS2ERBSetHelper.class);

	public static List<LinkedList<byte[]>> createRRBCSMForArming(CallData callData, Action action, LinkedList<BCSMEvent> input) throws Exception {
		LinkedList<BCSMEvent> bcsmEventList = null;
		
		if(logger.isDebugEnabled()){
			logger.debug("Inside createRRBCSMForArming");
		}
		
		if(input != null){
			bcsmEventList = input;
		}else{	
			bcsmEventList = getConfiguredRRBSCMList(callData, action);
		}

		Integer maxConfiguredBCSMEventSize = Integer.valueOf(SipProtocolConfig.getConfigData(MAX_BCSM_EVENTS));
		logger.debug("[PH]:: Max-Configured BCSM Event Size = " + maxConfiguredBCSMEventSize);

		//There can be cases that there are more than configured arm events, so get the sublists, and arm with 4 events in row
		List<LinkedList<byte[]>> encodedOperationsList = new LinkedList<LinkedList<byte[]>>();
		List<List<BCSMEvent>> sublists = ListUtils.partition(bcsmEventList, maxConfiguredBCSMEventSize);

		for (List<BCSMEvent> bcsmEventSubList : sublists) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Encoding BCSM Components List: " + bcsmEventSubList);
			}
			RequestReportBCSMEventArg rrbcsmEventArg = new RequestReportBCSMEventArg();
			rrbcsmEventArg.setBcsmEvents(bcsmEventSubList);

			LinkedList<Object> operationObjs = new LinkedList<Object>();
			LinkedList<String> opCode = new LinkedList<String>();
			operationObjs.add(rrbcsmEventArg);
			opCode.add(InapOpCodes.RRBE);
			LinkedList<byte[]> encodeSubList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
			encodedOperationsList.add(encodeSubList);
		}
		return encodedOperationsList;
	}


	private static LinkedList<BCSMEvent> getConfiguredRRBSCMList(CallData callData, Action action) throws Exception {
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(action.getLeg()));
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createRRBCSMForArming");
		}

		// In case application doesn't set NoAnswer timer in that case we need to set 
		// monitor mode for oNoAnswer and CalledPartyBusy as notifyAndContinue
		int monitorMode = 1; 
		Integer noAnswerTimeObj = (Integer) legData.get(LegDataAttributes.NP_NO_ANSWER_TIMER_DURATION);

		Set<SS7ErbModeAndLegInfo> erbTypeSetObj = InapCS2ScfProtocolUtil.getErbSetByApplicationWithMode(callData, action.getLeg());
		if (CollectionUtils.isEmpty(erbTypeSetObj)) {
			throw new Exception("[PH]:: No ERB set provided to createRRBCSMForArming");
		}

		LinkedList<BCSMEvent> bcsmEventList = new LinkedList<BCSMEvent>();
		for (SS7ErbModeAndLegInfo erbTypeObj : erbTypeSetObj) {
			switch (erbTypeObj.getErbType()) {
			case ERB_ROUTESELECTFAILURE: {
				BCSMEvent bcsmEvent = InapCS1ERBSetHelper.getBCSMEvent(
						EventTypeBCSM.EnumType.routeSelectFailure,
						(erbTypeObj.getSendMode() == 1)?MonitorMode.EnumType.notifyAndContinue:MonitorMode.EnumType.interrupted,
								erbTypeObj.getLegType());
				bcsmEventList.add(bcsmEvent);
			}
			break;
			case ERB_BUSY: {
				BCSMEvent bcsmEvent = InapCS1ERBSetHelper.getBCSMEvent(
						EventTypeBCSM.EnumType.oCalledPartyBusy,
						(erbTypeObj.getSendMode() == 1)?MonitorMode.EnumType.notifyAndContinue:MonitorMode.EnumType.interrupted,
								erbTypeObj.getLegType());
				bcsmEventList.add(bcsmEvent);
			}
			break;
			case ERB_NO_ANSWER: {
				// if timer is set then it should be interrupt mode
				if(noAnswerTimeObj != null){
					monitorMode = 2;
				}else{
					monitorMode = 1;
				}
				BCSMEvent bcsmEvent = InapCS1ERBSetHelper.getBCSMEvent(
						EventTypeBCSM.EnumType.oNoAnswer,
						(erbTypeObj.getSendMode() == 1)?MonitorMode.EnumType.notifyAndContinue:MonitorMode.EnumType.interrupted,
								erbTypeObj.getLegType());

				InapCS2ERBSetHelper.updateBCSMEventForNoAnswer(bcsmEvent, noAnswerTimeObj);
				bcsmEventList.add(bcsmEvent);
			}
			break;
			case ERB_ANSWER: {
				BCSMEvent bcsmEvent = InapCS1ERBSetHelper.getBCSMEvent(
						EventTypeBCSM.EnumType.oAnswer,
						(erbTypeObj.getSendMode() == 1)?MonitorMode.EnumType.notifyAndContinue:MonitorMode.EnumType.interrupted,
								erbTypeObj.getLegType()) ;
				bcsmEventList.add(bcsmEvent);
			}
			break;
			case ERB_DISCONNECT: {

				if(erbTypeObj.getLegType() == 3){
					BCSMEvent bcsmEvent1 = InapCS1ERBSetHelper.getBCSMEvent(
							EventTypeBCSM.EnumType.oDisconnect,
							(erbTypeObj.getSendMode() == 1)?MonitorMode.EnumType.notifyAndContinue:MonitorMode.EnumType.interrupted,
							1);
					bcsmEventList.add(bcsmEvent1);
					BCSMEvent bcsmEvent2 = InapCS1ERBSetHelper.getBCSMEvent(
							EventTypeBCSM.EnumType.oDisconnect,
							(erbTypeObj.getSendMode() == 1)?MonitorMode.EnumType.notifyAndContinue:MonitorMode.EnumType.interrupted,
							2);
					bcsmEventList.add(bcsmEvent2);
				}else{
					BCSMEvent bcsmEvent1 = InapCS1ERBSetHelper.getBCSMEvent(
							EventTypeBCSM.EnumType.oDisconnect,
							(erbTypeObj.getSendMode() == 1)?MonitorMode.EnumType.notifyAndContinue:MonitorMode.EnumType.interrupted,
									erbTypeObj.getLegType());
					bcsmEventList.add(bcsmEvent1);
				}
			}
			break;
			case ERB_ABANDON: {
				BCSMEvent bcsmEvent = InapCS1ERBSetHelper.getBCSMEvent(
						EventTypeBCSM.EnumType.oAbandon,
						(erbTypeObj.getSendMode() == 1)?MonitorMode.EnumType.notifyAndContinue:MonitorMode.EnumType.interrupted,
								erbTypeObj.getLegType());
				bcsmEventList.add(bcsmEvent);
			}
			break;
			case ERB_TERMSEIZED: {
				BCSMEvent bcsmEvent = InapCS1ERBSetHelper.getBCSMEvent(
						EventTypeBCSM.EnumType.oTermSeized,
						(erbTypeObj.getSendMode() == 1)?MonitorMode.EnumType.notifyAndContinue:MonitorMode.EnumType.interrupted,
						erbTypeObj.getLegType());
				bcsmEventList.add(bcsmEvent);
			}
			break;
			}
		}
		return bcsmEventList;
	}

	private static BCSMEvent getBCSMEvent(EventTypeBCSM.EnumType enumType, MonitorMode.EnumType monitorModeType,int legType) {
		return getEncodedBCSMEvent(enumType, monitorModeType, legType);
	}

	private static BCSMEvent getBCSMEvent(EventTypeBCSM.EnumType enumType, MonitorMode.EnumType monitorModeType, LegType legType) {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("[PH]:: Creating BCSM Event for Type: [%s], MonitorMode: [%s], LegType: [%s]",
					enumType,
					monitorModeType,
					legType));
		}

		MonitorMode monitorMode1 = new MonitorMode();
		monitorMode1.setValue(monitorModeType);

		EventTypeBCSM eventTypeBCSM = new EventTypeBCSM();
		eventTypeBCSM.setValue(enumType);

		BCSMEvent bcsmEvent = new BCSMEvent();
		bcsmEvent.setEventTypeBCSM(eventTypeBCSM);
		bcsmEvent.setMonitorMode(monitorMode1);

		// Leg ID has to be added only for oDisconnect
		if(enumType ==  EventTypeBCSM.EnumType.oDisconnect) {
			LegID legID = new LegID();
			legID.selectSendingSideID(legType);
			bcsmEvent.setLegID(legID);
		}

		return bcsmEvent;
	}
	
	private static void updateBCSMEventForNoAnswer(BCSMEvent bcsmEvent,
			Integer noAnswerTimer) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: noAnswerTimer from service [seconds] = " + noAnswerTimer);
		}
		if (null != noAnswerTimer) {
			if(noAnswerTimer > 175){
				noAnswerTimer = noAnswerTimer / 1000;
			}

			noAnswerTimer = InapCS1ScfProtocolUtil.getNumberInRange(3, 175, noAnswerTimer);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Setting no answer timer as: " + noAnswerTimer);
			}

			DpSpecificCriteria noAnswerCriteria = new DpSpecificCriteria();
			noAnswerCriteria.selectApplicationTimer(new ApplicationTimer(noAnswerTimer));
			bcsmEvent.setDpSpecificCriteria(noAnswerCriteria);
		}
	}
	
	private static BCSMEvent getEncodedBCSMEvent(EventTypeBCSM.EnumType enumType, 
			MonitorMode.EnumType monitorModeType, int legType) {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("[PH]:: Creating BCSM Event for Type: [%s], MonitorMode: [%s], LegType: [%d]",
					enumType,
					monitorModeType,
					legType));
		}

		MonitorMode monitorMode1 = new MonitorMode();
		monitorMode1.setValue(monitorModeType);

		EventTypeBCSM eventTypeBCSM = new EventTypeBCSM();
		eventTypeBCSM.setValue(enumType);

		BCSMEvent bcsmEvent = new BCSMEvent();
		bcsmEvent.setEventTypeBCSM(eventTypeBCSM);
		bcsmEvent.setMonitorMode(monitorMode1);

		if(legType == 1){
			LegID legID = new LegID();
			legID.selectSendingSideID(leg1Type);
			bcsmEvent.setLegID(legID);
		}else if(legType == 2){
			LegID legID = new LegID();
			legID.selectSendingSideID(leg2Type);
			bcsmEvent.setLegID(legID);
		}

		//		// Leg ID has to be added only for oDisconnect
		//		if(enumType ==  EventTypeBCSM.EnumType.oDisconnect) {
		//			LegID legID = new LegID();
		//			legID.selectSendingSideID(legType);
		//			bcsmEvent.setLegID(legID);
		//		}

		return bcsmEvent;
	}
}
