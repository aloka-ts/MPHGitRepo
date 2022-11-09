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
package com.agnity.ph.common.measurement;

import static com.agnity.ph.common.measurement.enums.SS7Message.PRIMITIVE_BEGIN;
import static com.agnity.ph.common.measurement.enums.SS7Message.PRIMITIVE_END;
import static com.agnity.ph.common.measurement.enums.SS7Message.PRIMITIVE_USER_ABORT;
import static com.agnity.ph.common.measurement.enums.SipMessage.REQ_INVITE;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Set;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.measurement.enums.MSConnectionMode;
import com.agnity.ph.common.measurement.enums.SS7Message;
/**
 * This class provides the utility methods for Measurement Counters.
 * @author rbehl
 *
 */
public class PhMeasurementUtills {

	private static final String COMMA = ",";
	private static Logger logger = Logger.getLogger(PhMeasurementUtills.class);		
	
	/**
	 * This method will prepare the formatted String for counters.
	 * @return String representation of Counters
	 */
	public static String formatCounterString() {
		
		StringBuilder sb = new StringBuilder();
		PhMeasurementService measurementService = PhMeasurementService.getInstance();
		SimpleDateFormat sd = new SimpleDateFormat("yyyyMMddhhmmss");
		sb.append(sd.format(new Date())).append(COMMA);
		sb.append(getSipTraffic(measurementService)).append(COMMA);
		sb.append(getSS7Traffic(measurementService, PRIMITIVE_BEGIN, true)).append(COMMA);
		sb.append(getSS7Traffic(measurementService, PRIMITIVE_BEGIN, false)).append(COMMA);
		sb.append(getSS7Traffic(measurementService, PRIMITIVE_END, true)).append(COMMA);
		sb.append(getSS7Traffic(measurementService, PRIMITIVE_END, false)).append(COMMA);
		sb.append(getSS7Traffic(measurementService, PRIMITIVE_USER_ABORT, true)).append(COMMA);
		sb.append(getSS7Traffic(measurementService, PRIMITIVE_USER_ABORT, false)).append(COMMA);
		sb.append(getSipMediaServerCount(measurementService)).append(COMMA);
		sb.append(getSS7MediaServerCount(measurementService)).append(COMMA);
		sb.append(getAssistMediaServerCount(measurementService)).append(COMMA);
		sb.append(getApplicationTraffic(measurementService));
		
		String callStatus = sb.toString();
		callStatus = callStatus.substring(0, callStatus.length()-1);
		if(logger.isDebugEnabled()) {
			logger.debug("formatted string : " + callStatus);
		}
		return callStatus;
	}
	
	private static long getSipTraffic(PhMeasurementService measurementService) {
		final MeasurementCounter counter = measurementService.getMeasurementCounter(Protocol.SIP);
		return counter != null ? counter.getCountForSipMessage(REQ_INVITE) : 0;
	}
	
	private static long getSS7Traffic(PhMeasurementService measurementService, SS7Message ss7Message, boolean isReceived) {
		long ss7Traffic = 0;
		
		final Set<Protocol> registeredProtocols = measurementService.getRegisteredProtocols();
		for(Protocol protocol : registeredProtocols) {
			if(protocol == Protocol.SIP) 
				continue;
			MeasurementCounter counter = measurementService.getMeasurementCounter(protocol);
			long count = counter != null ? counter.getCountForSS7Message(ss7Message, isReceived) : 0L;
			ss7Traffic = ss7Traffic + count;
		}
		return ss7Traffic;
	}
	
	private static String getApplicationTraffic(PhMeasurementService measurementService) {
		StringBuilder sb = new StringBuilder();
		
		Set<String> applicationsTriggered = measurementService.getMeasurementCounter(Protocol.SIP).getServicesTriggered();
		Set<Protocol> registeredProtocols = measurementService.getRegisteredProtocols();
		for(String appId : applicationsTriggered) {
			sb.append(appId).append(COMMA);
			for(Protocol protocol : registeredProtocols) {
				MeasurementCounter measurementCounter = measurementService.getMeasurementCounter(protocol);
				sb.append(measurementCounter.getCountForServiceTriggered(appId)).append(COMMA);
				sb.append(measurementCounter.getCountForServiceTriggeredByChaining(appId)).append(COMMA);
				
			}
		}
		
		return sb.toString();
	}
	
	private static long getSipMediaServerCount(PhMeasurementService measurementService) {
		final MeasurementCounter measurementCounter = measurementService.getMeasurementCounter(Protocol.SIP);
		return measurementCounter.getMediaServerCount(MSConnectionMode.SIP);
	}
	
	private static long getSS7MediaServerCount(PhMeasurementService measurementService) {
		long count = 0;
		Set<Protocol> protocols = measurementService.getRegisteredProtocols();
		for(Protocol protocol : protocols) {
			if(protocol == Protocol.SIP) {
				continue;
			}
			MeasurementCounter measurementCounter = measurementService.getMeasurementCounter(protocol);
			count = count + measurementCounter.getMediaServerCount(MSConnectionMode.SS7);
		}
		
		return count;
	}
	
	private static long getAssistMediaServerCount(PhMeasurementService measurementService) {
		long count = 0;
		Set<Protocol> protocols = measurementService.getRegisteredProtocols();
		for(Protocol protocol : protocols) {
			if(protocol == Protocol.SIP) {
				continue;
			}
			
			MeasurementCounter measurementCounter = measurementService.getMeasurementCounter(protocol);
			count = count + measurementCounter.getMediaServerCount(MSConnectionMode.ASSIT);
		}
		
		return count;
	}
}
