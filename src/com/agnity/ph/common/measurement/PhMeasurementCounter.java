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
/**
 * This class provides implementation for managing 
 * several counters for Multiple Protocols.
 * @author rbehl
 */
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.measurement.enums.MSConnectionMode;
import com.agnity.ph.common.measurement.enums.SS7Message;
import com.agnity.ph.common.measurement.enums.SipMessage;

public class PhMeasurementCounter implements MeasurementCounter {


	private Map<String, AtomicLong> serviceTriggeredCount;
	private Map<String, AtomicLong> serviceTriggeredByChainingCount;
	private Map<SipMessage, AtomicLong> sipMessageCounters;
	private Map<SS7Message, AtomicLong> ss7MessageCountersRx;
	private Map<SS7Message, AtomicLong> ss7MessageCountersTx;
	private Map<MSConnectionMode, AtomicLong> mediaServerCounters;
	private static Logger logger = Logger.getLogger(PhMeasurementCounter.class);
	
	public class SortingOrder implements Comparator<String> {
		public int compare(String s1, String s2){
			int value1 = Integer.parseInt(s1);
			int value2 = Integer.parseInt(s2);
			
			return value1 > value2 ? 1 : (value1 < value2 ? -1 : 0);
		}
	}
	
	public PhMeasurementCounter(Protocol protocol) {
		this.serviceTriggeredCount = new ConcurrentSkipListMap<String, AtomicLong>(new SortingOrder());
		this.serviceTriggeredByChainingCount = new ConcurrentSkipListMap<String, AtomicLong>(new SortingOrder());
		this.mediaServerCounters = Collections.synchronizedMap(
										new EnumMap<MSConnectionMode, AtomicLong>(MSConnectionMode.class));
		initializeMediaServerCount();
		if(protocol == Protocol.SIP) {
			sipMessageCounters = Collections.synchronizedMap(
										new EnumMap<SipMessage, AtomicLong>(SipMessage.class));
			initializeSipMessageCounter();
		}else {
			ss7MessageCountersRx = Collections.synchronizedMap(
					new EnumMap<SS7Message, AtomicLong>(SS7Message.class));
			ss7MessageCountersTx = Collections.synchronizedMap(
					new EnumMap<SS7Message, AtomicLong>(SS7Message.class));
			initializeSS7MessageCounters();
		}
	}
	
	@Override
	public long incrementServiceTriggeredCount(String serviceId, boolean isChained) {
		final AtomicLong counter = serviceTriggeredCount.get(serviceId);
		if(counter == null) {
			logger.error("ServiceId " + serviceId + " is not registered");
			return -1;
		}
		
		if(isChained) {
			return incrementServiceTriggeredByChaining(serviceId);
		}else {
			return counter.incrementAndGet();
		}
	}
	
	@Override
	public long incrementServiceTriggeredCount(String serviceId, boolean isChained, long delta) {
		final AtomicLong counter = serviceTriggeredCount.get(serviceId);
		if(counter == null) {
			logger.error("ServiceId " + serviceId + " is not registered");
			return -1;
		}
		
		if(isChained) {
			return incrementServiceTriggeredByChainingCount(serviceId, delta);
		}else {
			return counter.addAndGet(delta);
		}
	}
	
	
	private long incrementServiceTriggeredByChaining(String serviceId) {
		final AtomicLong counter = serviceTriggeredByChainingCount.get(serviceId);

		return counter.incrementAndGet();
	}
	
	private long incrementServiceTriggeredByChainingCount(String serviceId, long delta) {
		final AtomicLong counter = serviceTriggeredByChainingCount.get(serviceId);

		return counter.addAndGet(delta);
	}
	
	@Override
	public long getCountForServiceTriggered(String serviceId) {
		final AtomicLong count = serviceTriggeredCount.get(serviceId);
		return	count != null ? count.get() : 0; 
	}
	
	@Override
	public long getCountForServiceTriggeredByChaining(String serviceId) {
		final AtomicLong count = serviceTriggeredByChainingCount.get(serviceId);
		return count != null ? count.get() : 0;
	}
	
	@Override
	public long incrementSS7MessageCount(SS7Message message, boolean isReceived) {
		AtomicLong count = null;
		if(isReceived) {
			count = ss7MessageCountersRx.get(message);
		}else {
			count = ss7MessageCountersTx.get(message);
		}
		return count.incrementAndGet();
	}
	
	@Override
	public long incrementSS7MessageCount(SS7Message message, boolean isReceived, long delta) {
		AtomicLong count = null;
		if(isReceived) {
			count = ss7MessageCountersRx.get(message);
		}else {
			count = ss7MessageCountersTx.get(message);
		}
		return count.addAndGet(delta);
	}
	
	private void initializeSS7MessageCounters() {
		
		for(SS7Message ssMessage : SS7Message.values()) {
			ss7MessageCountersRx.put(ssMessage, new AtomicLong(0));
			ss7MessageCountersTx.put(ssMessage, new AtomicLong(0));
		}
	}
	
	private void initializeSipMessageCounter() {
		
		for(SipMessage sipMessage : SipMessage.values()) {
			sipMessageCounters.put(sipMessage, new AtomicLong(0));
		}
	}
	
	@Override
	public long getCountForSS7Message(SS7Message message, boolean isReceived) {
		if(isReceived) 
			return ss7MessageCountersRx.get(message).get();
		else
			return ss7MessageCountersTx.get(message).get();
	}
	
	
	@Override
	public long incrementSipMessageCount(SipMessage sipMessage) {
		final AtomicLong counter = sipMessageCounters.get(sipMessage);
		return counter.incrementAndGet();
	}
	
	@Override
	public long incrementSipMessageCount(SipMessage sipMessage, long delta) {
		final AtomicLong counter = sipMessageCounters.get(sipMessage);
		return counter.addAndGet(delta);
	}
	
	@Override
	public long getCountForSipMessage(SipMessage sipMessage) {
		return sipMessageCounters.get(sipMessage).get();
	}

	@Override
	public void registerService(String serviceId) {
		serviceTriggeredCount.put(serviceId, new AtomicLong(0));
		serviceTriggeredByChainingCount.put(serviceId, new AtomicLong(0));
	}
	
	@Override
	public Set<String> getServicesTriggered() {
		return serviceTriggeredCount.keySet();
	}
	
	@Override
	public Set<String> getServicesTriggeredByChaining() {
		return serviceTriggeredByChainingCount.keySet();
	}

	@Override
	public long incrementMediaServerCount(MSConnectionMode connectionMode) {
		final AtomicLong count = mediaServerCounters.get(connectionMode);
		return count.incrementAndGet();
	}

	private void initializeMediaServerCount() {
		for(MSConnectionMode connectionMode : MSConnectionMode.values()) {
			mediaServerCounters.put(connectionMode, new AtomicLong());
		}
	}
	
	@Override
	public long getMediaServerCount(MSConnectionMode connectionMode) {
		return mediaServerCounters.get(connectionMode).get();
	}
	
	@Override
	public void unRegisterService(String serviceId) {
		serviceTriggeredCount.remove(serviceId);
		serviceTriggeredByChainingCount.remove(serviceId);
	}
}
