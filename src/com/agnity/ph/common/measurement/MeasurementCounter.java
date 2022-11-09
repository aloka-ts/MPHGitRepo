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
 * This class provides utility methods to manage several counters
 * for Multiple Protocols.
 * @author rbehl
 */
import java.util.Set;

import com.agnity.ph.common.measurement.enums.MSConnectionMode;
import com.agnity.ph.common.measurement.enums.SS7Message;
import com.agnity.ph.common.measurement.enums.SipMessage;

public interface MeasurementCounter {

	/**
	 * This method increment the counter by 1 for the application provided.
	 * @param serviceId for which counter to be incremented
	 * @param isChained, if true then service is triggered from Service Chaining.
	 * @return long
	 */
	long incrementServiceTriggeredCount(String serviceId, boolean isChained);

	/**
	 * This method increment the counter by offset for the application provided.
	 * @param serviceId for which counter to be incremented
	 * @param isChained, if true then service is triggered from Service Chaining.
	 * @param delta by which counter needs to be incremented.
	 * @return long
	 */
	long incrementServiceTriggeredCount(String serviceId, boolean isChained, long delta);

	/**
	 * This method provide the number of times a service is triggered.
	 * @param serviceId
	 * @return long
	 */
	long getCountForServiceTriggered(String serviceId);

	/**
	 * This method returns the number of times a service is triggered by chaining.
	 * @param serviceId
	 * @return long
	 */
	long getCountForServiceTriggeredByChaining(String serviceId);

	/**
	 * This method increments the respective message count by 1.
	 * @param message
	 * @param isReceived
	 * @return currentCount
	 */
	long incrementSS7MessageCount(SS7Message message, boolean isReceived);
	
	/**
	 * This method increments the respective SS7 message count by given delta.
	 * @param message
	 * @param isReceived
	 * @param delta
	 * @return currentCount
	 */
	long incrementSS7MessageCount(SS7Message message,  boolean isReceived, long delta);

	/**
	 * 
	 * @param message
	 * @param isReceived
	 * @return currentCount
	 */
	long getCountForSS7Message(SS7Message message, boolean isReceived);

	/**
	 * This method increments count of respective SIP message by 1.
	 * @param sipMessage
	 * @return long
	 */
	long incrementSipMessageCount(SipMessage sipMessage);

	/**
	 * This method increments count of respective SS7 message by delta.
	 * @param sipMessage
	 * @param delta
	 * @return long
	 */
	long incrementSipMessageCount(SipMessage sipMessage, long delta);

	/**
	 * Returns the current count of respective SIP message.
	 * @param sipMessage
	 * @return long
	 */
	long getCountForSipMessage(SipMessage sipMessage);
	
	/**
	 * This method will register the service for Measurement Counters.
	 * @param serviceId
	 */
	void registerService(String serviceId);
	
	/**
	 * This method will return set of all services triggered.
	 * @return {@link Set}
	 */
	Set<String> getServicesTriggered();
	
	/**
	 * This method will return set of all services triggered by service chaining.
	 * @return {@link Set}
	 */
	Set<String> getServicesTriggeredByChaining();
	
	/**
	 * increments the Media Server invocation for provided Connection mode.
	 * @param MSConnectionMode
	 * @return current count
	 */
	long incrementMediaServerCount(MSConnectionMode connectionMode);
	
	/**
	 * Return the current count of Media Server invocation for respective connection mode.
	 * @param connectionMode
	 * @return
	 */
	long getMediaServerCount(MSConnectionMode connectionMode);
	
	/**
	 * This method will unregister the service for Measurement Counters.
	 * @param serviceId
	 */
	void unRegisterService(String serviceId);

}