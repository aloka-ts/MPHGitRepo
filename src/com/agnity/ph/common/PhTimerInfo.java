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
package com.agnity.ph.common;

import java.io.Serializable;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Protocol;

/**
 * This class is used to pass timer information while creating a timer .The
 * protocol name is passed to it .so that when timer is timed-out the timeout
 * even can be handover to specific protocol handler 
 *
 */
public class PhTimerInfo implements Serializable {

	private static final long serialVersionUID = 1L;
	private static Logger logger = Logger.getLogger(PhTimerInfo.class);

	String timerName;
	Protocol protocol;
	Object data;

	public Object getData() {
		return data;
	}

	public void setTimerName(String timerName) {
		this.timerName = timerName;
	}

	public void setProtocol(Protocol protocol) {
		this.protocol = protocol;
	}

	public String getTimerName() {
		return timerName;
	}

	public Protocol getProtocol() {
		return protocol;
	}

	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append(" Timer Name :" + timerName + " Protocol : " + protocol);
		return sb.toString();
	}

	public void setData(Object timerData) {
		this.data=timerData;
		
	}
}
