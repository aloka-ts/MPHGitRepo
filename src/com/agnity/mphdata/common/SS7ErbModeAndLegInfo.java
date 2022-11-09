/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.


Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.mphdata.common;

import java.io.Serializable;

/**
 * This class is used to contain ERB fields which defines how ERB need to be encoded
 * like monitor mode and leg Type. This can be defined through mph.properties 
 * as below for key INAP_ERB_MODE_AND_LEG_INFO
 * "ERB_ROUTESELECTFAILURE:0:Leg2|ERB_BUSY:0:Leg2|ERB_NO_ANSWER:0:null|ERB_ANSWER:1:Leg2|
 * ERB_DISCONNECT:0:Both|ERB_ABANDON:0:Leg1"
 * 
 * Each Protocol using it would need to parse and use it while encoding RRBCSM
 * @author rarya
 *
 */
public class SS7ErbModeAndLegInfo implements Serializable {

	private int legType = -1;
	private int sendMode = -1; 
	private Action.ERB_TYPE erbType = null;

	/**
	 * @param erbType
	 * @param mode
	 * @param legType - 0 (Not to be set) or 1 or 2 or 3 (both)
	 */
	public SS7ErbModeAndLegInfo(Action.ERB_TYPE erbType, int mode, int legType){
		this.erbType = erbType;
		this.sendMode = mode;
		this.legType = legType;
	}

	public int getLegType() {
		return legType;
	}

	public void setLegType(int legType) {
		this.legType = legType;
	}

	public int getSendMode() {
		return sendMode;
	}

	public void setSendMode(int sendMode) {
		this.sendMode = sendMode;
	}

	public Action.ERB_TYPE getErbType() {
		return erbType;
	}

	public void setErbType(Action.ERB_TYPE erbType) {
		this.erbType = erbType;
	}
	
	@Override
	public String toString() {
		return "SS7ErbModeAndLegInfo [erbType=" + erbType + ", legType=" + legType + ", sendMode="
				+ sendMode + "]";
	}
}
