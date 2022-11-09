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
 * This calss provide wrapper to Trunk group information excahnged in AIN. 
 * This covers PrimaryTrunk group, Alternate Trunk Group, SecondAlternateTrunkGroup
 * @author rarya
 *
 */
public class TrunkGroupInfo implements Serializable, Cloneable {

	private static final long serialVersionUID = 2569442030269716832L;

	// Options for Number to Outpulse
	public static final int C_NUM_TO_OUTPULSE  = 0;
	public static final int C_NORMAL_ROUTING_NUM = 1;

	// Simulated Facility Group Indicator 
	public static final int C_NOT_SFG = 0;
	public static final int C_SFG = 1;

	// Call Treatment Indicator 
	public static final int C_TI_NOT_USED                       = 0;
	public static final int C_TI_NO_OVERFLOW_NO_RETURN          = 1;
	public static final int C_TI_OVERFLOW                       = 2;
	public static final int C_TI_OFFHOOK_NO_OVERFLOW_NO_RETURN  = 3;
	public static final int C_TI_OFFHOOK_OVERFLOW               = 4;
	public static final int C_TI_RINGBACK_NO_OVERFLOW_NO_RETURN = 5;
	public static final int C_TI_RINGBACK_OVERFLOW              = 6;
	public static final int C_TI_RETURN                         = 7;
	public static final int C_TI_OFFHOOK_RETURN                 = 8;
	public static final int C_TI_RINGBACK_RETURN                = 9;


	private int			numToOutpulse;
	private int 		sfg;
	private int			callTreatmentInd;
	private String		trunkGroupAddress;



	@Override
	public TrunkGroupInfo clone() {
		TrunkGroupInfo cr    = new TrunkGroupInfo();
		cr.numToOutpulse     = this.numToOutpulse;
		cr.callTreatmentInd  = this.callTreatmentInd;
		cr.trunkGroupAddress = this.trunkGroupAddress;
		cr.sfg = this.sfg;
		return cr;
	}

	public int getNumToOutpulse() {
		return numToOutpulse;
	}

	public void setNumToOutpulse(int numToOutpulse) {
		this.numToOutpulse = numToOutpulse;
	}

	public int getSfg() {
		return sfg;
	}

	public void setSfg(int sfg) {
		this.sfg = sfg;
	}

	public int getCallTreatmentInd() {
		return callTreatmentInd;
	}

	public void setCallTreatmentInd(int callTreatmentInd) {
		this.callTreatmentInd = callTreatmentInd;
	}

	public String getTrunkGroupAddress() {
		return trunkGroupAddress;
	}

	public void setTrunkGroupAddress(String trunkGroupAddress) {
		this.trunkGroupAddress = trunkGroupAddress;
	}

	@Override
	public String toString() {
		return "TrunkGroupInfo[numToOutpulse=" + numToOutpulse + 
				", sfg =" + sfg +
				", callTreatmentInd=" + callTreatmentInd + 
				", trunkGroupAddress=" + trunkGroupAddress+ "]";
	}
}