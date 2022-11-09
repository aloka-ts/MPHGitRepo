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
 * This class provide wrapper to Billing Indicator being used in AIN.
 * This class shall cover PrimaryBillingIndicator, SecondAlternateBillingIndicator
 * AlternateBillingIndicator, OverflowBillingIndicator
 * @author rarya
 *
 */
public class BillingIndicatorInfo implements Serializable, Cloneable {

	private static final long serialVersionUID = 3201341480689543495L;

	private String			callType;
	private String			serviceFeatureIdentifiation;
	
	public String getCallType() {
		return callType;
	}

	public void setCallType(String callType) {
		this.callType = callType;
	}

	public String getServiceFeatureIdentifiation() {
		return serviceFeatureIdentifiation;
	}

	public void setServiceFeatureIdentifiation(String serviceFeatureIdentifiation) {
		this.serviceFeatureIdentifiation = serviceFeatureIdentifiation;
	}

	@Override
	public BillingIndicatorInfo clone() {
		BillingIndicatorInfo cr = new BillingIndicatorInfo();
		cr.callType = this.callType;
		cr.serviceFeatureIdentifiation = this.serviceFeatureIdentifiation;
		return cr;
	}
	
	@Override
	public String toString() {
		return "BillingIndicatorInfo[callType=" + callType + 
				", serviceFeatureIdentifiation=" + serviceFeatureIdentifiation + "]";
	}

}
