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
package com.agnity.mphdata.common;

import java.io.Serializable;

/**
 * This is a generic class that is used as a placeholder to store the IE received/ to be sent from
 * network . For E,g this is used to store Called party number , calling party number , redirecting 
 * number , Generic number etc.
 *
 */
public class PhoneNumber implements Serializable, Cloneable {
	private static final long	serialVersionUID	= -7123530386775686406L;
	/*
	 * Nature of Addresses(NOA) for Billing Number 
	 * NOA-Nature of Address
	 * NAT-National 
	 * INT-International 
	 * NPR-No presentation restriction
	 * PR-presentation Restriction
	 */
	public static final int BNS_NOA_NAT_NPR = 0;
	public static final int BNS_NOA_INT_NPR = 1;
	public static final int BNS_NOA_NAT_PR = 2;
	public static final int BNS_NOA_INT_PR = 3;
	//Nature of Addresses
	public static final int		NOA_UNKNOWN			  = 2;
	public static final int		NOA_NATIONAL		  = 3;
	public static final int		NOA_INTERNATIONAL	  = 4;
	public static final int     NOA_NATIONAL_OPERATOR = 114;

	//Number Plans
	public static final int		NP_UNKNOWN			= 6;
	public static final int		NP_PRIVATE			= 5;
	public static final int		NP_ISDN				= 1;

	private String				address;
	private int					natureOfAddress;								//2- UNKNOWN,3- NATIONAL,4- INTERNATIONAL
	private int					numberingPlan;									//Added for sbtm:6: UNKNOWN,5: PRIVATE,1: ISDN

	//OLEC for caller
	private int					localExCarrier;								//Added for sbtm
	private int					presentationIndicator;							//Added for sbtm:Presentation Indicator
	private int					numberOverriding	= -1;

	private int addressPresentationRestrictedIndicator;   //Presentation allowed : 0
	private int screeningIndicator = 3;  //Network Provided
	private int numberIncompleteIndicator; //Number complete
	
	private int natureOfAddressActual;

	public int getNatureOfAddressActual() {
		return natureOfAddressActual;
	}

	public void setNatureOfAddressActual(int natureOfAddressActual) {
		this.natureOfAddressActual = natureOfAddressActual;
	}

	public static final String NOA_PARAM_KEY = "noa";
	public static final String NPI_PARAM_KEY = "npi";

	public int getLocalExCarrier() {
		return localExCarrier;
	}

	public void setLocalExCarrier(int localExCarrier) {
		this.localExCarrier = localExCarrier;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String number) {
		this.address = number;
	}

	public int getNatureOfAddress() {
		return natureOfAddress;
	}

	public void setNatureOfAddress(int natureOfAddress) {
		this.natureOfAddress = natureOfAddress;
	}

	public int getNumberingPlan() {
		return numberingPlan;
	}

	public void setNumberingPlan(int numberingPlan) {
		this.numberingPlan = numberingPlan;
	}

	public int getAddressPresentationRestrictedIndicator() {
		return addressPresentationRestrictedIndicator;
	}

	public void setAddressPresentationRestrictedIndicator(int addressPresentationRestrictedIndicator) {
		this.addressPresentationRestrictedIndicator = addressPresentationRestrictedIndicator;
	}

	public int getScreeningIndicator() {
		return screeningIndicator;
	}

	public void setScreeningIndicator(int screeningIndicator) {
		this.screeningIndicator = screeningIndicator;
	}

	public int getNumberIncompleteIndicator() {
		return numberIncompleteIndicator;
	}

	public void setNumberIncompleteIndicator(int numberIncompleteIndicator) {
		this.numberIncompleteIndicator = numberIncompleteIndicator;
	}

	@Override
	public PhoneNumber clone() {
		PhoneNumber pn = new PhoneNumber();
		pn.address = this.address == null ? null : new String(this.address);
		pn.localExCarrier = this.localExCarrier;
		pn.natureOfAddress = this.natureOfAddress;
		pn.numberingPlan = this.numberingPlan;
		pn.presentationIndicator = this.presentationIndicator;
		return pn;
	}

	public int getPresentationIndicator() {
		return presentationIndicator;
	}

	public void setPresentationIndicator(int presentationIndicator) {
		this.presentationIndicator = presentationIndicator;
	}

	public int getNumberOverriding() {
		return numberOverriding;
	}

	public void setNumberOverriding(int value) {
		numberOverriding = value;
	}

	public static int getIntegerForPresentationIndicator(String presentationIndicator) {
		if (null == presentationIndicator || presentationIndicator.isEmpty()) {
			return NP_UNKNOWN;
		} else if (presentationIndicator.equalsIgnoreCase("isdn")) {
			return NP_ISDN;
		} else if (presentationIndicator.equalsIgnoreCase("private")) {
			return NP_PRIVATE;
		}
		return NP_UNKNOWN;
	}

	@Override
	public String toString() {
		return "PhoneNumber[" +
				"address='" + address + '\'' +
				", natureOfAddress=" + natureOfAddress +
				", numberingPlan=" + numberingPlan +
				", localExCarrier=" + localExCarrier +
				", presentationIndicator=" + presentationIndicator +
				", numberOverriding=" + numberOverriding +
				", addressPresentationRestrictedIndicator=" + addressPresentationRestrictedIndicator +
				", screeningIndicator=" + screeningIndicator +
				", numberIncompleteIndicator=" + numberIncompleteIndicator +
				']';
	}
}
