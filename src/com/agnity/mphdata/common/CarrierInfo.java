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
 * This class contains Carrier information received from AIN network. 
 * @author rarya
 *
 */
public class CarrierInfo implements Serializable, Cloneable {

	private static final long serialVersionUID = 2116364191829565175L;
	// Carrier Selection
	public static final int		CS_NO_INDICATION                		= 0;
	public static final int		CS_PRE_SUBSCRIBED_NOT_INPUT_BY_CLGPTY	= 1;
	public static final int		CS_PRE_SUBSCRIBED_INPUT_BY_CLGPTY		= 2;
	public static final int		CS_PRE_SUBSCRIBED_NO_IND				= 3;
	public static final int		CS_NOT_PRE_SUBSCRIBED  					= 4;

	//Nature of Carrier
	public static final int		NC_NO_NOC_PROVIDED				= 0;
	public static final int		NC_LOCAL						= 1;
	public static final int		NC_INTRA_LATA					= 2;
	public static final int		NC_INTER_LATA					= 3;
	public static final int		NC_LOCAL_INTER_AND_INTRA_LATA	= 4;
	public static final int		NC_LOCAL_INTRA_LATA				= 5;
	public static final int		NC_INTRA_AND_INTER_LATA			= 6;

	private String				address;
	private int					carrierSelection;
	private int 				natureOfCarrier;
	
	
	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public int getCarrierSelection() {
		return carrierSelection;
	}


	public void setCarrierSelection(int carrierSelection) {
		this.carrierSelection = carrierSelection;
	}


	public int getNatureOfCarrier() {
		return natureOfCarrier;
	}


	public void setNatureOfCarrier(int natureOfCarrier) {
		this.natureOfCarrier = natureOfCarrier;
	}


	@Override
	public CarrierInfo clone() {
		CarrierInfo cr = new CarrierInfo();
		cr.address = this.address;
		cr.carrierSelection = this.carrierSelection;
		cr.natureOfCarrier = this.natureOfCarrier;
		return cr;
	}
	
	@Override
	public String toString() {
		return "Carrier[address=" + address + 
				", CarrierSelection=" + carrierSelection + 
				", NatureOfCarrier=" + natureOfCarrier + "]";
	}

}
