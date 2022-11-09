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

/**
 * This class is used to check for licence availability for different protocols. currently license
 * for all the protocols is enabled . In future it can be configured using a property file 
 *
 */
public class ProtocolLicense {

	private static final ProtocolLicense INSTANCE = new ProtocolLicense();

	private boolean sipSupported;

	private boolean nttinapSupported;

	private boolean ituinapSupported;

	private boolean capv2Supported;

	private boolean ainSupported;

	private boolean httpSupported;
	
	private boolean mapSupported;

	private boolean diameterSupported;
	
	private boolean enumSupported;
	
	private boolean smppSupported;
	
	private boolean diameterShSupported;

	public boolean isEnumSupported() {
		return enumSupported;
	}

	public void setEnumSupported(boolean enumSupported) {
		this.enumSupported = enumSupported;
	}

	public void setMapSupported(boolean mapSupported) {
		this.mapSupported = mapSupported;
	}

	public boolean isAinSupported() {
		return ainSupported;
	}

	public void setAinSupported(boolean ainSupported) {
		this.ainSupported = ainSupported;
	}

	private ProtocolLicense() {
		// TODO:: Read the license file here and enable protocol accordingly
		sipSupported = true;
		ituinapSupported = true;
		capv2Supported = true;
		ainSupported = true;
		httpSupported = true;
		mapSupported = true;
		diameterSupported=true;
		enumSupported=true;
		smppSupported=true;
		diameterShSupported=true;
	}

	public static ProtocolLicense getInstance() {
		return INSTANCE;
	}

	public boolean isSipSupported() {
		return sipSupported;
	}

	public void setSipSupported(boolean sipSupported) {
		this.sipSupported = sipSupported;
	}

	public boolean isNttinapSupported() {
		return nttinapSupported;
	}

	public void setNttinapSupported(boolean nttinapSupported) {
		this.nttinapSupported = nttinapSupported;
	}

	public boolean isItuinapSupported() {
		return ituinapSupported;
	}

	public void setItuinapSupported(boolean ituinapSupported) {
		this.ituinapSupported = ituinapSupported;
	}

	public boolean isCapv2Supported() {
		return capv2Supported;
	}

	public void setCapv2Supported(boolean capv2Supported) {
		this.capv2Supported = capv2Supported;
	}

	public void setHttpSupported(boolean httpSupported) {
		this.httpSupported = httpSupported;
	}

	public boolean isHttpSupported() {
		return httpSupported;
	}

	public boolean isMapSupported() {
		return mapSupported;
	}
	
	public void setDiameterSupported(boolean isDiamSupported){
		this.diameterSupported=isDiamSupported;
	}

	public boolean isDiameterSupported() {
		// TODO Auto-generated method stub
		return diameterSupported;
	}
	
	public void setSmppSupported(boolean smppSupported) {
		this.smppSupported = smppSupported;
	}

	public boolean isSmppSupported() {
		// TODO Auto-generated method stub
		return smppSupported;
	}
	
	public boolean isDiameterShSupported() {
		return diameterShSupported;
	}

	public void setDiameterShSupported(boolean diameterShSupported) {
		this.diameterShSupported = diameterShSupported;
	}


}
