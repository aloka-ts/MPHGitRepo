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

import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.Enum.EnumProtocolHandler;
import com.agnity.ph.ainscf.AinScfProtocolHandler;
import com.agnity.ph.capv2scf.Capv2ScfProtocolHandler;
import com.agnity.ph.diameter.DiameterProtocolHandler;
import com.agnity.ph.diameter.sh.DiameterShProtocolHandler;
import com.agnity.ph.http.HttpProtocolHandler;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolHandler;
import com.agnity.ph.mapscf.MapScfProtocolHandler;
import com.agnity.ph.sip.SipProtocolHandler;
import com.agnity.ph.smpp.SmppProtocolHandler;
import com.agnity.ph.inapcs2scf.InapCS2ScfProtocolHandler;

/**
 * This class is the Factory class which exposes a static factory method which
 * returns Protocol handler for a specific protocol passed to it.
 *
 */
public class ProtocolHandlerFactory {

	private ProtocolHandlerFactory() {
		/*
		 * Don't create object of it
		 */
	}

	/**
	 * This method checks for license for requested protocol and return protocol handler.
	 * @param protocol 
	 * @return
	 */
	public static ProtocolHandler getProtocolHandler(Protocol protocol) {

		if (protocol == null) {
			return null;
		}
		ProtocolLicense lic = ProtocolLicense.getInstance();
		if (protocol == Protocol.SIP && lic.isSipSupported()) {
			return SipProtocolHandler.getInstance();
		}if (protocol == Protocol.HTTP && lic.isHttpSupported()) {
			return HttpProtocolHandler.getInstance();
		}  else if (protocol == Protocol.ITUINAPCS1_SCF && lic.isItuinapSupported()) {
			return InapCS1ScfProtocolHandler.getInstance();
		} else if (protocol == Protocol.AIN_SCF && lic.isAinSupported()) {
			return AinScfProtocolHandler.getInstance();
		} else if (protocol == Protocol.CAPV2_SCF && lic.isCapv2Supported()) {
			return Capv2ScfProtocolHandler.getInstance();
		} else if (protocol == Protocol.HTTP && lic.isHttpSupported()) {
			return HttpProtocolHandler.getInstance();
		} else if (protocol == Protocol.MAP_SCF && lic.isMapSupported()) {
			return MapScfProtocolHandler.getInstance();
		} else if (protocol == Protocol.ITUINAPCS2_SCF && lic.isItuinapSupported()) {
			return InapCS2ScfProtocolHandler.getInstance();
		}else if (protocol == Protocol.DIAMETER && lic.isDiameterSupported()) {
			return DiameterProtocolHandler.getInstance();
		}else if (protocol == Protocol.ENUM && lic.isEnumSupported()) {
			return EnumProtocolHandler.getInstance();
		}else if (protocol == Protocol.SMPP && lic.isSmppSupported()) {
			return SmppProtocolHandler.getInstance();
		}else if (protocol == Protocol.DIAMETER_SH && lic.isDiameterShSupported()) {
			return DiameterShProtocolHandler.getInstance();
		}
		
		return null;
	}
}
