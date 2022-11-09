/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.
 

Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
****/
package com.agnity.ph.ainscf.cain.datatype;

/**
 * @author rarya
 *
 */
public interface CainConstants {
	
	// CAIN Extension header 
	byte [] cainExtnHeaderBytes= {0x31, 0x00};
	
	// ServTranslationSceme - Tag, Length
	byte [] servTranslationScheme = {(byte) 0x81, 0x00};
	
	// Network Busy Action 
	byte networkBusyActionTag = (byte) 0x93;
	
	// treatment code 
	byte treatmentTag = (byte) 0x8F;
	
	// Cain Billing Number Tag, len, NOA, NP
	byte [] billingNumEvenHead = {(byte) 0x9f, 0x2a, 0x00, 0x45, 0x10};
	byte [] billingNumOddHead  = {(byte) 0x9f, 0x2a, 0x00, (byte) 0xc5, 0x10};

}
