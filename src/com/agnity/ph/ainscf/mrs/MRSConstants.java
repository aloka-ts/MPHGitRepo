/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.


Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.ph.ainscf.mrs;

/**
 * @author manish.kumar
 *
 */
public class MRSConstants {
	
	//TCAP Information for Initial Query Message
	public static byte IQM_PARAMETER_SET_ID = (byte) 0xF2;
	public static byte IQM_PARAMETER_SET_ID_LEN = (byte) 0x18;
	
	//service key identifier
	public static byte IQM_SERVICE_KEY_ID = (byte) 0xAA;
	public static byte IQM_SERVICE_KEY_ID_LEN = (byte) 0x16;
	
	public static byte IQM_DEST_NUM_DIGITS_ID = (byte) 0x84;
	public static byte IQM_DEST_NUM_DIGITS_ID_LEN = (byte) 0x09;
	
	public static byte IQM_DEST_NUM_TYPE_OF_DIGITS = (byte)0x06;
	public static byte IQM_DEST_NUM_NATURE_OF_NUM_NP = (byte)0x00;//zero for no presentation restriction
	public static byte IQM_DEST_NUM_NATURE_OF_NUM_P = (byte)0x01;//one for presentation restriction
	public static byte IQM_DEST_NUM_NUMBERING_PLAN =(byte)0x21;
	public static byte IQM_DEST_NUM_OF_DIGITS =(byte)0x0A;
	
	public static byte ISVM_DEST_NUM_NUMBERING_PLAN = (byte)0x11;

}
