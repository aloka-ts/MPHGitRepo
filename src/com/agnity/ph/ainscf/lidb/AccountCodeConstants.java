/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.
 

Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
****/
package com.agnity.ph.ainscf.lidb;

/**
 * @author rarya
 *
 */
public interface AccountCodeConstants {

	public static int ACCOUNT_CODE_LENGTH = 22;
	
	// response
	public static int AC_SUCCESS_RESP_LENGTH = 6;
	public static byte AC_PARAMETER_SET_ID   = (byte) 0xF2;
	public static byte AC_PARAMETER_SET_LEN  = 0x04;
	public static byte AC_PARAMETER_TAG1     =  (byte) 0xDF;
	public static byte AC_PARAMETER_TAG2     = 0x46;
	public static byte AC_VALID_LEN          = 0x01;
	public static byte AC_VALID_FLAG         = 0x01;
	public static byte AC_INVALID_FLAG       = 0;

	public static String AC_FOUND            = "1";
	
}
