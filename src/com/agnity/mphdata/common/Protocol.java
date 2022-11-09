/*******************************************************************************
 * Copyright (c) 2011 Agnity, Inc. All rights reserved.
 * <p>
 * This is proprietary source code of Agnity, Inc.
 * <p>
 * Agnity, Inc. retains all intellectual property rights associated
 * with this source code. Use is subject to license terms.
 * <p>
 * This source code contains trade secrets owned by Agnity, Inc.
 * Confidentiality of this computer program must be maintained at
 * all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.mphdata.common;

import java.io.Serializable;

/**
 * This class defines various network protocols.
 *
 */
public enum Protocol implements Serializable {
	 SIP,
	    NTTINAP_SCF,
	    ITUINAP_SCF,
	    ITUINAPCS1_SCF,
	    SINAP_SCF,
	    CAPV2_SCF,
	    CAPV3_SCF,
	    NTTINAP_SSF,
	    ITUINAPCS1_SSF,
	    ITUINAP_SSF,
	    SINAP_SSF,
	    CAPV2_SSF,
	    CAPV3_SSF,
	    AIN_SCF,
	    HTTP,
	    MAP_SCF,
	    ITUINAPCS2_SCF, 
	    DIAMETER,
	    ENUM,
	    TRIGGER_CRITERIA, SMPP,
	    DIAMETER_SH,
	    GDI
}
