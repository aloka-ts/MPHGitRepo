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
package com.agnity.ph.common.exception;

public class ParameterOutOfRangeException extends Exception {

	/**
	 * 
	 */
	private static final long	serialVersionUID	= -5660714504363006358L;
	String errorString;
	/**
	 * @param errorString
	 */
	public ParameterOutOfRangeException(String errorString) {
		super();
		this.errorString = errorString;
	}
	/**
	 * 
	 */
	public ParameterOutOfRangeException() {
		super();
		// TODO Auto-generated constructor stub
	}
	

}