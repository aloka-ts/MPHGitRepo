/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.
 

Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
****/
package com.agnity.ph.ainscf.gr533;

/**
 * @author rarya
 *
 */
public interface Gr533ProblemCodes {
	
	// problem code for Reject as per GR 533
	// for invoke
	public final int ProbCode_Invoke_Duplicate_InvokeId  = 1;
	public final int ProbCode_Invoke_Unrecognized_OpCode = 2;
	public final int ProbCode_Invoke_IncorrectParameter  = 3;
	public final int ProbCode_Invoke_Unrecognized_CorrId = 4;
	
	// for return result 
	public final int ProbCode_RRL_Unrecognized_CorrId = 1;
	public final int ProbCode_RRL_Unexpected_RRL      = 2;
	public final int ProbCode_RRL_IncorrectParameter  = 3;
	
	// for return error 
	public final int ProbCode_RERR_Unrecognized_CorrId = 1;
	public final int ProbCode_RERR_Unexpected_RERROR   = 2;
	public final int ProbCode_RERR_Unrecognized_Error  = 3;
	public final int ProbCode_RERR_Unexpected_Error    = 4;
	public final int ProbCode_RERR_IncorrectParameter  = 5;
	
	// problem Type 
	public final int ProbleType_General = 1;
	public final int ProbleType_Invoke  = 2;
	public final int ProbleType_RRL     = 3;
	public final int ProbleType_RERROR  = 4;
	public final int ProblemType_TransactionPortaion = 5;
	
}
