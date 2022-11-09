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
package com.agnity.ph.common.measurement.enums;

import java.util.HashMap;
import java.util.Map;

/**
 * This enum includes all SS7 Messages.
 * @author rbehl
 *
 */

public enum SS7Message {
	
	PRIMITIVE_ERROR(1),
	PRIMITIVE_INVOKE(2),
	PRIMITIVE_LOCAL_CANCEL(3),
	PRIMITIVE_REJECT(5),
	PRIMITIVE_RESULT(7),
	PRIMITIVE_TIMER_RESET(9),
	PRIMITIVE_USER_CANCEL(10),
	PRIMITIVE_BEGIN(11),
	PRIMITIVE_CONTINUE(12),
	PRIMITIVE_END(13),
	PRIMITIVE_NOTICE(14),
	PRIMITIVE_PROVIDER_ABORT(15),
	PRIMITIVE_UNIDIRECTIONAL(16),
	PRIMITIVE_USER_ABORT(17),
	PRIMITIVE_END_PRE_ARRANGED(18);
	
	private static Map<Integer, SS7Message> messages = null;
	
	static {
		messages = new HashMap<Integer, SS7Message>();
		for(SS7Message ss7Message : SS7Message.values()) {
			messages.put(ss7Message.getCode(), ss7Message);
		}
	}
	private int code;
	private SS7Message(int code) {
		this.code = code;
	}
	
	public int getCode() {
		return code;
	}
	
	public static SS7Message valueOf(int code) {
		return messages.get(code);
	}
	
}

