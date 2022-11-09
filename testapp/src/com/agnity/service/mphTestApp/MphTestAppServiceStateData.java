package com.agnity.service.mphTestApp;

import java.io.Serializable;

public class MphTestAppServiceStateData implements Serializable {
	
	boolean isRBTTimerStarted=false;
	
	public boolean isRBTTimerStarted() {
		return isRBTTimerStarted;
	}

	public void setRBTTimerStarted(boolean isRBTTimerStarted) {
		this.isRBTTimerStarted = isRBTTimerStarted;
	}

	public boolean isIsorigconnectedToIvr() {
		return isorigconnectedToIvr;
	}

	public void setIsorigconnectedToIvr(boolean isorigconnectedToIvr) {
		this.isorigconnectedToIvr = isorigconnectedToIvr;
	}

	boolean isorigconnectedToIvr=false;

}
