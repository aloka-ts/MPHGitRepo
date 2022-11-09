package com.agnity.ph.common.threadpool;

import javax.servlet.sip.SipApplicationSession;


public interface EventWork {

	/**
	 * Get the index of associated queue
	 * @return
	 */
	public int getQueueIndex() ;

	/**
	 * Check if work is valid
	 * @return
	 */
	public boolean isValid();

	/**
	 * Finish the specified work
	 * @throws Exception 
	 */
	public void finishIt() throws Exception ;

	public SipApplicationSession getAppSession();

} // end-of-class
