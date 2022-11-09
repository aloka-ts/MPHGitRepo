/**
 * 
 */
package com.agnity.ph.common;

import java.io.Serializable;

import javax.servlet.sip.ServletTimer;

/**
 * @author reeta
 *
 */
public interface AppTimerListenerInterface extends Serializable {
	
	public abstract void timerExpired(ServletTimer timer );
}
