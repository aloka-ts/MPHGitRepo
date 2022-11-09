package com.agnity.ph.common.threadpool;


import org.apache.log4j.Logger;

import com.baypackets.bayprocessor.slee.common.BaseContext;
import com.baypackets.bayprocessor.slee.common.ThreadAlreadyRegisteredException;
import com.baypackets.bayprocessor.slee.common.ThreadNotRegisteredException;
import com.baypackets.bayprocessor.slee.threadmonitor.MonitoredThread;
import com.baypackets.bayprocessor.slee.threadmonitor.MonitoredThreadState;
import com.baypackets.bayprocessor.slee.threadmonitor.ThreadMonitor;
import com.baypackets.bayprocessor.slee.threadmonitor.ThreadOwner;

class EventWorkerThread extends MonitoredThread implements Runnable {
	
	public static Logger logger = Logger.getLogger(EventWorkerThread.class);

	/** 
	 * The Queue that this Thread belongs to. 
	 */
	EventQueue eQueue;

	private ThreadOwner threadOwner = null;
	
	private ThreadMonitor threadMonitor = null;
	
	private int queueNumber = 0;
	
	private boolean stopped = false;
	
	/** 
	 * The constructor. 
	 *  
	 * @param eq The EventQueue
	 */
//	private EventWorkerThread(EventQueue eq) {
//		eQueue = eq;
//	}

	EventWorkerThread(EventQueue eq, 
	        String threadName, 
	        int threadTimeout, 
	        int queueNumber) {
	    
	    super(threadName, threadTimeout, BaseContext.getTraceService());
        this.eQueue = eq;
        this.setQueueNumber(queueNumber);
    }
	
	/** 
	 * Scan for and finish work.
	 * Enhanced to update the timestamp on each work that needs to be processed.
	 * Also, on the invocation of this thread, now the Registration/Unregistration of this thread needs to be done
	 * with ThreadMonitor.
	 * @author Ankit Singhal
	 */
	public void run() {

	    EventWork work = null;

	    // Register thread with thread monitor
	    try {
	        // Set thread state to idle before registering
	        this.setThreadState(MonitoredThreadState.Idle);
	        this.threadMonitor.registerThread(this);
	    } catch(ThreadAlreadyRegisteredException exp) {
	        logger.error("This thread is already registered with Thread Monitor" + exp);
	    } catch(Throwable e) {
	    	 logger.error("run() EventWorkerThread"+e);
	    }
	    
	    //Process the work
	    try{
	//        while (eQueue.isActive()) {
	        while(!this.stopped) {
	            try {
	                work = eQueue.dequeue();
	                eQueue.setLastProcessingTime(System.currentTimeMillis());

	                if (work != null) {
	                    if (logger.isDebugEnabled()) {
	                        logger.debug("Got a new work! : " + work + ", Checking validity..run()  EventWorkerThread");
	                    }
	                    if (work.isValid()) {
	                        // Update time in thread monitor
	                        this.updateTimeStamp();

	                        // Set thread state to running before blocking on dequeue
	                        this.setThreadState(MonitoredThreadState.Running);

	                        work.finishIt();
	                    } else {
	                        //Trying to print the current callState String
	                    	 logger.error("Target is not valid so not executing it...run() EventWorkerThread");
	                    }
	                    //	                    // Set thread state to idle before blocking on dequeue
	                    //	                    this.setThreadState(MonitoredThreadState.Idle);
	                }else{
	                	 logger.error("Work is null!!run() EventWorkerThread");
	                }
	            } catch (InterruptedException ie) {
	            	 logger.error("Caught in Thread interrupted: " + ie.getMessage());
	            } catch(Throwable th) {
	                //Catch the throwable to handle some runtime error kills the thread.
	            	 logger.error("Caught in Throwable.." + th);
	            }
	            
	            // Set thread state to idle before blocking on dequeue
	            this.setThreadState(MonitoredThreadState.Idle);
	        }
	    }catch(Throwable th) {
	    	logger.error("Caught at Thread Level. I will die... : run() EventWorkerThread" + th);
	    } finally {
	        // Unregister thread with thread monitor
	        try {
	            this.threadMonitor.unregisterThread(this);
	        } catch(ThreadNotRegisteredException exp) {
	        	logger.error("This thread is not registered with Thread Monitor" + exp.getMessage());
	        }
	    }
	}

	public void shutdown(){
	   // this.eQueue.shutdown();
	    this.stopped = true;
	    if (this.getThreadState() == MonitoredThreadState.Idle) {
	        if (logger.isDebugEnabled()) {
	           logger.debug("Thread [" + this.getName() + "] is being stoppped shutdown()");
	        }
	        this.interrupt();
	    }
	    this.setThreadState(MonitoredThreadState.Stopped);
	}

	public ThreadOwner getThreadOwner() {
        return threadOwner;
    }

    public void setThreadOwner(ThreadOwner owner) {
        threadOwner = owner;
    }
    
    public void setThreadMonitor(ThreadMonitor tm) {
        threadMonitor = tm;
    }

    /**
     * @return the queueNumber
     */
    public int getQueueNumber() {
        return queueNumber;
    }

    /**
     * @param queueNumber the queueNumber to set
     */
    public void setQueueNumber(int queueNumber) {
        this.queueNumber = queueNumber;
    }
    
} // end-of-class
