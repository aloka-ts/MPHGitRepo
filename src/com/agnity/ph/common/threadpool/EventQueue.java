package com.agnity.ph.common.threadpool;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
//import java.util.concurrent.atomic.AtomicBoolean;


public class EventQueue {

	BlockingQueue<EventWork> Q;
	//private AtomicBoolean isActive = new AtomicBoolean(true);
	private volatile long lastProcessingTime = System.currentTimeMillis();


	/**
	 * Constructor to create Queue for given index
	 * @param index
	 */
	public EventQueue() {
		Q = new LinkedBlockingQueue<EventWork>();
	}

	/**
	 * Method to submit work to the Queue
	 * @param obj
	 */
	public void enqueue(EventWork obj) {
		Q.offer(obj);
	}

	/**
	 * Method to get the work from the Queue
	 * @return
	 * @throws InterruptedException
	 */
	public EventWork dequeue() throws InterruptedException {
		return Q.take();
	}

	/**
	 * Method to Examine the topmost Queue element
	 * @return
	 */
	public EventWork peek() {
		return Q.peek();
	}

	/**
     * Method to get the current size of the Queue
     * @return
     */
    public int size() {
        return Q.size();
    }
    
	/**
	 * Return the status of the Queue
	 */
//	public boolean isActive() {
//		return this.isActive.get();
//	}

//	public void activateQueue(){
//	    this.isActive = new AtomicBoolean(true);
//	}
	
	/**
	 * Method to set shutdown parameter
	 */
//	public void shutdown() {
//		this.isActive.set(false);
//	}

	public void setLastProcessingTime(long time) {
		lastProcessingTime = time;
	}

	public long getLastProcessingTime() {
		return lastProcessingTime;
	}


} // end-of-class
