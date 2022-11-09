package com.agnity.ph.common.threadpool;

import java.sql.SQLException;

import org.apache.log4j.Logger;

import com.baypackets.bayprocessor.slee.common.BaseContext;
import com.baypackets.bayprocessor.slee.common.InitializationFailedException;
import com.baypackets.bayprocessor.slee.threadmonitor.MonitoredThread;
import com.baypackets.bayprocessor.slee.threadmonitor.ThreadMonitor;
import com.baypackets.bayprocessor.slee.threadmonitor.ThreadOwner;


/**
 * Class which implements thread pool on event Queue.
 * Each thread of this pool has its own queue and it keeps running till Queue is active.
 * 
 * @author Reeta
 *
 */
public class EventThreadPool implements ThreadOwner{
	
	
	public static Logger logger = Logger.getLogger(EventThreadPool.class);

    /**
     * Thread pool size
     */
    private int size;

    public int getSize() {
		return size;
	}


	/**
     * The threads in the pool.
     */
    protected EventWorkerThread threads[] = null;

    /**
     * The backlog of assignments, which are waiting for the thread pool.
     */
    EventQueue[] eQueue = null;

    /*
     * The thread owner of the thread pool. 
     * This will be kept as null, because pool itself is the owner of all XeQtor threads.
     */
    private ThreadOwner owner = null;

    /*
     * The monitoring thread API
     */
    private ThreadMonitor threadMonitor = null;

    //Thread Timeout value. Read from BaseContext.ConfigRepository
    private static int threadTimeoutTime = 100;

    private static String dbThreadNamePrefix = "Ph-XeQtor-";

    /**
     * The constructor.
     * 
     * @param size Number of threads in the thread pool.
     * @throws InitializationFailedException 
     */
    private EventThreadPool(int size) {
        this.size = size;
        threads = new EventWorkerThread[size];
        eQueue = new EventQueue[size];
    }

    /**
     * Overloaded constructor accepting the threadOwner.
     */
    public EventThreadPool(int size, ThreadOwner threadOwner, int threadTimeout) {
        this(size);
        this.owner = threadOwner;
        EventThreadPool.threadTimeoutTime = threadTimeout;
    }

    /**
     * Initializes Queues and Threads
     */
    public void initialize() {
        logger.error("Entering...initialize()");

        ///Initializing ThreadMonitor//////
        logger.error("Initializing ThreadMonitor..initialize()");
        threadMonitor = new ThreadMonitor();
        threadMonitor.initialize(BaseContext.getTraceService(), false);
        logger.error("ThreadMonitor Initialized...initialize()");
        
        ///Initializing ThreadPool//////
        logger.error("Initializing ThreadPool..initialize()");
        (new ThreadPool("EventThreadPool")).start();
        logger.error("Thread Pool Initialized..initialize()");
        
        logger.error("Leaving...initialize()");
    }

    /**
     * Add a task to the thread pool.
     * Any class which implements the EventWork interface may be assigned.
     * When this task runs, its finishIt() method will be called.
     * 
     * @param work An object that implements the Runnable interface
     */
    public void assign(EventWork work) {

        int index = work.getQueueIndex();

        if (logger.isDebugEnabled()) {
            logger.debug("Assigning work to Queue [" + index + "] assign()");
        }

        eQueue[index].enqueue(work);

    }

    /**
     * Print the number of works assigned to Queues
     */
    public void printQueueInfo() {
        StringBuilder queueInfo = new StringBuilder();
        for (int i=0; i<eQueue.length; i++) {
            queueInfo.append(eQueue[i].size() + ",");
        }
        logger.error("QueueInfo==>" + queueInfo +"printQueueInfo()");
    }

    /**
     * Set the flag to shutdown Queue.
     * @author Ankit Singhal
     */
    public void shutdown() {

        logger.error("Shutting XeQtor threads shutdown()");
        for (int i = 0; i < threads.length; i++) {
            threads[i].shutdown();
        }
        
        logger.error("Shutting ThreadMonitor");
        threadMonitor.stop();
    }


    /**
     * Check if queue is valid for new work
     * @param index
     * @param threshold
     * @param timeout
     * @return
     */
    public boolean isValidQueue(int index, int threshold, int timeout) {
        boolean isValid = true;
        int pendingMessages = eQueue[index].size();
        long processingTime = System.currentTimeMillis() - eQueue[index].getLastProcessingTime();

        if (threshold < 0 && timeout < 0) {
            isValid = true;
        } else if (threshold < 0) {
            if (pendingMessages > 0 && processingTime > timeout) {
                isValid = false;
            }
        } else if (timeout < 0) {
            if (pendingMessages > threshold) {
                isValid = false;
            }
        } else if (pendingMessages > threshold && processingTime > timeout) {
            isValid = false;
        }

        if (!isValid) {
            logger.error(pendingMessages + "," + processingTime + "Queue[" + index + "]");
        }
        return isValid;
    }

    /**
     * Inner class of EventThreadPool which is itself Thread.
     * This will create Thread pool in parallel to application processing.
     *  
     * @author Vaibhav
     *
     */
    class ThreadPool extends Thread {

        public ThreadPool(String name) {
            this.setName(name);
        }

        public void run() {
            logger.error("Entering...run()");

            for (int index = 0; index < size; index++) {

                eQueue[index] = new EventQueue();
                EventWorkerThread eWThread = initializeEventWorkerThread(index);

                threads[index] = eWThread;

                logger.error("Thread " + threads[index].getName() + " created. run()");

                try {
                    spawnThread(threads[index]);
                } catch (Exception ex) {
                    logger.error("Could not start thread: " + ex.getMessage());
                }
            } // for
            logger.error("Leaving... run()");
        }

    } // end-of-class ThreadPool

    /**
     * API for starting the thread
     * @param mt Instance of EventWorkerThread
     * @author Ankit Singhal
     */
    private void spawnThread(EventWorkerThread mt){
        logger.error("Scheduling thread " + mt.getName());
        mt.start();
    }

    /**
     * API to initialize the EventWorkerThread and setting the ThreadMonitor and ThreadOwner
     * @param threadIndex Queue Id for which this thread belongs
     * @return Thread Instance
     * @author Ankit Singhal
     */
    public EventWorkerThread initializeEventWorkerThread(int threadIndex){
        String tName = dbThreadNamePrefix + threadIndex;
        EventWorkerThread eWThread = new EventWorkerThread(
                eQueue[threadIndex], 
                tName,
                EventThreadPool.getThreadTimeoutTime(),
                threadIndex
                );
        eWThread.setThreadOwner(this.owner == null ? this : this.owner);
        eWThread.setThreadMonitor(this.threadMonitor);
        //eQueue[threadIndex].activateQueue();
        try {
            BaseContext.getDbAccessService().registerThdConnection(tName);
        } catch (SQLException e) {
            logger.error("Could not register thread: " + e.getMessage());
        }
        
        return eWThread;
    }

    public void setThreadMonitor(ThreadMonitor tm) {
        threadMonitor = tm;
    }

    public static int getThreadTimeoutTime() {
        if (logger.isInfoEnabled()) {
            logger.info("Returning thread timeout time = " + threadTimeoutTime +getThreadTimeoutTime());
        }
        return threadTimeoutTime;
    }

    
    /**
     * Callback API initiated by ThreadMonitorThread in the event of thread Expiry
     * @param Thread for which callback is received. (Expired Thread)
     * @author Ankit Singhal
     */
    @Override
    public int threadExpired(MonitoredThread thread) {
        if (logger.isDebugEnabled()) {
            logger.debug("Entering..."+ 
                    "threadExpired()");
        }

        logger.error("Thread " + thread.getName() + " has expired.. Trying to spawn a new thread..threadExpired()");

        //Fetch the queue number on which the new thread should poll now.
        int queueNumber = ((EventWorkerThread)thread).getQueueNumber();

        //log expired thread stack::
        StackTraceElement[] arrSte = thread.getStackTrace();
        StringBuilder stackTraceErrorStr = new StringBuilder("");
        for(StackTraceElement ste : arrSte){
            stackTraceErrorStr.append(ste).append(System.getProperty("line.separator"));
        }
        logger.error("ThreadStackTrace for thread: " + 
                        thread.getName() + 
                        ": " + 
                        stackTraceErrorStr+"threadExpired()");   

        //Print the next topmost element of the queue for which my cute thread expired.. ;)
        //This will not remove the first element rather will just print the first element.
        logger.error("NextElement of my Queue is: " + eQueue[queueNumber].peek()+ "threadExpired()");

        EventWorkerThread eWThread = initializeEventWorkerThread(queueNumber);

        //Update the thread's array with the new Thread created
        threads[queueNumber] = eWThread;

        logger.error("Spawning new Thread for Queue: " + queueNumber+ "threadExpired()");
        spawnThread(threads[queueNumber]);

        try {
            //Shutdown the Thread
            ((EventWorkerThread)thread).shutdown();
        } catch(Exception exp) {
            logger.error("threadExpired()",exp);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("Leaving...threadExpired()");
        }
        return CONTINUE;
    }


} // end-of-class
