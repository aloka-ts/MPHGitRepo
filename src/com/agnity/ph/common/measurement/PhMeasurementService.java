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
package com.agnity.ph.common.measurement;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.sip.SipProtocolConfig;

/**
 * This class will maintain separate measurement parameters for each protocol.
 * @author rbehl
 *
 */
public class PhMeasurementService {

	private Map<Protocol, PhMeasurementCounter> measurementCounters = Collections.synchronizedMap(new EnumMap<Protocol, PhMeasurementCounter>(Protocol.class));
	private static Logger logger = Logger.getLogger(PhMeasurementService.class);
	private static PhMeasurementService instance;
	private Set<Protocol> registeredProtocols;

	public static PhMeasurementService getInstance() {
		if(instance == null) {
			synchronized(PhMeasurementService.class) {
				if(instance == null) {
					if(logger.isDebugEnabled()) {
						logger.debug("creating instance of PhMeasurementService");
					}
					instance = new PhMeasurementService();

					instance.intialize();
				}
			}
		}

		return instance;
	}

	public void registerMeasurementCounters(Protocol protocol) {
		if(measurementCounters.get(protocol) == null) {
			synchronized(PhMeasurementService.class) {
				if(measurementCounters.get(protocol) == null) {
					registeredProtocols.add(protocol);
					measurementCounters.put(protocol, new PhMeasurementCounter(protocol));
					if(logger.isDebugEnabled()) {
						logger.debug("Measurement counter instance successfully registered for protocol : " + protocol + 
								" current protocol registered : " + registeredProtocols);
					}
				}
			}
		}

	}

	public MeasurementCounter getMeasurementCounter(Protocol protocol) {
		return measurementCounters.get(protocol);
	}

	public void intialize() {
		if(logger.isDebugEnabled()) {
			logger.debug("Intializing PhMeasurementService...");
		}

		Boolean dumpCounters = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.DUMP_COUNTERS));
		if(dumpCounters) {

			this.registeredProtocols = new TreeSet<Protocol>(new Comparator<Protocol>() {
				@Override
				public int compare(Protocol p1, Protocol p2) {
					return p1.toString().compareTo(p2.toString());
				}
			} );

			
			final ThreadFactory customThreadFactory = new ThreadFactory() {
				
				@Override
				public Thread newThread(Runnable r) {
					Thread t = new Thread(r, "DumpCounter");
					if(t.isDaemon()) {
						t.setDaemon(false);
					}
					if(t.getPriority() != Thread.NORM_PRIORITY) {
						t.setPriority(Thread.NORM_PRIORITY);
					}
					
					return t;
				}
			};
			
			ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1, customThreadFactory);
			
			String dumpCountersTime = SipProtocolConfig.getConfigData(SipProtocolConfig.DUMP_COUNTERS_TIME);
			long period = dumpCountersTime != null ? Long.parseLong(dumpCountersTime) : 0L;
			if(logger.isDebugEnabled()) {
				logger.debug("starting dumpCounter thread for time period of " + period + " seconds");
			}
			executor.scheduleAtFixedRate(new DumpCounter(), 60, period, TimeUnit.SECONDS);

			if(logger.isDebugEnabled()) {
				logger.debug("PhMeasurementService initialized successfully");
			}
		} else {
			if(logger.isDebugEnabled()) {
				logger.debug("Not intializing PhMeasurementService as flag is " + dumpCounters);
			}
		}


	}

	public class DumpCounter implements Runnable {
		private BufferedWriter bufferedWriter;
		private String dateDirectory;
		public DumpCounter() {
			try {
				DateFormat df = new SimpleDateFormat("MM_dd_yyyy");
				dateDirectory = df.format(new Date());
				String path = "/LOGS/CAS" + File.separator + dateDirectory + File.separator + "phcounters.log"; 

				File file = new File(path);
				if(!file.exists()) {
					file.createNewFile();
				}
				bufferedWriter = new BufferedWriter(new FileWriter(file));
				StringBuilder sb = new StringBuilder();
				sb.append("TimeStamp,").
				append("nInviteReceived,").
				append("nBeginReceived,").append("nBeginTransmit,").
				append("nEndReceived,").append("nEndTransmit,").
				append("nAbortReceived,").append("nAbortTransmit,").
				append("nSipMediaInvocation,").append("nSS7MediaInvocation,").
				append("assistMediainvocation,").
				append("AppId,").append("nTrigger,").append("nTriggerByChaining");
				
				System.out.println("PhMeasurement Counters stats : " + sb.toString());
				if(logger.isDebugEnabled()) {
					logger.debug("PhMeasurement Counters stats : " + sb.toString());
				}
			}catch(IOException ex) {
				logger.error("Error occured in intializing DumpCounter", ex);
			}

		}

		public void run() {
			try {
				if(isDateDirectoryChanged()) {
					if(logger.isDebugEnabled()) {
						logger.debug("date directory is changed to " + dateDirectory);
					}
					String path = "/LOGS/CAS" + File.separator + dateDirectory + File.separator + "phcounters.log"; 
					File file = new File(path);
					if(!file.exists()) {
						file.createNewFile();
					}
					bufferedWriter.close();
					bufferedWriter = new BufferedWriter(new FileWriter(file));
				}
				String counterString = PhMeasurementUtills.formatCounterString();
				bufferedWriter.write(counterString);
				bufferedWriter.newLine();

				bufferedWriter.flush();
			}catch(Exception ex) {
				//don't want to kill the thread.
				logger.error("Error occured while writing counters", ex);
			}
		}

		private boolean isDateDirectoryChanged() {
			DateFormat df = new SimpleDateFormat("MM_dd_yyyy");
			String currentDirectory = df.format(new Date());
			String newDateDir="/LOGS/CAS"+ File.separator+currentDirectory;
			
			//Path path = Paths.get(newDateDir);
			File file = new File(newDateDir);
			if(!dateDirectory.equals(currentDirectory) && file.exists()) {
				logger.error("isDateDirectoryChanged  -->"+ true); 
				dateDirectory = currentDirectory;
				return true;
			}

			return false;
		}
	}

	public Set<Protocol> getRegisteredProtocols() {
		return registeredProtocols;
	}
}
