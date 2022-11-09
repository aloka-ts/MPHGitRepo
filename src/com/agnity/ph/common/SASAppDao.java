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
package com.agnity.ph.common;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Hashtable;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.sql.DataSource;
import oracle.jdbc.OracleStatement;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import oracle.jdbc.driver.OracleConnection;
import com.baypackets.ase.common.Registry;
import com.baypackets.ase.jndi.ds.DataSourceUtil;
import com.baypackets.ase.jndi.ds.odg.ODGDataSourceWrapper;
import com.baypackets.ase.util.AseAlarmService;
import com.baypackets.ase.util.Constants;
import com.baypackets.bayprocessor.slee.common.BaseContext;

/**
 * This class abstracts the DB access activities from the rest of the service.
 * All DB related operations are being performed in this class. 
 */
public class SASAppDao {

	/*
	 * logger : Logger instance for logging
	 */
	private static Logger logger = Logger.getLogger(SASAppDao.class);

	/*
	 * dataSource : SAS data source instance
	 */
	private static DataSource dataSource;
	private static ODGDataSourceWrapper dataSourceODG;

	private static int numOfDataSources = 0;

	//AC out of scope in current release
	//private static boolean isACEnabled = false;
	private static boolean isODGMode = false;
	private static boolean isRACMode = false;
	
	public static final String CAS_STARTUP_DB_RETRY_COUNT="cas.startup.db.retry.count";
	 public static final String CAS_STARTUP_DB_RETRY_WAITTIMEOUT="cas.startup.db.retry.waittimeout";
	 public static final int ALARM_DB_STATUS_DOWN = 1524;
	   public static final int ALARM_DB_STATUS_UP= 1525;
	   public static final int ALARM_CAS_SHUTING_DOWN_NO_DB=1526;
//	   public static final int ALARM_CAS_STARTING_DB_STATUS_OK=1527;
	   
	  public DbStatusCheck dbStatusCheck= new DbStatusCheck();

	private SASAppDao() {

	}

	/**
	 * Static block to get data source object
	 */
	static {
		boolean alarm_raised=false;
		String PROVIDER_URL = "file:" + System.getProperty("ase.home")
		+ "/jndiprovider/fileserver/";
		String CONTEXT_FACTORY = "com.sun.jndi.fscontext.RefFSContextFactory";
		if (logger.isDebugEnabled()) {
			logger.debug("Getting Data source");
		}

//	int retryCount=	Integer.parseInt(StringUtils.defaultIfBlank(BaseContext.getConfigRepository().getValue(
//			CAS_STARTUP_DB_RETRY_COUNT), "1"));
//	int db_wait_time=	Integer.parseInt(StringUtils.defaultIfBlank(BaseContext.getConfigRepository().getValue(
//			CAS_STARTUP_DB_RETRY_WAITTIMEOUT), "5"));
		
//		if(retryCount>0) {
//			for(int i=0;i<retryCount;i++) {
			try {
//				if(logger.isDebugEnabled()) {
//					logger.debug("Retry count :- "+ retryCount + " db_wait_time:- "+ db_wait_time);
//					logger.debug("Staarting to look for db connectivity for :- "+ i+ " th time ");		
//				}
				numOfDataSources=0;
				Hashtable<String, String> env = new Hashtable<String, String>();
				env.put(Context.INITIAL_CONTEXT_FACTORY, CONTEXT_FACTORY);
				env.put(Context.PROVIDER_URL, PROVIDER_URL);
				InitialContext ctx = new InitialContext(env);
				if (DataSourceUtil.getProperties("APPDB")!=null){
					try {
						
						dataSource = (DataSource) ctx.lookup("APPDB");
						logger.debug("dataSourceRAC-->"+dataSource);
						if(dataSource!=null) {
							numOfDataSources++;
							isRACMode =true;
						}	
					}
					catch(Exception e) {
						logger.error("RAC Data Source not available.Exception=>"+e.getMessage());
					}
				}
				//AC out of scope in current release
				/*try {
					dataSourceAC = (ODGACDataSourceWrapper) ctx.lookup("AC_APPDB");
					logger.debug("dataSourceAC-->"+dataSourceAC);
					if(dataSourceAC!=null) {
						numOfDataSources++;
						isACEnabled =true;
					}
				}
				catch(Exception e) {
					logger.error("AC Data Source not available.Exception=>"+e.getMessage());
				}*/
				if (DataSourceUtil.getProperties("DG_APPDB")!=null){
					try {
						dataSourceODG = (ODGDataSourceWrapper) ctx.lookup("DG_APPDB");
						logger.debug("dataSourceODG-->"+dataSourceODG);
						if(dataSourceODG!=null) {
							numOfDataSources++;
							isODGMode =true;
						}
					}
					catch(Exception e) {
						logger.error("DG Data Source not available.Exception=>"+e.getMessage());
					}
				}

				if (numOfDataSources==0 || numOfDataSources>1) {
					isRACMode=false;
					//isACEnabled=false;
					isODGMode=false;
				//	logger.error("Sleeping thread for :-" + db_wait_time + " as not able to connect to db will retry count at :- "+ i);
				//	Thread.sleep(db_wait_time*1000);
					logger.error("No or Multiple Data Sources Available");
					if(!alarm_raised) {
						alarm_raised=true;
						reportAlarm(ALARM_DB_STATUS_DOWN,"No connection to db available");
					}
		
					//if(i==(retryCount-1)) {
						//raise an alarm and system exit
//						reportAlarm(ALARM_CAS_SHUTING_DOWN_NO_DB, "cas shutting down as db connectivity is not establied");
//						System.exit(0);
						throw new Exception("Zero or Multiple Data Sources Available");	
					//}
					
				}

			} catch (Exception e) {
				logger.error("Exception in datasource lookup", e);
			}
			if(numOfDataSources==1) {
			//	if(alarm_raised) {
				//	alarm_raised=false;
					if(logger.isDebugEnabled()) {
						logger.debug("sending clearing alarm as connection is established after retrying ");	
					}	
					//reportAlarm(ALARM_CAS_STARTING_DB_STATUS_OK, "db connection is established, so cas is up");
				//}
				
				reportAlarm(ALARM_DB_STATUS_UP,"Connection to db established");
				logger.debug("Starting new thread to moniter db status ");
				
				DbStatusCheck dbStatusCheck= new DbStatusCheck();
				dbStatusCheck.start();
			
			//	break;
			}
//			}
//			} else {
//				logger.error("Db retry count is set to 0 so not setting any db lookup ");
//			}
		}

	/*public static boolean isACEnabled() {
	return isACEnabled;
}*/

	
	
	public static void reportAlarm(int alarmCode,String alarmMessage){
    	if(alarmMessage!=null){
    		if (logger.isDebugEnabled()) {
        		logger.debug("Reporting alarm for failed service operation to EMS/wEMS agent...");
        	}
    		try {
    			AseAlarmService service = (AseAlarmService)Registry.lookup(Constants.NAME_ALARM_SERVICE); 
    			// Sending alarm code with trouble subsystem id as dummy for wEMS
    			service.sendAlarm(alarmCode,12,alarmMessage);
    			
    			if (logger.isDebugEnabled()) {
    				logger.debug("Successfully reported alarm to EMS/wEMS.");
    			}
    		} catch (Exception e) {
    			String msg = "Error occurred while reporting failed service deployment alarm to EMS: " + e.getMessage();
    			logger.error(msg, e);
    		}
    	}
	}
	public static boolean isODGMode() {
		return isODGMode;
	}

	public static boolean isRACMode() {
		return isRACMode;
	}

	/**
	 * This method attempts to get the DB connection from data source
	 * 
	 * @return Connection : DB connection object
	 */

	public static Connection getReadConnection() throws Exception{
		logger.debug("getReadConnection()=> isRACMode-->"+isRACMode);
		logger.debug("getReadConnection()=> isODGMode-->"+isODGMode);
		//logger.debug("getReadConnection()=> isACEnabled-->"+isACEnabled);

		if (isRACMode) {
			return getConnection(2);
		}
		else if (isODGMode) {
			return dataSourceODG.getReadConnection();
		}
		/*else if (isACEnabled) {
		return dataSourceAC.getReadConnection();
	}	*/	
		return null;
	}

	public static Connection getWriteConnection() throws Exception{

		logger.debug("getReadConnection()=> isRACMode-->"+isRACMode);
		logger.debug("getReadConnection()=> isODGMode-->"+isODGMode);
		//logger.debug("getReadConnection()=> isACEnabled-->"+isACEnabled);

		if (isRACMode) {
			return getConnection(2);
		}
		else if (isODGMode) {
			return dataSourceODG.getWriteConnection();
		}
		/*else if (isACEnabled) {
		return dataSourceAC.getWriteConnection();
	}	*/	
		return null;		
	}

	public static Connection getConnection(int maxConnCnt) {
		Connection conn = null;
		for (int i = 0; i < maxConnCnt; i++) {
			try {
				conn = dataSource.getConnection();
				if (conn == null) {
					throw new SQLException("Database connection is null");
				}
				if (logger.isDebugEnabled()) {
					logger.debug("Got DB connection");
				}
				break;
			} catch (SQLException ex) {
				conn = null;
				logger.error("Attempt " + i + "; Error in getting connection "
						+ ex.getMessage());
				if (logger.isInfoEnabled()) {
					logger.error("Attempt " + i
							+ "; Error while getting database connection", ex);
				}
			}
		}
		return conn;
	}

	public static ResultSet executeQuery(String query, PreparedStatement stmt) throws Exception{

		//if (isRACMode || isACEnabled) {
		if (isRACMode) {
			if (query!=null)
				return stmt.executeQuery(query);
			else 
				return stmt.executeQuery();
		}
		else if (isODGMode) {
			return dataSourceODG.executeQuery(query, stmt);
		}		
		return null;		
	}

	public static int executeUpdate(String query, Statement stmt) throws Exception{

		//if (isRACMode || isACEnabled) {
		if (isRACMode) {
			return stmt.executeUpdate(query);
		}
		else if (isODGMode) {
			return dataSourceODG.executeUpdate(query, stmt);
		}		
		return -1;	
	}

	public static int executeUpdate(String query, PreparedStatement stmt) throws Exception{

		//if (isRACMode || isACEnabled) {
		if (isRACMode) {
			if (query!=null)
				return stmt.executeUpdate(query);
			else 
				return stmt.executeUpdate();
		}
		else if (isODGMode) {
			return dataSourceODG.executeUpdate(query, stmt);
		}		
		return -1;		
	}

	public static void executeRO(CallableStatement proc) throws Exception{

		//if (isRACMode || isACEnabled) {
		if (isRACMode) {
			proc.execute();
		}
		else if (isODGMode) {
			dataSourceODG.executeROProc(proc);
		}				
	}

	public static void executeRW(CallableStatement proc) throws Exception{

		//if (isRACMode || isACEnabled) {
		if (isRACMode) {
			proc.execute();
		}
		else if (isODGMode) {
			dataSourceODG.executeRWProc(proc);
		}				
	}

	/**
	 * This method is for cleanup of the DB resources. It closes the statements,
	 * connections and result set.
	 * 
	 * @param conn
	 *            represents the instance of Connection
	 * @param stmt
	 *            represents the instance of Statement
	 * @param stmtKey
	 *            represents the String variable.
	 * @param rs
	 *            represents the instance of ResultSet
	 */
	public static void cleanupResources(Connection conn, Statement stmt,
			String stmtKey, ResultSet rs) {
		try {
			if (rs != null) {
				rs.close();
				rs = null;
			}
		} catch (Exception ex) {
			logger.error("Error while closing resultset.");
			if (logger.isInfoEnabled()) {
				logger.error("Error while closing resultset.", ex);
			}
		}

		try {
			if (stmt != null) {
				if (stmtKey != null) {
					((OracleStatement) stmt).closeWithKey(stmtKey);
				} else {
					stmt.close();
				}
				stmt = null;
			}
		} catch (Exception ex) {
			logger.error("Error while closing statement: " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("Error while closing statement.", ex);
			}
		}

		try {
			if (conn != null) {
				conn.close();
				conn = null;
			}
		} catch (Exception ex) {
			logger.error("Error while closing DB connection.");
			if (logger.isInfoEnabled()) {
				logger.error("Error while closing connection.", ex);
			}
		}
	}

	/**
	 * This method performs the roll back of database transaction.
	 * 
	 * @param conn
	 *            represents the instance of Connection
	 */
	public static void rollback(Connection conn) {
		if (conn != null) {
			try {
				conn.rollback();
			} catch (Exception ex) {
				logger.error("Exception in rolling back transaction: "
						+ ex.getMessage());
				if (logger.isInfoEnabled()) {
					logger.error("Failed to rollback transaction.", ex);
				}
			}
		}
	}
	
	public static class DbStatusCheck extends Thread{
		private static Logger logger = Logger.getLogger(DbStatusCheck.class);
		public static final String CAS_STARTUP_DB_LISTENER_WAITTIMEOUT="cas.db.tns.listner.error.wait.time";
		 public static final String CAS_STARTUP_DB_LISTENER_RETRY_COUNT="cas.db.tns.listener.error.retry.count";
		public String query = "select sysdate from dual";
		boolean alarm_raised =false;
		 int retryCount=	Integer.parseInt(StringUtils.defaultIfBlank(BaseContext.getConfigRepository().getValue(
					CAS_STARTUP_DB_LISTENER_RETRY_COUNT), "10"));
		int db_wait_time=	Integer.parseInt(StringUtils.defaultIfBlank(BaseContext.getConfigRepository().getValue(
					CAS_STARTUP_DB_LISTENER_WAITTIMEOUT), "10"));
		int count=0;
		private Connection conn;
		private PreparedStatement stmt;
		private ResultSet rs = null;
		public void run () {
			while(true) {
				try {
				 conn =	SASAppDao.getReadConnection();
				 if (conn == null) {
						logger.error("Got null connection : ");
						 dbConnectivityDown();
						Thread.sleep(db_wait_time*1000);
					}
				 else {
				if(logger.isDebugEnabled()) {
					 logger.debug("Got connection now excecuting query:- "+ query);	
				}
				
				 stmt = ((OracleConnection) conn).getStatementWithKey(query);
					if (stmt == null) {
						stmt = conn.prepareStatement(query);
					}
					rs = SASAppDao.executeQuery(null, stmt);
					
					if(rs.next()) {
					String sysdate=	rs.getString("sysdate");
					if(StringUtils.isNotBlank(sysdate)) {
						if(logger.isDebugEnabled()) {
							logger.debug("DB connectivity is working fine ,so putting it sleep for :- "+ db_wait_time+ " and setting count to 0");		
						}
						
						count=0;
						if(alarm_raised) {
							alarm_raised=false;
							if(logger.isDebugEnabled()) {
								logger.debug("sending clearing alarm as connection is established after retrying ");	
							}	
							reportAlarm(ALARM_DB_STATUS_UP,"Connection to db established");
							//reportAlarm(ALARM_CAS_STARTING_DB_STATUS_OK, "db connection is established, so cas is up");
						}
						Thread.sleep(db_wait_time*1000);
						continue;
					}else {
					   dbConnectivityDown();
					   Thread.sleep(db_wait_time*1000);
					}
					}else {
						  dbConnectivityDown();
						  Thread.sleep(db_wait_time*1000);
					}
				 }	
				}catch(Exception e) {
					logger.debug("Error occured in thread running:-"+ e);
					 dbConnectivityDown();
					
				}finally{
					SASAppDao.cleanupResources(conn, stmt, query, rs);
				}
			}
			
		}
		public void dbConnectivityDown(){
			if(logger.isDebugEnabled()) {
				 logger.debug("DB connectivity is down so incrementing retry counter " +count);		
			}
			    count++;
			    if(!alarm_raised) {
					alarm_raised=true;
					reportAlarm(ALARM_DB_STATUS_DOWN,"No connection to db available");
				}
			    if(count==retryCount) {
			    	logger.error("Shutting down cas as db connectivity has gone down");
			    	reportAlarm(ALARM_CAS_SHUTING_DOWN_NO_DB, "cas shutting down as db connectivity is not establied");
					System.exit(0);
			    }
		}
	}
}
