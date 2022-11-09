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
package com.agnity.service.mphTestApp;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;

import com.agnity.ph.common.AppStatsRegistery;
import com.agnity.ph.common.ProtocolHandlerHttpServlet;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.http.HttpProtocolHandler;

/**
 * 
 */

/**
 * HTTP servlet implementation for Test App
 *
 */
public class TestProtocolHandlerHttpServlet extends ProtocolHandlerHttpServlet {

	private static Logger logger = Logger.getLogger(TestProtocolHandlerHttpServlet.class);

	
	@Override
	 public void init(ServletConfig config) throws ServletException {
	        super.init(config);
	        if (logger.isDebugEnabled()) {
	            logger.debug("ProtocolHandlerHttpServlet() called...");
	        }

	        /*
	         * Instantiate the data access object used to find and persist call
	         * information to and from the backing store and register it with the
	         * ServletContext.
	         */
	        
	        TestAppStatsProvider testStatsProvider= new TestAppStatsProvider();
	        
	        if (logger.isDebugEnabled()) {
	            logger.debug("Register Testapp stats Provider for "+TestAppStatsProvider.SERVICE_NAME);
	        }
	        AppStatsRegistery.APP_STATS_REGISTRY.registerStatsProvider(TestAppStatsProvider.SERVICE_NAME, testStatsProvider);
	    }

	/**
	 * 
	 */
	public TestProtocolHandlerHttpServlet() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public ServiceInterface getServiceInstance() {
		logger.debug("Inside getServiceInstance");
		return  MphTestAppMainImpl.getInstance();
	}

}
