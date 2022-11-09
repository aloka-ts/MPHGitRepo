/*******************************************************************************
 * Copyright (c) 2016 Agnity, Inc. All rights reserved.
 * <p>
 * This is proprietary source code of Agnity, Inc.
 * <p>
 * Agnity, Inc. retains all intellectual property rights associated
 * with this source code. Use is subject to license terms.
 * <p>
 * This source code contains trade secrets owned by Agnity, Inc.
 * Confidentiality of this computer program must be maintained at
 * all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/

package com.agnity.ph.soap;

import org.apache.log4j.Logger;

import javax.xml.soap.SOAPConnection;
import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Singleton helper class for creating SOAP connection and sending request to SOAP server.
 * Created by ankitsinghal on 03/04/16.
 */
public class PhSOAPConnection {
    private static Logger logger = Logger.getLogger(PhSOAPConnection.class);

    private static volatile PhSOAPConnection phsoapConnection = new PhSOAPConnection();

    SOAPConnectionFactory soapConnectionFactory;

    private PhSOAPConnection() {
        try {
            if (null == soapConnectionFactory) {
                soapConnectionFactory = SOAPConnectionFactory.newInstance();
            }
        } catch (SOAPException se) {
            logger.error("Error creating soap connection factory: " + se.getMessage());
        }
    }

    /**
     * Returns a singleton instance of PhSOAPConnection
     * @return PhSOAPConnection object
     */
    public PhSOAPConnection getInstance() {
        return phsoapConnection;
    }

    /**
     * Sends Requests the SOAP server and returns the response
     * @param inputMessage Input SOAPMessage object
     * @param endpoint URL endpoint of the SOAP server
     * @return SOAP response
     * @throws SOAPException
     */
    public SOAPMessage getSOAPResult(SOAPMessage inputMessage, URL endpoint) throws SOAPException {
        if (null == inputMessage) {
            throw new IllegalArgumentException("Input soap message cannot be null!");
        }
        if (null == endpoint || endpoint.getPath().isEmpty()) {
            throw new IllegalArgumentException("URL endpoint cannot be empty!");
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Input SOAPMessage: " + inputMessage + ", endPoint: " + endpoint.getPath());
        }
        SOAPConnection connection = null;
        SOAPMessage result = null;
        try {
            connection = soapConnectionFactory.createConnection();
            result = connection.call(inputMessage, endpoint);
            if (logger.isDebugEnabled()) {
                logger.debug("SOAP call result : " + result);
            }
        } catch (SOAPException se) {
            logger.error("Error received on soap call: " + se.getMessage());
            throw se;
        } finally {
            if (null != connection) {
                try {
                    connection.close();
                } catch (SOAPException se) {
                    logger.error("Error closing soap client connection! " + se.getMessage());
                }
            }
        }
        return result;
    }

    /**
     * Sends Requests the SOAP server and returns the response
     * @param inputMessage Input SOAPMessage object
     * @param endpoint URL Endpoint as String of the SOAP server
     * @return SOAP response
     * @throws SOAPException
     */
    public SOAPMessage getSOAPResult(SOAPMessage inputMessage, String endpoint) throws SOAPException {
        try {
            URL url = new URL(endpoint);
            return getSOAPResult(inputMessage, url);
        } catch (MalformedURLException e) {
            logger.error("Invalid URL! " + endpoint);
            throw new SOAPException(e.getMessage());
        }
    }
}
