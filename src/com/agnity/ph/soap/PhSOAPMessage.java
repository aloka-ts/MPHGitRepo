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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;

/**
 * Singleton helper class for creating SOAP messages.
 * Created by ankitsinghal on 03/04/16.
 */
public class PhSOAPMessage {

    private static Logger logger = Logger.getLogger(PhSOAPMessage.class);

    private static volatile PhSOAPMessage phsoapMessage = new PhSOAPMessage();

    private MessageFactory messageFactory;

    private PhSOAPMessage() {
        try {
            if (null == messageFactory) {
                messageFactory = MessageFactory.newInstance();
            }
        } catch (SOAPException se) {
            logger.error("Error creating soap message factory: " + se.getMessage());
        }
    }

    /**
     * Returns a singleton instance of this class
     * @return instance of PHSOAPMessage
     */
    public PhSOAPMessage getInstance() {
        return phsoapMessage;
    }

    /**
     * Creates an empty SOAP message from SOAP message factory
     * @return SOAP message containing empty header and body
     * @throws SOAPException
     */
    public SOAPMessage createEmptyMessage() throws SOAPException {
        SOAPMessage message = null;
        try {
            message = messageFactory.createMessage();
            if (logger.isDebugEnabled()) {
                logger.debug("Returning empty soap message: " + message);
            }
        } catch (SOAPException se) {
            logger.error("Error creating soap message: " + se.getMessage());
            throw se;
        }
        return message;
    }

    /**
     * Creates a soap message with SOAP body having JAX-B marshalled object supplied as an argument.
     * @param object Should be a JAX-B compliant object which will be marshalled into XML.
     * @return SOAP message with given object as XML in SOAP body.
     * @throws SOAPException
     * @throws JAXBException
     */
    public SOAPMessage createMessageWithObject(Object object) throws SOAPException, JAXBException {
        if (null == object) {
            throw new IllegalArgumentException("Object to be added in SOAP Message cannot be null!");
        }
        SOAPMessage message = null;
        try {
            message = createEmptyMessage();
            addObjectToMessageBody(message, object);
        } catch (SOAPException se) {
            logger.error("Error creating soap message with input object: " + se.getMessage());
        }
        return message;
    }

    private void addObjectToMessageBody(SOAPMessage inputMessage, Object object) throws JAXBException, SOAPException {
        try {
            JAXBContext jc = JAXBContext.newInstance(object.getClass());
            Marshaller marshaller = jc.createMarshaller();
            marshaller.marshal(object, inputMessage.getSOAPBody());
            inputMessage.saveChanges();
            if (logger.isDebugEnabled()) {
                logger.debug("Object added to soap message successfully: " + object);
            }
        } catch (JAXBException jaxbException) {
            logger.error("JAXB exception while adding object to message body: " + jaxbException.getMessage());
            throw jaxbException;
        } catch (SOAPException soapException) {
            logger.error("SOAP Exception while adding object to message body: " + soapException.getMessage());
            throw soapException;
        }
    }

}
