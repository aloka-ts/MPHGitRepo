/*******************************************************************************
 * Copyright (c) 2011 Agnity, Inc. All rights reserved.
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
package com.agnity.ph.common;

import com.agnity.mphdata.common.Protocol;
import jain.protocol.ss7.SccpUserAddress;

import java.util.List;

/**
 * This interface is used to initialise SS7 Protocol config and is
 * implemented by config classes of all the protocols .Services uses the
 * Protocol Config classes of a specific SS7 protocol to initialise the specific SS7 protocol
 * configuration. 
 *
 */
public interface Ss7ProtocolConfig {

    //List<String> getServiceKeyList();
    
    List<String> getServiceKeyList(String serviceId);

  //  List<SccpUserAddress> getSccpLocalAddressList();
    
    List<SccpUserAddress> getSccpLocalAddressList(String serviceId);
    Protocol getProtocol();
}
