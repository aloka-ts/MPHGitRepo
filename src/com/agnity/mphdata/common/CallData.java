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
package com.agnity.mphdata.common;

import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.PersistanceType;
import org.apache.log4j.Logger;

import java.io.Serializable;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

/**
 * Call Data represents a vessel for all IE's Transferred to and from PH/Application
 * Application/PH can set the data in a MAP in the form of key value pair.
 * Whenever required any attribute can be retrieved from map.
 */
public class CallData implements Serializable {

    private static Logger logger = Logger.getLogger(CallData.class);
    private static final long serialVersionUID = -8950386471372036222L;
    public static final transient String CALL_DATA = "CALL-DATA";

    private Map<CallDataAttribute, Object> persitableData = new EnumMap<CallDataAttribute, Object>(CallDataAttribute.class);
    private transient Map<CallDataAttribute, Object> nonpersitableData = new EnumMap<CallDataAttribute, Object>(CallDataAttribute.class);

    public Object get(CallDataAttribute key) {
        if (PersistanceType.PERSISTABLE == key.getPersistanceType()) {
            return getPersistableData(key);
        } else {
            return getNonpersistableData(key);
        }
    }

    public void set(CallDataAttribute key, Object value) {
        if (PersistanceType.PERSISTABLE == key.getPersistanceType()) {
            setPersistableData(key, value);
        } else {
            setNonpersistableData(key, value);
        }
    }

    public void remove(CallDataAttribute key) {
        if (PersistanceType.PERSISTABLE == key.getPersistanceType()) {
            removePersistableData(key);
        } else {
            removeNonpersistableData(key);
        }
    }

    private Object getPersistableData(CallDataAttribute key) {
        Object val = persitableData.get(key);
        if (logger.isDebugEnabled()) {
            logger.debug("getPersistableData Key : " + key + " Value : " + val);
        }
        return val;
    }

		private Object getNonpersistableData(CallDataAttribute key) {
	
			Object val = null;
			if (nonpersitableData != null) {
				val = nonpersitableData.get(key);
			}
			if (logger.isDebugEnabled()) {
				logger.debug("getNonpersistableData Key : " + key + " Value : "
						+ val);
			}
			return val;
		}

    private void setPersistableData(CallDataAttribute key, Object value) {
        if (logger.isDebugEnabled()) {
            logger.debug("setPersistableData Key : " + key + " Value : " + value);
        }
        persitableData.put(key, value);
    }

    private void setNonpersistableData(CallDataAttribute key, Object value) {

        if (logger.isDebugEnabled()) {
            logger.debug("setNonpersistableData Key : " + key + " Value : " + value);
        }
    	if(nonpersitableData == null){
    		nonpersitableData = new EnumMap<CallDataAttribute, Object>(CallDataAttribute.class);
    	}

        nonpersitableData.put(key, value);
    }

    private void removePersistableData(CallDataAttribute key) {
//        if (logger.isDebugEnabled()) {
//            logger.debug("removePersistableData Key : " + key);
//        }
        persitableData.remove(key);
    }

    private void removeNonpersistableData(CallDataAttribute key) {
//        if (logger.isDebugEnabled()) {
//            logger.debug("removeNonpersistableData Key : " + key);
//        }
        nonpersitableData.remove(key);
    }

    /**
     * This method is used to get app specific data from persistable MAP
     *
     * @return
     */
    public Object getAppSpecificPersistableData(String serviceId,String appSpecificDataKey) {
        if(null == appSpecificDataKey){
            throw new IllegalArgumentException("Key cannot be null");
        }
        return getAppSpecificData(serviceId,CallDataAttribute.P_APP_SPECIFIC_DATA, appSpecificDataKey);
    }

    /**
     * This method is used to get app specific data from persistable MAP
     *
     * @return
     */
    public Object getAppSpecificNonPersistableData(String serviceId,String appSpecificDataKey) {
        if(null == appSpecificDataKey){
            throw new IllegalArgumentException("Key cannot be null");
        }
        return getAppSpecificData(serviceId,CallDataAttribute.NP_APP_SPECIFIC_DATA, appSpecificDataKey);
    }


    /**
     * This method is used to get app specific data from input map
     *
     * @return
     */
    private Object getAppSpecificData(String serviceId,CallDataAttribute insertDataType, String appSpecificDataKey) {
        Object returnVal = null;
        Map<CallDataAttribute, Object> inputMap;
        if (insertDataType == CallDataAttribute.P_APP_SPECIFIC_DATA) {
            inputMap = persitableData;
        } else {
            inputMap = nonpersitableData;
        }
        Object appSpecificMapData = inputMap.get(CallDataAttribute.P_APP_SPECIFIC_DATA);
        if (appSpecificMapData instanceof Map) {
            returnVal = ((Map) appSpecificMapData).get(appSpecificDataKey);
            if (logger.isDebugEnabled()) {
                logger.debug(" getAppSpecificData Key : " + appSpecificDataKey + " Value : " + returnVal);
            }
        } else {
            if (null != appSpecificMapData) {
                //Error!!! Expected map only here!! logger_error and return null..
                logger.error("getAppSpecificData Key : " + appSpecificDataKey + ", is not an instance of Map!! returning null!");
            }
        }
        return returnVal;
    }

    /**
     * This method is used to set app specific data in persistable MAP
     *
     * @return
     */
    public void setAppSpecificPersistableData(String serviceId,String appSpecificDataKey, Object value) {
        if(null == appSpecificDataKey){
            throw new IllegalArgumentException("Key cannot be null");
        }
        setAppSpecificData(serviceId,CallDataAttribute.P_APP_SPECIFIC_DATA, appSpecificDataKey, value);
    }

    /**
     * This method is used to set app specific data in persistable MAP
     *
     * @return
     */
    public void setAppSpecificNonPersistableData(String serviceId,String appSpecificDataKey, Object value) {
        if(null == appSpecificDataKey){
            throw new IllegalArgumentException("Key cannot be null");
        }
        setAppSpecificData(serviceId,CallDataAttribute.NP_APP_SPECIFIC_DATA, appSpecificDataKey, value);
    }

    /**
     * This method is used to set app specific data in input map
     *
     * @return
     */
    private void setAppSpecificData(String serviceId,CallDataAttribute insertDataType, String appSpecificDataKey, Object value) {
        Map<CallDataAttribute, Object> inputMap;
        if (insertDataType == CallDataAttribute.P_APP_SPECIFIC_DATA) {
            inputMap = persitableData;
        } else {
            inputMap = nonpersitableData;
        }
        Object appSpecificMapData = inputMap.get(CallDataAttribute.P_APP_SPECIFIC_DATA);
        if (appSpecificMapData instanceof Map) {
            ((Map) appSpecificMapData).put(appSpecificDataKey, value);
            if (logger.isDebugEnabled()) {
                logger.debug("setAppSpecificData Key : " + appSpecificDataKey + " Value : " + value);
            }
        } else {
            if (null == appSpecificMapData) {
                Map<String, Object> appSpecificMap = new HashMap<String, Object>();
                appSpecificMap.put(appSpecificDataKey, value);
                inputMap.put(CallDataAttribute.P_APP_SPECIFIC_DATA, appSpecificMap);
            } else {
                logger.error("setAppSpecificData Key : " + appSpecificDataKey + ", is not an instance of Map!!!");
            }
        }
    }


    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<CallDataAttribute, Object> entry : persitableData.entrySet()) {
            CallDataAttribute key = entry.getKey();
            Object value = entry.getValue();
            sb.append("Key = ").append(key).append(", value = ");
            if (value instanceof String) {
                sb.append((String) value);
            } else if (value instanceof Integer) {
                sb.append((Integer) value);
            } else {
                sb.append(" ?? " + value);
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    //FIXME: What is the need for this method!
    public Map<CallDataAttribute, Object> getPersitableData() {
        return persitableData;
    }
}
