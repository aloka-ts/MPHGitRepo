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
package com.agnity.mphdata.common;

import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.enums.PersistanceType;
import com.baypackets.ase.util.TimeBasedUUIDGenerator;

import org.apache.log4j.Logger;

import java.io.Serializable;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

/**
 * legdata is sued to store information that are relevent to one leg i.e orig or term.
 * Application/PH can set the data in a MAP in the form of key value pair.
 * Whenever required any attribute can be retrieved from map.
 */

public class LegData implements Serializable {

    private static final long serialVersionUID = 558378882164660795L;
    private static Logger logger = Logger.getLogger(LegData.class);
    private Map<LegDataAttributes, Object> persitableData = new EnumMap<LegDataAttributes, Object>(LegDataAttributes.class);
    private transient Map<LegDataAttributes, Object> nonpersitableData = new EnumMap<LegDataAttributes, Object>(LegDataAttributes.class);
    private String uniqueIdentifier = "";

    public LegData() {
        this.uniqueIdentifier = TimeBasedUUIDGenerator.getUUID();
    }

    @Override
    public int hashCode() {
        return uniqueIdentifier.hashCode();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
        	return true;
        }
        if (o == null || getClass() != o.getClass()) {
        	return false;
        }

        LegData legData = (LegData) o;
        return uniqueIdentifier.equals(legData.getUniqueIdentifier());
    }

    public final String getUniqueIdentifier() {
        return uniqueIdentifier;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<LegDataAttributes, Object> entry : persitableData.entrySet()) {
            LegDataAttributes key = entry.getKey();
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
        
        if(nonpersitableData!=null){
        for (Map.Entry<LegDataAttributes, Object> entry : nonpersitableData.entrySet()) {
            LegDataAttributes key = entry.getKey();
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
      
        }
        
        return sb.toString();

    }

    public Object get(LegDataAttributes key) {
        if (PersistanceType.PERSISTABLE == key.getPersistanceType()) {
            return getPersistableData(key);
        } else {
            return getNonpersistableData(key);
        }
    }

    private Object getPersistableData(LegDataAttributes key) {
        Object val = persitableData.get(key);
        if (logger.isDebugEnabled()) {
            logger.debug("getPersistableData Key : " + key + " Value : " + val);
        }
        return val;
    }

    private Object getNonpersistableData(LegDataAttributes key) {
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

    public void set(LegDataAttributes key, Object value) {
        if (PersistanceType.PERSISTABLE == key.getPersistanceType()) {
            setPersistableData(key, value);
        } else {
            setNonpersistableData(key, value);
        }
    }

    private void setPersistableData(LegDataAttributes key, Object value) {
        if (logger.isDebugEnabled()) {
            logger.debug("setPersistableData Key : " + key + " Value : " + value);
        }
        persitableData.put(key, value);
    }

    private void setNonpersistableData(LegDataAttributes key, Object value) {
        if (logger.isDebugEnabled()) {
            logger.debug("setNonpersistableData Key : " + key + " Value : " + value);
        }
        if(nonpersitableData == null){
    		nonpersitableData = new EnumMap<LegDataAttributes, Object>(LegDataAttributes.class);
    	}
        nonpersitableData.put(key, value);
    }

    public void remove(LegDataAttributes key) {
        if (PersistanceType.PERSISTABLE == key.getPersistanceType()) {
            removePersistableData(key);
        } else {
            removeNonpersistableData(key);
        }
    }

    private void removePersistableData(LegDataAttributes key) {
//        if (logger.isDebugEnabled()) {
//            logger.debug("removePersistableData Key : " + key);
//        }
        persitableData.remove(key);
    }

    private void removeNonpersistableData(LegDataAttributes key) {
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
    public Object getAppSpecificPersistableData(String appSpecificDataKey) {
        return getAppSpecificData(LegDataAttributes.P_APP_SPECIFIC_DATA, appSpecificDataKey);
    }

    /**
     * This method is used to get app specific data from input map
     *
     * @return
     */
    private Object getAppSpecificData(LegDataAttributes insertDataType, String appSpecificDataKey) {
        Object returnVal = null;
        Map<LegDataAttributes, Object> inputMap = persitableData;
        if (insertDataType == LegDataAttributes.P_APP_SPECIFIC_DATA) {
            inputMap = persitableData;
        } else {
            inputMap = nonpersitableData;
        }
        Object appSpecificMapData = inputMap.get(LegDataAttributes.P_APP_SPECIFIC_DATA);
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
     * This method is used to get app specific data from persistable MAP
     *
     * @return
     */
    public Object getAppSpecificNonPersistableData(String appSpecificDataKey) {
        return getAppSpecificData(LegDataAttributes.NP_APP_SPECIFIC_DATA, appSpecificDataKey);
    }

    /**
     * This method is used to set app specific data in persistable MAP
     *
     * @param appSpecificDataKey
     * @return
     */
    public void setAppSpecificPersistableData(String appSpecificDataKey, Object value) {
        setAppSpecificData(LegDataAttributes.P_APP_SPECIFIC_DATA, appSpecificDataKey, value);
    }

    /**
     * This method is used to set app specific data in input map
     *
     * @param appSpecificDataKey
     * @return
     */
    private void setAppSpecificData(LegDataAttributes insertDataType, String appSpecificDataKey, Object value) {
        Map<LegDataAttributes, Object> inputMap;
        if (insertDataType == LegDataAttributes.P_APP_SPECIFIC_DATA) {
            inputMap = persitableData;
        } else {
            inputMap = nonpersitableData;
        }
        Object appSpecificMapData = inputMap.get(LegDataAttributes.P_APP_SPECIFIC_DATA);
        if (appSpecificMapData instanceof Map) {
            ((Map) appSpecificMapData).put(appSpecificDataKey, value);
            if (logger.isDebugEnabled()) {
                logger.debug("setAppSpecificData Key : " + appSpecificDataKey + " Value : " + value);
            }
        } else {
            if (null == appSpecificMapData) {
                Map<String, Object> appSpecificMap = new HashMap<String, Object>();
                appSpecificMap.put(appSpecificDataKey, value);
                inputMap.put(LegDataAttributes.P_APP_SPECIFIC_DATA, appSpecificMap);
            } else {
                logger.error("setAppSpecificData Key : " + appSpecificDataKey + ", is not an instance of Map!!!");
            }
        }
    }

    /**
     * This method is used to set app specific data in persistable MAP
     *
     * @param appSpecificDataKey
     * @return
     */
    public void setAppSpecificNonPersistableData(String appSpecificDataKey, Object value) {
        setAppSpecificData(LegDataAttributes.NP_APP_SPECIFIC_DATA, appSpecificDataKey, value);
    }
}
