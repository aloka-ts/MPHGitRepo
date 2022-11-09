package com.agnity.ph.ainscf.acg;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Timer;
import java.util.TimerTask;

import org.apache.log4j.Logger;

import com.agnity.ph.ainscf.acg.config.ApplicationRule;
import com.agnity.ph.ainscf.acg.config.OverloadLevel;
import com.baypackets.ase.ocm.CPU;

public class ACGOveroadManager {

	private static Logger logger = Logger.getLogger(ACGOveroadManager.class);
	private static final ACGOveroadManager SELF_INSTANCE = new ACGOveroadManager();

	private static final int NPA_NXX_XXXX = 10;
	private static final int NPA_NXX = 6;

	public static ACGOveroadManager getInstance() {

		return SELF_INSTANCE;
	}

	public void initialize() {
		if (logger.isDebugEnabled()) {
			logger.debug("initialize");
		}
		ACGAppSpecificRuleParser.parseRules();
		
		TimerTask timerTask = new SamplingTask();
        //running timer task as daemon thread
        Timer timer = new Timer(true);
        timer.scheduleAtFixedRate(timerTask, 0, 60*1000);
        if (logger.isDebugEnabled()) {
			logger.debug("initialize TimerTask started");
        }
       
	}

	/**
	 * This method is used to check if particular number is overloaded
	 * @param appId
	 * @param number
	 * @return
	 */
	private boolean checkOverLoad(String appId, String number) {

		if (logger.isDebugEnabled()) {
			logger.debug("checkOverLoad");
		}
		boolean isOverLoad = false;
		ACGAppSpecificRule appSpecRule = ACGAppSpecificRuleParser
				.getApplicationRule(appId);

		ApplicationRule appRule = appSpecRule.getRule();

		List<OverloadLevel> definedOlLevels = appRule.getOverloadLevel();

		OverloadLevel numberOll = null;
		if (number != null) {
			if (number.length() == NPA_NXX_XXXX) {
				numberOll = appSpecRule.getMap_NPA_NXX_XXXX().get(number);
			} else if (number.length() == NPA_NXX) {
				numberOll = appSpecRule.getMap_NPA_NXX().get(number);
			}

			if (numberOll == null) {
				int cpu = numberOll.getCPU();
				int memory = numberOll.getMemory();
				int sesionCount = numberOll.getSessionCount();

				while (definedOlLevels.iterator().hasNext()) {

					OverloadLevel definedOlLevel = definedOlLevels.iterator().next();
					if (numberOll.getCPU() >= definedOlLevel.getCPU()) {
						isOverLoad = true;
					}
					if (numberOll.getMemory() >= definedOlLevel.getMemory()) {
						isOverLoad = true;
					}
					if (numberOll.getSessionCount() >= definedOlLevel
							.getSessionCount()) {
						isOverLoad = true;
					}
				}
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("checkOverLoad leaving "+isOverLoad);
		}
		return isOverLoad;
	}

	/**
	 * This method is used to update overload params on a request for this number
	 * @param appId
	 * @param number
	 * @param cpu
	 * @param memory
	 * @param sessionCount
	 * @return 
	 */
	public boolean updateAndCheckOverLoadLevel(String appId,String number){//int cpu,int memory,
		
	
		if (logger.isDebugEnabled()) {
			logger.debug("updateAndCheckOverLoadLevel entering for AppId "+appId +" Number "+number);
		}
		boolean overload=false;
	try{	
		int sessionCount=0;
		CPU _instance = CPU.getInstance();
		float cpu = _instance.getCurrentCPUUsage();
		float memory = _instance.getCurrentMemoryUsage();
		
		ACGAppSpecificRule appSpecRule = ACGAppSpecificRuleParser
				.getApplicationRule(appId);
		OverloadLevel numberOll = null;	
		
		LinkedHashMap<String, OverloadLevel> map=null;
		if (number != null) {
			
			if (number.length() == NPA_NXX_XXXX) {
				map = appSpecRule.getMap_NPA_NXX_XXXX();
				numberOll = map.get(number);
				
				if(numberOll==null){
					numberOll=new OverloadLevel();
					
				}else{
					sessionCount=numberOll.getSessionCount()+1;
				}
				numberOll.setCPU((int)cpu);
				numberOll.setMemory((int)memory);
				numberOll.setSessionCount(sessionCount);
				numberOll=setOverLoadLevel(numberOll,appId);
				
				if (logger.isDebugEnabled()) {
					logger.debug("updateAndCheckOverLoadLevel update CPU "+ cpu +" Memory "+memory +" Session"+sessionCount);
				}
		
				addUpdateEntryInMap(map,number,numberOll);
				
			} else if (number.length() == NPA_NXX) {
				
				map = appSpecRule.getMap_NPA_NXX();
				numberOll = map.get(number);
				if(numberOll==null){
					numberOll=new OverloadLevel();
					
				}else{
					sessionCount=numberOll.getSessionCount()+1;
				}
				numberOll.setCPU((int)cpu);
				numberOll.setMemory((int)memory);
				numberOll.setSessionCount(sessionCount);
				numberOll=setOverLoadLevel(numberOll,appId);
				
				if (logger.isDebugEnabled()) {
					logger.debug("updateAndCheckOverLoadLevel update CPU "+ cpu +" Memory "+memory +" Session"+sessionCount);
				}
				addUpdateEntryInMap(map,number,numberOll);
			}
			
			updateMapInSortedOrder(map);
			
			overload=checkOverLoad(appId, number);
		}
	  }catch(Exception e){
		  logger.error(" exception occurd while checking overload");
	  }
	if (logger.isDebugEnabled()) {
		logger.debug("updateAndCheckOverLoadLevel leaving " +overload);
	}
		return overload;
		
	}
	
	/**
	 * This method is used bye above method to set overload level for a number
	 * based on overload params and defined levels
	 * @param level
	 * @param appId
	 * @return
	 */
	private OverloadLevel setOverLoadLevel(OverloadLevel level,String appId){
		
		if (logger.isDebugEnabled()) {
			logger.debug("setOverLoadLevel " +level +" For service "+appId);
		}
		ACGAppSpecificRule appSpecRule = ACGAppSpecificRuleParser
				.getApplicationRule(appId);

		ApplicationRule appRule = appSpecRule.getRule();
        int levelVal=0;
		List<OverloadLevel> definedOlLevels = appRule.getOverloadLevel();
		while (definedOlLevels.iterator().hasNext()) {

			OverloadLevel definedOlLevel = definedOlLevels.iterator().next();
			levelVal=definedOlLevel.getLevel();
			if (level.getCPU() >= definedOlLevel.getCPU()) {
				break;
			}
			if (level.getMemory() >= definedOlLevel.getMemory()) {
				break;
			}
			if (level.getSessionCount() >= definedOlLevel
					.getSessionCount()) {
				break;
			}
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug("setOverLoadLevel leaving");
		}
		level.setLevel(levelVal);
		return level;
	}
	
	
	/**
	 * This method is used to add/update entry ina map for a new number
	 * @param map
	 * @param number
	 * @param numberOll
	 */
	private void addUpdateEntryInMap(LinkedHashMap<String, OverloadLevel> map, String number,
			OverloadLevel numberOll) {
		List<Map.Entry<String, OverloadLevel>> list = new ArrayList<Map.Entry<String, OverloadLevel>>(
				map.entrySet());

		if (logger.isDebugEnabled()) {
			logger.debug("addUpdateEntryInMap " +number  +"oll "+numberOll);
		}
		// Comparable Interface function to
		// sort the values of List
		if (list.size() == 256) {
            list.remove(255);
            map.clear();	
		}		
		for (Map.Entry<String, OverloadLevel> entry : list) {
			// Put all sorted value back to the
			// LinkedHashMap
			map.put(entry.getKey(), entry.getValue());
		}	
		map.put(number, numberOll);
		
		if (logger.isDebugEnabled()) {
			logger.debug("addUpdateEntryInMap leaving");
		}
	}
	
	/*
	 * After adding entry in the map sort the map as per level
	 */
	private void updateMapInSortedOrder(
			java.util.LinkedHashMap<String, OverloadLevel> map) {
		List<Map.Entry<String, OverloadLevel>> list = new ArrayList<Map.Entry<String, OverloadLevel>>(
				map.entrySet());

		if (logger.isDebugEnabled()) {
			logger.debug("updateMapInSortedOrder Entering");
		}
		// Comparable Interface function to
		// sort the values of List
		Collections.sort(list,
				new Comparator<Map.Entry<String, OverloadLevel>>() {
					// Comparing entries
					public int compare(Entry<String, OverloadLevel> entry1,
							Entry<String, OverloadLevel> entry2) {
						return entry1.getValue().getLevel()
								- entry2.getValue().getLevel();
					}
				});

		// Clear the above LinkedHashMap
		// using clear() method
		map.clear();

		// Iterating over elements using for each loop
		for (Map.Entry<String, OverloadLevel> entry : list) {

			// Put all sorted value back to the
			// LinkedHashMap
			map.put(entry.getKey(), entry.getValue());
		}
		if (logger.isDebugEnabled()) {
			logger.debug("updateMapInSortedOrder leaving");
		}

	}
}
