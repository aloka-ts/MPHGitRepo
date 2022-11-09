package com.agnity.ph.ainscf.acg;

import java.util.Enumeration;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.collections4.iterators.EnumerationIterator;

public class SamplingTask extends TimerTask {

	@Override
	public void run() {
		// TODO Auto-generated method stub
		ConcurrentHashMap<String, ACGAppSpecificRule> rules=ACGAppSpecificRuleParser.getAllApplicationRules();
		
		Enumeration<ACGAppSpecificRule> specifRules=rules.elements();
		
		while(specifRules.hasMoreElements()){
			ACGAppSpecificRule specRule=specifRules.nextElement();
			specRule.getMap_NPA_NXX().clear();
			specRule.getMap_NPA_NXX_XXXX().clear();
		}

	}

}
