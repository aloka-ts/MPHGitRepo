package com.agnity.ph.ainscf.acg;

import java.util.LinkedHashMap;
import java.util.TreeMap;

import com.agnity.ph.ainscf.acg.config.ApplicationRule;
import com.agnity.ph.ainscf.acg.config.OverloadLevel;

public class ACGAppSpecificRule {
	
	private  LinkedHashMap<String,OverloadLevel> map_NPA_NXX= new LinkedHashMap<String,OverloadLevel>();
	
    private  LinkedHashMap<String,OverloadLevel> map_NPA_NXX_XXXX= new LinkedHashMap<String,OverloadLevel>();
	
	private ApplicationRule rule;
	
	public  LinkedHashMap<String, OverloadLevel> getMap_NPA_NXX() {
		return map_NPA_NXX;
	}

	
	public  LinkedHashMap<String, OverloadLevel> getMap_NPA_NXX_XXXX() {
		return map_NPA_NXX_XXXX;
	}

	public ApplicationRule getRule() {
		return rule;
	}

	public void setRule(ApplicationRule rule) {
		this.rule = rule;
	}

}
