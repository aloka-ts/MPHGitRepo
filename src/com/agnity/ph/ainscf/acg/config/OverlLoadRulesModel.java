package com.agnity.ph.ainscf.acg.config;

import java.util.ArrayList;

public class OverlLoadRulesModel {

	 private ArrayList<OverloadLevelRule> overloadLevelRules;

	    public ArrayList<OverloadLevelRule> getOverloadLevelRules() { return overloadLevelRules; }
	    public void setOverloadLevelRules(ArrayList<OverloadLevelRule> value) { this.overloadLevelRules = value; }

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("overloadLevelRules : ");
		sb.append(overloadLevelRules);

		return sb.toString();
	}

}
