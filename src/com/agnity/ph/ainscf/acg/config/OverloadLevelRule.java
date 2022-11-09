package com.agnity.ph.ainscf.acg.config;

import java.util.ArrayList;

public class OverloadLevelRule {
	private ApplicationRule applicationRule;

    public ApplicationRule getApplicationRule() { return applicationRule; }
    public void setApplicationRule(ApplicationRule value) { this.applicationRule = value; }


	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("ApplicationRule : ");
		sb.append(applicationRule);
		
		return sb.toString();
	}


}
