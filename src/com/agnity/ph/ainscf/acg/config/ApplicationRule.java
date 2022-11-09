package com.agnity.ph.ainscf.acg.config;

import java.util.ArrayList;



//Application:
//    SessionCountBase: 36000
//    AppId: 249
//    OverloadLevel:
//    -
//      SessionCountPmi: 40000
//      CPU: 65
//      Memory: 65
//      Level: 3.1
//    -
//      SessionCountPmi: 45000
//      CPU: 67
//      Memory: 67
//      Level: 3.2
public class ApplicationRule {
	
	private long sessionCountBase;
    public String getAppId() {
		return appId;
	}
	public void setAppId(String appId) {
		this.appId = appId;
	}
	private String appId;
    private ArrayList<OverloadLevel> overloadLevel;

    public long getSessionCountBase() { return sessionCountBase; }
    public void setSessionCountBase(long value) { this.sessionCountBase = value; }

    public ArrayList<OverloadLevel> getOverloadLevel() { return overloadLevel; }
    public void setOverloadLevel(ArrayList<OverloadLevel> value) { this.overloadLevel = value; }
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("SessionCountBase : ");
		sb.append(sessionCountBase);
		sb.append("AppId : ");
		sb.append(appId);
		sb.append("OverLoadLevel : ");
		sb.append(overloadLevel);
		
		return sb.toString();
	}

}
