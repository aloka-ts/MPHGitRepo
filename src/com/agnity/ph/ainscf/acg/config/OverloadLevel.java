package com.agnity.ph.ainscf.acg.config;

public class OverloadLevel {

	 private int sessionCount;
	    private int cpu;
	    private int memory;
	    private int level;

	    public int getSessionCount() { return sessionCount; }
	    public void setSessionCount(int value) { this.sessionCount = value; }

	    public int getCPU() { return cpu; }
	    public void setCPU(int value) { this.cpu = value; }

	    public int getMemory() { return memory; }
	    public void setMemory(int value) { this.memory = value; }

	    public int getLevel() { return level; }
	    public void setLevel(int value) { this.level = value; }
	    
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("cpu : ");
		sb.append(cpu);
		sb.append("memory : ");
		sb.append(memory);
		sb.append("level : ");
		sb.append(level);
		
		return sb.toString();
	}

}
