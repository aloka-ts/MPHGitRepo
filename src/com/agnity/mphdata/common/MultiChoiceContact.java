package com.agnity.mphdata.common;

import java.io.Serializable;

import javax.servlet.sip.SipURI;

public class MultiChoiceContact implements Serializable{

	String tgrp;
	String trunkContext;
	SipURI contactUri;
	String trunkGroup;
	String phoneNumber;
	String termIp;
	String npdi;
	String pAIHdr;
	String psx_rn;
	String psx_dpc;
	String psx_ssn;
	String spid;
	int termPort = -1;
	private String urep;
	private String ucat;

	public String getPhoneNumber() {
		return phoneNumber;
	}

	public void setPhoneNumber(String phoneNumber) {
		this.phoneNumber = phoneNumber;
	}

	public String getPAIHdr() {
		return pAIHdr;
	}

	public void setPAIHdr(String pAIHdr) {
		this.pAIHdr = pAIHdr;
	}

	public SipURI getContactUri() {
		return contactUri;
	}

	public void setContactUri(SipURI contactUri) {
		this.contactUri = contactUri;
	}

	public String getDtg() {
		return trunkGroup;
	}

	public void setDtg(String trunkGroup) {
		this.trunkGroup = trunkGroup;
	}

	public String getTgrp() {
		return tgrp;
	}

	public void setTgrp(String tgrp) {
		this.tgrp = tgrp;
	}

	public String getTrunkContext() {
		return trunkContext;
	}

	public void setTrunkContext(String trunkContext) {
		this.trunkContext = trunkContext;
	}
	
	public String getTrunkGroup() {
		return trunkGroup;
	}


	public void setNpdi(String npdi) {
		this.npdi=npdi;
		
	}
	
	public String getNpdi() {
		return npdi;
	}

	public void setTermPort(int port) {
		termPort=port;
		
	}

	public String getTermIp() {
		return termIp;
	}

	public void setTermIp(String termIp) {
		this.termIp = termIp;
	}

	public int getTermPort() {
		return termPort;
	}
	
	public String getPsx_rn() {
		return psx_rn;
	}

	public void setPsx_rn(String psx_rn) {
		this.psx_rn = psx_rn;
	}

	public String getPsx_dpc() {
		return psx_dpc;
	}

	public void setPsx_dpc(String psx_dpc) {
		this.psx_dpc = psx_dpc;
	}

	public String getPsx_ssn() {
		return psx_ssn;
	}

	public void setPsx_ssn(String psx_ssn) {
		this.psx_ssn = psx_ssn;
	}

	public String getPsxspid() {
		return spid;
	}

	public void setPsxspid(String spid) {
		this.spid = spid;
	}

	public void setPsxUrep(String urep) {
		// TODO Auto-generated method stub
		this.urep=urep;
		
	}

	public void setPsxUcat(String ucat) {
		// TODO Auto-generated method stub
		this.ucat=ucat;
		
	}
}
