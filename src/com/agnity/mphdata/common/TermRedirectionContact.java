package com.agnity.mphdata.common;

import java.io.Serializable;
import java.util.Comparator;

import javax.servlet.sip.SipURI;

import com.agnity.ph.common.PhConstants;

/**
 * This class is used to save the redirection contact received in 302 from a
 * terminating route This class implements Comparator and comparable interfaces
 * for providing sorting of list of TermRedirectionContacts on the basis of Q
 * value received in contact
 * 
 * @author reeta
 *
 */
public class TermRedirectionContact implements
		Comparator<TermRedirectionContact>, Comparable<TermRedirectionContact>,Serializable {

	SipURI redirectContactUri;
	float qValue = 1;
	String redirectNum="";
	String redirectAddr="";
	int redirectPort=-1;
	String rn;
	String npdi ;
	String ssn ;
	String spid ;
	String dpc;
	String user;
	
	public String getNpdi() {
		return npdi;
	}

	public String getSsn() {
		return ssn;
	}

	public String getSpid() {
		return spid;
	}

	public String getDpc() {
		return dpc;
	}
	
	public String getRn() {
		return rn;
	}

	public void setRn(String rn) {
		this.rn = rn;
	}

	boolean isQValueSet;
	
	public boolean isQValueSet() {
		return isQValueSet;
	}

	public String getRedirectNum() {
		return redirectNum;
	}

	public String getRedirectAddr() {
		return redirectAddr;
	}

	public int getRedirectPort() {
		return redirectPort;
	}

	public void setRedirectNum(String redirectNum) {
		this.redirectNum = redirectNum;
	}

	public void setRedirectAddr(String redirectAddr) {
		this.redirectAddr = redirectAddr;
	}

	public void setRedirectPort(int redirectPort) {
		this.redirectPort = redirectPort;
	}

	public SipURI getRedirectContactUri() {
		return redirectContactUri;
	}

	public void setRedirectContactUri(SipURI redirectContactUri) {
		this.redirectContactUri = redirectContactUri;
	}

	public float getQValue() {
		return qValue;
	}

	public void setQValue(float qValue) {
		if (qValue >= 0) {
			isQValueSet=true;
			this.qValue = qValue;
		}
	}
	
	public void setNpdi(String npdi) {
		this.npdi = npdi;
	}

	public void setSsn(String ssn) {
		this.ssn = ssn;
	}

	public void setSpid(String spid) {
		this.spid = spid;
	}

	public void setDpc(String dpc) {
		this.dpc = dpc;
	}


	/**
	 * This method is used to compare two instances of this class
	 */
	@Override
	public int compare(TermRedirectionContact o1, TermRedirectionContact o2) {
		return o2.compareTo(o1);
	}

	/**
	 * This method provides the algo for comparing 2 instances of this class
	 */
	@Override
	public int compareTo(TermRedirectionContact another) {
		return ((Float) this.getQValue())
				.compareTo((Float) another.getQValue());
	}

	/**
	 * This method returns String representation of instance of this class
	 */
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("RredirectNum : ");
		sb.append(redirectNum);
		sb.append("RedirectAddr : ");
		sb.append(redirectAddr);
		sb.append("RedirectPort : ");
		sb.append(redirectPort);
		sb.append("RedirectionContact :");
		sb.append(redirectContactUri);
		sb.append(" Qvalue : ");
		sb.append(qValue);
		sb.append(" Npdi : ");
		sb.append(npdi);
		sb.append("Rn : ");
		sb.append(rn);
		sb.append("Ssn : ");
		sb.append(ssn);
		sb.append("Spid : ");
		sb.append(spid);
		sb.append("Dpc : ");
		sb.append(dpc);
		return sb.toString();
	}

	public void setUser(String user) {
		this.user=user;
		// TODO Auto-generated method stub	
	}
	
	public String getUser() {
		return this.user;
		// TODO Auto-generated method stub	
	}
}
