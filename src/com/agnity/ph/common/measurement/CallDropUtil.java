package com.agnity.ph.common.measurement;


import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipServletResponse;

import com.agnity.ain.asngenerated.Set;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.measurement.enums.SipMessage;
import com.agnity.ph.sip.SipProtocolHelper;
import com.baypackets.ase.container.AseApplicationSession.AppSessionAttributeHolder;



public class CallDropUtil {

	String mphProFile = System.getProperty("ase.home")+ "/conf/mph.properties";

	String value ="";
	ConcurrentHashMap<String, ArrayList<Integer>> cmap = new ConcurrentHashMap<String ,ArrayList<Integer>>();
	PhMeasurementService measurementservice = PhMeasurementService.getInstance();
	private long SipTrafficCounter  ;
	
	PhMeasurementUtills phMeasurementUtills = new PhMeasurementUtills();
	PhMeasurementCounter phMeasurementCounter  = new PhMeasurementCounter(Protocol.SIP);
	SipServletResponse sipResponse;
	
	
	
	
	


	
	
	public void setSipTrafficCounter() {
		this.SipTrafficCounter = phMeasurementCounter.getCountForSipMessage(SipMessage.REQ_INVITE);
	}
	
	
	
	
	public long  getSipTraffiCounter() {
		setSipTrafficCounter();
		return this.SipTrafficCounter ;	
	}
	
	
	
	
	
	public void loadfile(String mphProFile ,  String key) {
		
		File mphpropFileHandle = new File(mphProFile);
		try {
		FileInputStream fs = new FileInputStream(mphpropFileHandle);
		Properties  p = new Properties();
		
		p.load(fs);
		
		
		 
		
		for(Object o: p.keySet()) {
			if(o.toString().equals(key)) {
				this.value = p.getProperty(key);
			}
			
			else {
				System.out.print("the key specificd dose not exit in mph.properties file!!");
			}
		}
	}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	
	
	
	
	
    
	public void ParseValue() {
		
		System.out.println("you will get the list.get(0) sip <SipThreshold> - list.get(1) then <TcapThreshold> in that order for particular appId ");
		
		String [] strarr = this.value.split(",");
	
		for(int i=0;i<strarr.length;i++) {
		 	 
			String [] insidedata = strarr[0].split("-");
			ArrayList<Integer> list = new ArrayList<>();
			list.add(Integer.parseInt(insidedata[1]));
			list.add(Integer.parseInt(insidedata[2]));
			this.cmap.put(insidedata[0], list); 
			
		}
	
		
	}
    
	
	public void callDroping() {
		
		for(String s : cmap.keySet()) {
			
			long count =phMeasurementCounter.getCountForServiceTriggered(s);
			if(count>cmap.get(s).get(0)) {
				
				SipApplicationSession	appSession = this.sipResponse.getApplicationSession();
				SipProtocolHelper.dropCall(appSession);
			
		}
		
	}
	}
}
	

