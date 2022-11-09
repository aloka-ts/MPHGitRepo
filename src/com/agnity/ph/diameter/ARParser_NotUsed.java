package com.agnity.ph.diameter;

import org.apache.log4j.Logger;

//import com.baypackets.ase.ra.diameter.rf.RfAccountingRequest;
import com.baypackets.ase.resource.ResourceException;

public class ARParser_NotUsed {
	
	private static ARParser_NotUsed ccrParser = new ARParser_NotUsed();
	private static Logger logger = Logger.getLogger(ARParser_NotUsed.class);

	private ARParser_NotUsed() {
	};

	public static ARParser_NotUsed getParser() {
		return ccrParser;
	}
	
	
	// <--- generic method section starts --->
//		public ARAVPAttributes_NotUSed parseAR(RfAccountingRequest accrequest) throws ResourceException {
//			return null;
//			
//		}

}
