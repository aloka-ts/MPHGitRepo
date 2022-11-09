package com.agnity.ph.diameter;

public interface DiameterConstants {
	
	
	public static final long  VENDOR_ID_3GPP =10415;
	
	public static final long  VENDOR_ID_ERICSSON =193;
	
	public static final String VENDOR_NAME_3GPP="3GPP";
	
	public static final String VENDOR_NAME_TGPP="TGPP";
	public static final String VENDOR_NAME_BASE ="base";
	
	public static final String VENDOR_NAME_ERICSSON="Ericsson";
	public static final long  VENDOR_ID_BASE =0;
	
	public static final long AUTH_APPLICATION_ID_SH=16777217;
	
	/**
	 * receiving incoming CCR request
	 */
	public static final short CCR_IN_EVENT=4;
	/**
	 * Session based ccr request
	 */
	public static final short CCR_IN_INITIAL=1;
	public static final short CCR_IN_INERIM=2;
	public static final short CCR_IN_TERMINATION=3;
	
	/**
	 * Sending CCR request
	 */
	public static final short CCR_OUT_INITIAL_REQUEST		= 1;
	public static final short CCR_OUT_UPDATE_REQUEST		= 2;
	public static final short CCR_OUT_TERMINATION_REQUEST	= 3;
	public static final short CCR_OUT_EVENT_REQUEST		= 4;
	/**
	 * receiving  Accounting request either event or session based
	 */
	public static final short AR_EVENT_RECORD=4;
	/**
	 * Session based
	 */
	public static final short AR_START_RECORD=5;
	public static final short AR_INERIM_RECORD=6;
	public static final short AR_STOP_RECORD=7;
	

	
	/**
	 * Sending Accounting request either event or session based
	 */
	public static final int AR_EVENT = 1;
	public static final int AR_SESSION = 2;
	// CC Request Type
		
	
	/**
	 * types of AVPS  e.g. Float32 for DiameterFloat32AVP, Integer32 for DiameterInteger32AVP
	 */
	public static final String Float32="Float32";
	public static final String Float64="Float64";
	public static final String Integer32="Integer32";
	public static final String Unsigned32="Unsigned32";
	public static final String Integer64="Integer64";
	public static final String Unsigned64="Unsigned64";
	public static final String OctetString="OctetString";
	public static final String Generic="Generic";
	public static final String Grouped="Grouped";
}
