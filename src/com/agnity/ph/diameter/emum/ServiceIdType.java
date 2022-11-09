package com.agnity.ph.diameter.emum;

public enum ServiceIdType {
	END_USER_E164,END_USER_IMSI,END_USER_SIP_URI,END_USER_NAI,END_USER_PRIVATE;
	
	public static String findName(int code){
		
	String svc_id_type=null;
		switch(code){
		case 0:{
			svc_id_type=ServiceIdType.END_USER_E164.name();
		}case 1:{
			svc_id_type=ServiceIdType.END_USER_IMSI.name();
		}case 2:{
			svc_id_type=ServiceIdType.END_USER_IMSI.name();
		}case 3:{
			svc_id_type=ServiceIdType.END_USER_IMSI.name();
		}case 4:{
			svc_id_type=ServiceIdType.END_USER_PRIVATE.name();
		}
	}
		return svc_id_type;
	}

}
