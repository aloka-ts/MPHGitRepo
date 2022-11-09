package com.agnity.ph.diameter.emum;

public enum ReportingReason {
	
	THRESHOLD,
	QHT,
	FINAL,
	QUOTA_EXHAUSTED,
	VALIDITY_TIME,
	OTHER_QUOTA_TYPE,
	RATING_CONDITION_CHANGE,
	FORCED_REAUTHORISATION,
	POOL_EXHAUSTED,
	UNUSED_QUOTA_TIMER;
	
	public static String findName(int code){
		
	String reason=null;
		switch(code){
		case 0:{
			reason=ReportingReason.THRESHOLD.name();
		}case 1:{
			reason=ReportingReason.QHT.name();
		}case 2:{
			reason=ReportingReason.FINAL.name();
		}case 3:{
			reason=ReportingReason.QUOTA_EXHAUSTED.name();
		}case 4:{
			reason=ReportingReason.VALIDITY_TIME.name();
		}case 5:{
			reason=ReportingReason.OTHER_QUOTA_TYPE.name();
		}case 6:{
			reason=ReportingReason.RATING_CONDITION_CHANGE.name();
		}case 7:{
			reason=ReportingReason.FORCED_REAUTHORISATION.name();
		}case 8:{
			reason=ReportingReason.POOL_EXHAUSTED.name();
		}case 9:{
			reason=ReportingReason.UNUSED_QUOTA_TIMER.name();
		}
	}
		return reason;
	}

}
