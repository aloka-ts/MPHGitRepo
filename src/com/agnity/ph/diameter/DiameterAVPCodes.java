package com.agnity.ph.diameter;

public interface DiameterAVPCodes {

	public static final long Session_Id = 263;
	public static final long Destination_Realm = 283;
	public static final long Destination_Host = 293;
	public static final long Origin_Host = 264;
	public static final long Origin_Realm = 296;
	public static final long Auth_Application_Id = 258;
	public static final long CC_Request_Type = 416;
	public static final long CC_Request_Number = 415;
	public static final long Service_Context_id = 461;
	public static final long User_Name = 1;
	public static final long Origin_State_ID = 278;
	public static final long Event_Timestamp = 55;
	public static final long Subscription_Id = 443;
	public static final long Mutil_Service_Credit_Control = 456;
	public static final long Service_Information = 873;
	public static final long Service_Identifier=439;
	public static final long Precedence = 1010;
	public static final long Unnown = 1013;
	public static final long Vendor_specific_Applictaion_id=260;
	public static final long vendor_id=266;

	public static final long Credit_Control_Failure_Handling = 427;
	public static final long Direct_Debiting_Failure_Handling = 428;

	public static final long AoC_Information=2054;
	public static final long AoC_Cost_Information=2053;
	public static final long Accumulated_Cost=2052;
	
	public static final long Requested_Service_Unit = 437;
	public static final long CC_Time = 420;
	public static final long CC_Money = 413;
	public static final int CC_Money_Unit_Value = 445;
	public static final int CC_Money_Value_digits = 447;
	public static final long CC_Money_Exponent = 429;
	public static final long CC_Total_Octets = 421;
	public static final long CC_Input_Octets = 412;
	public static final long CC_Output_Octets = 414;
	public static final long CC_Service_Specific_Units = 417;

	public static final long Granted_Service_Unit = 431;
	public static final long CC_Traffic_Time_Change = 451;

	public static final long Used_Service_Unit = 446;
	public static final long Reporting_Reason = 872;
	public static final long Tariff_Change_Usage = 452;
	public static final long Event_Charging_Timestamp = 1258;

	public static final long Final_Unit_Indication = 430;
	public static final long Final_Unit_Action = 449;
	public static final long Restriction_Filter_Rule = 438;
	public static final long Filter_Id = 11;
	public static final long Redirect_Server = 434;
	public static final long Redirect_Server_Address = 435;
	public static final long Redirect_Address_Type = 433;
	public static final long Subscription_Id_Type = 450;
	public static final long Subscription_Id_Data = 444;
	public static final long CC_Money_Currency_Code = 425;

	public static final long Announcement_Information = 3904;
	public static final long Announcement_Identifier = 3905;

	public static final long Announcement_Order = 3906;
	public static final long Ann_Variable_Part = 3907;
	public static final long Ann_Variable_Part_Order = 3908;
	public static final long Ann_Variable_Part_Type = 3909;
	public static final long Ann_Variable_Part_Value = 3910;
	public static final long Ann_Time_Indicator = 3911;
	public static final long Ann_Quota_Indicator = 3912;
	public static final long Ann_Play_Alternative = 3913;
	public static final long Ann_Language = 3914;
	public static final long Ann_Privacy_Indicator = 3915;

	public static final long Cost_Information = 423;

	public static final long Validity_Time = 448;
	public static final long Time_Quota_Threshold = 868;

	// ocs team code starts
	public static final long Ims_Information = 876;
	public static final long Ims_Calling_Party_Adress = 831;
	public static final long Ims_Called_Party_Adress = 832;
	public static final long Ims_Charging_Identifier = 1250;
	public static final long Ims_Called_Asserted_Id = 831;
	public static final long Ims_Role_Of_Node = 829;
	public static final long Ims_Node_Functionality = 862;
	public static final long Ims_Event_Type=823;
	public static final long Ims_Event_Type_Sip_Method=824;
	public static final long Ims_Event_Type_Event=825;
	public static final long Ims_Event_Type_Expires=888;
	public static final long Ims_User_Session_Id = 830;
	public static final long Ims_Time_stamps = 833;
	public static final long Ims_SIP_Request_Timestamp = 834;
	public static final long Ims_Access_Network_Info=1263;
	public static final long Ims_Inter_Operator_Identifier = 838;
	public static final long Ims_Terminating_IOI = 840;
	public static final long CC_Session_Failover = 418;
	
	//MTAS Ericssion specific
	public static final long Announcement_Instructions=1441;
	public static final long Announcement_Number=1442;
	public static final long Announcement_Inst_Order=1443;
	public static final long Announcement_Type=1448;
	


}
