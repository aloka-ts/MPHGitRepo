package com.agnity.ph.diameter;

public interface DiameterAVPNames {
	
	public static final String AoC_Information="AoC-Information";
	public static final String AoC_Cost_Information="AoC-Cost-Information";
	public static final String Mutil_Service_Credit_Control="Multiple-Services-Credit-Control";
	public static final String Requested_Service_Unit="Requested-Service-Unit";
	public static final String Granted_Service_Unit="Granted-Service-Unit";
	public static final String Used_Service_Unit="Used-Service-Unit";
	public static final String Service_Identifier="Service-Identifier";
	public static final String Final_Unit_Indication="Final-Unit-Indication";
	public static final String CC_Money="CC-Money";
	
	public static final String CC_Money_Unit_Value="Unit-Value";
	public static final String Announcement_Information = "Announcement-Information";
	public static final String Cost_Information="Cost-Information";
	public static final String Credit_Control_Failure_Handling ="Credit-Control-Failure-Handling";
	public static final String Direct_Debiting_Failure_Handling ="Direct-Debiting-Failure-Handling";
	public static final String Validity_Time="Validity-Time"; 
	public static final String  Time_Quota_Threshold="Time-Quota-Threshold";
	
    public static final String Event_Timestamp="Event-Timestamp";
	public static final String Destination_Host="Destination-Host";
	public static final String Service_Context_Id=" Service-Context-Id";
	public static final String User_Name="User-Name";
	public static final String Origin_State_Id="Origin-State-Id";
	public static final String Service_Information="Service-Information";
	
	public static final String Subscription_Id="Subscription-Id";
	public static final String Subscription_Id_Type="Subscription-Id-Type";
	public static final String Subscription_Id_Data="Subscription-Id-Data";

//	 Service-Context-Id // CCR initial - done pls review
//	// 3. Event-Timestamp
//	// 4. Destination-Host
//	 5. User-Name
//	 6. Origin-State-Id
//	 7. service-Information
	
	//ocs team for service-Info
	public static final String Ims_Information="Ims-Information";
	public static final String Calling_Party_Adress="Calling-Party-Adress";
	public static final String Called_Party_Adress="Called-Party-Adress";
	public static final String Ims_Charging_Identifier="Ims-Charging-Identifier";
	public static final String Called_Asserted_Id="Called-Asserted-Id";
	public static final String Role_Of_Node="Role-Of-Node";
	public static final String Node_Functionality="Node-Functionality";
	public static final String CC_Session_Failover = "CC-Session-Failover";
	
	
	public static final String Announcement_Instructions="Announcement-Instructions";
	public static final String Announcement_Number="Announcement-Number";
	public static final String Announcement_Order="Announcement-Order";
	public static final String Announcement_Type="Announcement-Type";
	public static final String Auth_Application_Id = "Auth-Application-Id";
	public static final String Multiple_Services_Indicator="Multiple-Services-Indicator";
	public static final String Termination_Cause = "Termination-Cause";
	
	//used for sh
	public static final String Vendor_Specific_Application_Id="Vendor-Specific-Application-Id";
	public static final String Vendor_id="Vendor-Id";
	
	

}
