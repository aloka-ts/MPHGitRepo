package com.agnity.ph.diameter;

import java.math.BigInteger;

import org.apache.log4j.Logger;

public class DiameterAVPAttribute {

	
	private static Logger logger = Logger
			.getLogger(DiameterAVPAttribute.class);
	String name = null;
	DiameterAVPType type = null;
	Object value = null;
	String vendorName = "3gpp";

	long avpCode;

	long vendorid;

	public DiameterAVPAttribute(String name, String vendorName,DiameterAVPType type) {
		this.name = name;
		this.vendorName = vendorName;
		this.type=type;
	}

	public DiameterAVPAttribute(long avpCode, long vendorid,DiameterAVPType type) {
		this.avpCode = avpCode;
		this.vendorid = vendorid;
		this.type=type;
	}

//	public void setType(DiameterAVPType type) {
//		this.type = type;
//	}
	
	public void setValue(Object value) {

		if(logger.isDebugEnabled()){
			logger.debug("setValue " + value  +" type is "+type);
		}
		if (value instanceof String) {
			switch (this.type) {
			case Integer32: {
				this.value = Integer.parseInt((String) value);
			}
				break;
			case Integer64: {
				this.value = Long.parseLong((String) value);
			}
				break;
			case Float32: {
				this.value = Float.parseFloat((String) value);
			}
				break;
			case Float64: {
				this.value = Double.parseDouble((String) value);
			}
				break;
			case Unsigned32: {
				this.value = Long.parseLong((String) value);
			}
				break;
			case Unsigned64: {
				this.value = BigInteger.valueOf(Long.parseLong((String) value));
			}
				break;
			case OctetString: {
				this.value = value;
			}
				break;
			case OctetString_byte: {
				this.value = Integer.parseInt((String) value);
			}
			}
		}else{
			this.value=value;
		}

		if(logger.isDebugEnabled()){
			logger.debug("setValue value set is "+ this.value);
		}
	}
	
	
	public String getName() {
		return this.name;
	}

	public DiameterAVPType getType() {
		return this.type;
	}
	
	public Object getValue() {
		return this.value;
	}

	public String getVendorName() {
		return this.vendorName;
	}

	public long getAvpCode() {
		return this.avpCode;
	}

	public long getVendorid() {
		return this.vendorid;
	}
	
	@Override
	public String toString(){
		return "DiameterAVPAttribute [name=" + getName()
				+ ", type()=" + getType() + ", vendor=" + getVendorName() + ", value=" + getValue() + ", vendorId=" + getVendorid()+"]";
	}

}
