package com.agnity.mphdata.common;

import java.io.Serializable;
import java.util.Map;

import javax.servlet.sip.Address;
import javax.servlet.sip.URI;

public class InviteAttributes implements Serializable {
    private static final long serialVersionUID = -8950386471372036222L;

    Address to;
    Address from;
    URI requestURI;
    
    public Map<String, String> getCustomHeaders() {
		return customHeaders;
	}

	public void setCustomHeaders(Map<String, String> customHeaders) {
		this.customHeaders = customHeaders;
	}

	Map<String, String> customHeaders;

    public InviteAttributes() {
    }

    public InviteAttributes(Address to, Address from, URI requestURI) {
        this.to = to;
        this.from = from;
        this.requestURI = requestURI;
    }

    public Address getTo() {
        return to;
    }

    public void setTo(Address to) {
        this.to = to;
    }

    public Address getFrom() {
        return from;
    }

    public void setFrom(Address from) {
        this.from = from;
    }

    public URI getRequestURI() {
        return requestURI;
    }

    public void setRequestURI(URI requestURI) {
        this.requestURI = requestURI;
    }

    @Override
    public String toString() {
        return "InviteAttributes [to=" + to + ", from=" + from
                + ", requestURI=" + requestURI + "]";
    }


}
