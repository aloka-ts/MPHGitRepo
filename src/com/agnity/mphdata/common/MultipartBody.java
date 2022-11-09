/*******************************************************************************
 *   Copyright (c) 2011 Agnity, Inc. All rights reserved.
 *   
 *   This is proprietary source code of Agnity, Inc. 
 *   
 *   Agnity, Inc. retains all intellectual property rights associated 
 *   with this source code. Use is subject to license terms.
 *   
 *   This source code contains trade secrets owned by Agnity, Inc.
 *   Confidentiality of this computer program must be maintained at 
 *   all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.mphdata.common;

import java.io.Serializable;

/**
 * This class is used to process multi - part body received in SIP messages 
 * @author sgoswami
 */
public class MultipartBody implements Serializable {

	private static final long	serialVersionUID	= -8407657907040120799L;

	private String				contentType;
	private String				contentDisposition;
	private byte[]				content;

	/**
	 * @param content
	 * @param contentType
	 */
	public MultipartBody(byte[] content, String contentType) {
		this.content = content;
		this.contentType = contentType;
	}


	/**
	 * This method returns the value of content type header
	 * 
	 */
	public String getContentType() {
		return contentType;
	}

	/**
	 * This method sets content type header
	 * @param contentType
	 */
	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	/**
	 * This methods returns the byte array contained in MIME
	 * @return
	 */
	public byte[] getContent() {
		return content;
	}

	/**
	 * This method sets the MIME Body
	 * @param content
	 */
	public void setContent(byte[] content) {
		this.content = content;
	}

	/**
	 * This Method returns the value of content disposition header
	 */
	public String getContentDisposition() {
		return contentDisposition;
	}

	/**
	 * This Method sets the content disposition header
	 * @param contentDisposition
	 */
	public void setContentDisposition(String contentDisposition) {
		this.contentDisposition = contentDisposition;
	}

}
