package com.agnity.ph.http;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.net.URL;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileItemFactory;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.log4j.Logger;

import com.baypackets.ase.util.AseStrings;

public class HttpEvent implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static final String EVENT_HTTP_GET = "GET".intern();
	public static final String EVENT_HTTP_POST = "POST".intern();
	public static final String EVENT_HTTP_PUT = "PUT".intern();

	private HttpServletRequest httpRequest;
	private HttpServletResponse httpResponse;
	private SipApplicationSession appSession;
	private ServletContext sContext;
	private String reqURI;
	private int remotePort = -1;
	private Date reqTimeStamp;
	private Date respTimeStamp;
	private String reqContentType=null;
	
	public void setReqContentType(String reqContentType) {
		this.reqContentType = reqContentType;
	}

	private String leg;

	public String getLeg() {
		return leg;
	}

	public void setLeg(String leg) {
		this.leg = leg;
	}

	private String respContentType;
	private int respCode=-1;

	public int getRespCode() {
		return respCode;
	}

	public String getRespContentType() {
		return respContentType;
	}

	public void setRespContentType(String respContentType) {
		this.respContentType = respContentType;
	}

	public String getRespContent() {
		return respContent;
	}

	public void setRespContent(String respContent) {
		this.respContent = respContent;
	}

	private String respContent;

	public Date getRespTimeStamp() {
		return respTimeStamp;
	}

	public void setRespTimeStamp(Date respTimeStamp) {
		this.respTimeStamp = respTimeStamp;
	}

	private String content = "null";
	private Map<String, List<String>> headersMap;

	private String remoteHost = null;

	public String getRemoteHost() {
		return remoteHost;
	}

	public void setRemoteHost(String remoteHost) {
		this.remoteHost = remoteHost;
	}

	public int getRemotePort() {
		return remotePort;
	}

	public void setRemotePort(int remotePort) {
		this.remotePort = remotePort;
	}

	public Date getReqTimeStamp() {
		return reqTimeStamp;
	}

	public void setReqTimeStamp(Date eventTimeStamp) {
		this.reqTimeStamp = eventTimeStamp;
	}

	public ServletContext getsContext() {
		return sContext;
	}

	public void setsContext(ServletContext sContext) {
		this.sContext = sContext;
	}

	private static Logger logger = Logger.getLogger(HttpEvent.class);

	private String eventId;
	private String sessionId;
	private String sender;

	public String getSender() {
		return sender;
	}

	public String getSessionId() {
		return sessionId;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public SipApplicationSession getAppSession() {
		return appSession;
	}

	public void setAppSession(SipApplicationSession appSession) {
		this.appSession = appSession;
	}

	public HttpServletRequest getHttpRequest() {
		return httpRequest;
	}

	public void setHttpRequest(HttpServletRequest httpRequest) {
		this.httpRequest = httpRequest;
	}

	public HttpServletResponse getHttpResponse() {
		return httpResponse;
	}

	public void setHttpResponse(HttpServletResponse httpResponse) {
		this.httpResponse = httpResponse;
	}

	public HttpEvent() {
		super();
	}

	public HttpEvent(String id) {
		this.eventId = id;
	}

	public void setEventId(String id) {
		eventId = id;
	}

	public String getEventId() {
		return eventId;
	}

	/**
	 * This method is used to load dala from post request
	 */
	public void loadData() {

		if (logger.isDebugEnabled()) {
			logger.debug("Inside loadData ...");
		}

		HttpServletRequest req = this.httpRequest;
		
		reqContentType=req.getContentType();
		
		org.apache.commons.fileupload.servlet.ServletFileUpload sfu = new ServletFileUpload();

		boolean isMultipart = ServletFileUpload.isMultipartContent(req);

		if (!isMultipart) {

			if (logger.isDebugEnabled()) {
				logger.debug("Not  mltipart type...");
			}

			try {
				String body = getBody(req);
				this.setContent(body);

			} catch (IOException e) {

				if (logger.isDebugEnabled()) {
					logger.debug("loadData() xception thrown ..." + e);
				}

				e.printStackTrace();
			}

		} else {

			if (logger.isDebugEnabled()) {
				logger.debug("Upload attached data to this HTTP request...");
			}

			File savedFile = null;
			FileItemFactory factory = new DiskFileItemFactory();
			ServletFileUpload upload = new ServletFileUpload(factory);
			List<?> items = null;

			try {
				items = upload.parseRequest(req);

				if (logger.isDebugEnabled()) {
					logger.debug("Content items parsed from request are  ..."
							+ items);
				}

			} catch (FileUploadException e) {
				logger.error(e.getMessage(), e);
			}

			Iterator<?> itr = items.iterator();

			while (itr.hasNext()) {
				FileItem item = (FileItem) itr.next();

				if (item.isFormField()) {

					if (logger.isDebugEnabled()) {
						logger.debug("item is of form type No File to upload ...");
					}

					String name = item.getFieldName();
					String value = item.getString();
					req.setAttribute(name, value);

					if (logger.isDebugEnabled()) {
						logger.debug("Attribute Name is :" + name + " Value : "
								+ value);
					}

				} else {

					if (logger.isDebugEnabled()) {
						logger.debug("File upload  from post data ...");
					}
					try {

						String itemName = item.getName();
						String name = item.getFieldName();

						if (logger.isDebugEnabled()) {
							logger.debug("Data file name ." + name
									+ " Path is " + itemName);
						}

						String ext = itemName.substring(itemName
								.lastIndexOf(AseStrings.PERIOD) + 1);

						if (logger.isDebugEnabled()) {
							logger.debug("File EXT is " + ext);
						}
						byte[] b = item.get();

						String webInf = this.sContext.getResource("/WEB-INF")
								.getPath();

						if (logger.isDebugEnabled()) {
							logger.debug("WEB-INF path is " + webInf);
						}
						savedFile = new File(webInf, "recordings");
						savedFile.mkdir();
						savedFile = new File(savedFile, name
								+ AseStrings.PERIOD + ext);

						FileOutputStream fos = new FileOutputStream(savedFile);

						fos.write(b);

						req.setAttribute(name, savedFile.getAbsolutePath());

						if (logger.isDebugEnabled()) {
							logger.debug("File uploaded " + savedFile
									+ "File name is " + name);
						}

					} catch (Exception e) {
						logger.error(e.getMessage(), e);
					}
				}
			}// while end

		} // else end

		if (logger.isDebugEnabled()) {
			logger.debug("Leaving loadData ...");
		}
	}

	public void postVxmlFile(String url) {

		if (logger.isDebugEnabled()) {
			logger.debug("<SBB> The vxml file url is..." + url);
		}

		BufferedInputStream bis = null;
		ServletOutputStream out = null;

		try {

			if (url.startsWith("http") || url.startsWith("file")) {

				URL url1 = new URL(url);
				// HttpURLConnection urlCon = (HttpURLConnection)
				// url1.openConnection ();
				// InputStream fis = urlCon.getInputStream();

				InputStream fis = url1.openStream();
				bis = new BufferedInputStream(fis);

				httpResponse.setContentType("text/xml");

			} else {

				// Load the VXML file
				File vxml = new File(url);
				FileInputStream fis = new FileInputStream(vxml);

				bis = new BufferedInputStream(fis);

				// Let the browser know that XML is coming

				httpResponse.setContentType("text/xml");
				httpResponse.setContentLength((int) vxml.length());

			}
			out = httpResponse.getOutputStream();
			// Output the VXML file
			int readBytes = 0;

			while ((readBytes = bis.read()) != -1) {
				// output the VXML
				out.write(readBytes);
			}

		} catch (FileNotFoundException e) {
			logger.error(e.getMessage(), e);
		} catch (IOException e) {
			logger.error(e.getMessage(), e);
		} finally {
			if (out != null)
				try {
					out.close();
					if (bis != null)
						bis.close();
				} catch (IOException e) {
					logger.error(e.getMessage(), e);
				}

		}
	}

	public String getParameter(String name) {
		if (httpRequest != null) {
		return this.httpRequest.getParameter(name);
		}else return null;
	}

	public Enumeration getParameterNames() {
		if (httpRequest != null) {
		return this.httpRequest.getParameterNames();
		} else 
			return null;
	}

	public String[] getParameterValues(String name) {
		if (httpRequest != null) {
		return this.httpRequest.getParameterValues(name);
		}
		else return null;
	}

	public Object getAttribute(String name) {
		if (httpRequest != null) 
		return this.httpRequest.getAttribute(name);
		else return null;
	}

	public void setAttribute(String name, Object value) {
		this.httpRequest.setAttribute(name, value);
	}

	public Enumeration getAttributeNames() {

		return this.httpRequest.getAttributeNames();
	}

	/**
	 * parse payload from http request
	 * 
	 * @param request
	 * @return
	 * @throws IOException
	 */
	public static String getBody(HttpServletRequest request) throws IOException {

		String body = null;
		BufferedReader reader = null;

		if (logger.isDebugEnabled()) {
			logger.debug("Inside getBody() ...");
		}

		try {
			// Read from request
			StringBuilder buffer = new StringBuilder();
			reader = request.getReader();
			String line;
			while ((line = reader.readLine()) != null) {
				buffer.append(line + "\n");
			}
			body = buffer.toString();
		} catch (IOException ex) {
			throw ex;
		} finally {
			if (reader != null) {
				try {
					reader.close();
				} catch (IOException ex) {
					throw ex;
				}
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug("leaving getBody() ... with " + body);
		}
		return body;
	}

	public InputStream getInputStream() {

		InputStream stream = null;
		try {
			stream = this.httpRequest.getInputStream();
		} catch (IOException e) {
			logger.error(e.getMessage(), e);
		}
		return stream;
	}

	public int getContentLength() {
		
		if (httpRequest != null) {
		return this.httpRequest.getContentLength();
		}
		return 0;
	}

	public String getContentType() {
		
		if (logger.isDebugEnabled()) {
			logger.debug("Return content type as " + reqContentType);
		}
		return reqContentType;
	}

	public HttpSession getSession() {
		
		if (httpRequest != null) {
		return this.httpRequest.getSession();
		}
		return null;
	}

	public Map getParameterMap() {

		if (httpRequest != null) {
			return httpRequest.getParameterMap();
		}
		return null;
	}

	public void setRequestURL(String requestURI) {
		this.reqURI = requestURI;

	}

	public String getReqURI() {
		return reqURI;
	}

	public void setHeadersMap(Map<String, List<String>> map) {
		this.headersMap = map;

	}

	public Map<String, List<String>> getHeadersMap() {
		return headersMap;
	}

	public void setRespCode(int responseCode) {
		this.respCode=responseCode;
		
	}

	public void setSessionId(String callId) {
		sessionId=callId;
		
	}

	public void setSender(String sender) {
		// TODO Auto-generated method stub
		this.sender=sender;
		
	}
}