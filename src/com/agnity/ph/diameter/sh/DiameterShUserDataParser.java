package com.agnity.ph.diameter.sh;

import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.codec.binary.Hex;
import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.agnity.mphdata.common.LegData;
import com.agnity.ph.common.enums.LegDataAttributes;
import org.apache.commons.codec.binary.Base64;
//import java.util.Base64;

public class DiameterShUserDataParser {

	private static Logger logger = Logger.getLogger(DiameterShUserDataParser.class);
	// private static final String FILENAME =
	// "C:\\Users\\reeta\\Desktop\\rogers\\HSS-traces\\shuserdata.xml";
	
//	<Sh-Data>
//	   <Extension>
//	      <Extension>
//	         <Extension>
//	            <Extension>
//	               <EPSLocationInformation>
//	                  <E-UTRANCellGlobalId>AwIWCG1nFg==</E-UTRANCellGlobalId>
//	                  <TrackingAreaId>AwIW4pM=</TrackingAreaId>
//	                  <MMEName>04.mmecda.mmegidac0.mme.epc.mnc610.mcc302.3gppnetwork.org</MMEName>
//	                  <AgeOfLocationInformation>0</AgeOfLocationInformation>
//	                  <Extension>
//	                     <VisitedPLMNID>302610</VisitedPLMNID>
//	                  </Extension>
//	               </EPSLocationInformation>
//	            </Extension>
//	         </Extension>
//	      </Extension>
//	   </Extension>
//	</Sh-Data>

	public static void parseUserData(String xml, LegData legdata) {

		
		if (logger.isDebugEnabled()) {
			logger.debug(" parseUserData "+ xml);
		}
		// Instantiate the Factory
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

		try {
			// parse XML file
			DocumentBuilder db = dbf.newDocumentBuilder();

			InputSource is = new InputSource();
			is.setCharacterStream(new StringReader(xml));

			Document doc = db.parse(is);// new File(FILENAME));

			doc.getDocumentElement().normalize();

			if (logger.isDebugEnabled()) {
				logger.debug("Root Element :"
						+ doc.getDocumentElement().getNodeName());
			}

			// get <staff>
			NodeList list = doc.getElementsByTagName("Extension");

			// for (int temp = 0; temp < list.getLength(); temp++) {

			// System.out.println("extension is   "+ temp);

			Node node = list.item(0);

			if (node.getNodeType() == Node.ELEMENT_NODE) {

				Element element = (Element) node;

				// get staff's attribute
				Node node1 = element.getElementsByTagName("Extension").item(0);
				Node node2 = ((Element) node1)
						.getElementsByTagName("Extension").item(0);
				Node node3 = ((Element) node2)
						.getElementsByTagName("Extension").item(0);
				// String content=node1.getTextContent();

				Element elementlast = (Element) node3;

				Element epc = (Element) elementlast.getElementsByTagName(
						"EPSLocationInformation").item(0);// .getTextContent();

				String utranCellGlobalid = null;
				String tracingAreaId =null;
				String MMEName=null;
				String ageOfLocInfo=null;
				
				if(epc.getElementsByTagName("E-UTRANCellGlobalId")!=null && epc.getElementsByTagName("E-UTRANCellGlobalId").item(0)!=null){
					String utranCellGlobalidbase64=epc.getElementsByTagName("E-UTRANCellGlobalId").item(0)
						.getTextContent();
					
					if (logger.isDebugEnabled()) {
						logger.debug(" Base64 utranCellGlobalid is  "+ utranCellGlobalidbase64);
					}
//				    byte[] decoded = Base64.getDecoder().decode(utranCellGlobalidbase64);	
//				    
//					utranCellGlobalid=toHex(decoded);
					byte[] decoded = Base64.decodeBase64(utranCellGlobalidbase64);
					utranCellGlobalid = Hex.encodeHexString(decoded); 
					if (logger.isDebugEnabled()) {
						logger.debug(" decoded utranCellGlobalid is  "+ utranCellGlobalid);
					}
				}
				if(epc.getElementsByTagName("TrackingAreaId")!=null && epc.getElementsByTagName("TrackingAreaId").item(0)!=null){
					String tracingAreaIdbase64=epc.getElementsByTagName("TrackingAreaId").item(0).getTextContent();
					
					if (logger.isDebugEnabled()) {
						logger.debug(" Base64 tracingAreaId is  "+ tracingAreaIdbase64);
					}
					byte[] decoded = Base64.decodeBase64(tracingAreaIdbase64);
					tracingAreaId = Hex.encodeHexString(decoded);
					
//					 byte[] decoded = Base64.getDecoder().decode(tracingAreaIdbase64);
//					tracingAreaId=toHex(decoded);
					if (logger.isDebugEnabled()) {
						logger.debug(" decoded tracingAreaId is  "+ tracingAreaId);
					}
				}
				
				if(epc.getElementsByTagName("MMEName")!=null&& epc.getElementsByTagName("MMEName").item(0)!=null){
				MMEName = epc.getElementsByTagName("MMEName").item(0)
						.getTextContent();
				}
				
				if(epc
						.getElementsByTagName("AgeOfLocationInformation")!=null&&epc
								.getElementsByTagName("AgeOfLocationInformation")
								.item(0)!=null){
				 ageOfLocInfo = epc
						.getElementsByTagName("AgeOfLocationInformation")
						.item(0).getTextContent();
				}

				String visitedPlmnId=null;
				
				if (epc.getElementsByTagName("Extension") != null&&epc.getElementsByTagName("Extension")
						.item(0)!=null) {
					Node nodeIntern = epc.getElementsByTagName("Extension")
							.item(0);

					Element nodeInternEPC = (Element) nodeIntern;

					if(nodeInternEPC
							.getElementsByTagName("VisitedPLMNID")!=null && nodeInternEPC
									.getElementsByTagName("VisitedPLMNID").item(0)!=null){
					visitedPlmnId = nodeInternEPC
							.getElementsByTagName("VisitedPLMNID").item(0)
							.getTextContent();
					}
				}
				
				legdata.set(LegDataAttributes.NP_UDA_EUTRAN_CELL_GLOBAL_ID, utranCellGlobalid);
				legdata.set(LegDataAttributes.NP_UDA_TRACKING_AREA_ID, tracingAreaId);
				legdata.set(LegDataAttributes.NP_UDA_MME_NAME, MMEName);
				legdata.set(LegDataAttributes.NP_UDA_AGE_OF_LOCATION_INFORMATION, ageOfLocInfo);
				legdata.set(LegDataAttributes.NP_UDA_VISITED_PLMN_ID,visitedPlmnId);
				if (logger.isDebugEnabled()) {
					logger.debug("UTRANCellGlobalId --> " + utranCellGlobalid
							+ "\nTrackingAreaId--> " + tracingAreaId
							+ "\nMMEName--> " + MMEName
							+ "\nAgeOfLocationInformation--> " + ageOfLocInfo
							+ " \nVisitedPLMNID--> " + visitedPlmnId);

				}
			}
			// }

		} catch (ParserConfigurationException | SAXException | IOException e) {
			
			String desc=ShResultCodes.getReturnCode(ShResultCodes.DIAMETER_END_USER_SERVICE_DENIED_code);
			legdata.set(LegDataAttributes.NP_UDA_RESULT_CODE, ShResultCodes.DIAMETER_END_USER_SERVICE_DENIED_code);
			legdata.set(LegDataAttributes.NP_UDA_RESULT_DESC, desc);
			
			logger.error(" parsing xception " +e);
		}

	}
	
	

//private static final char[] DIGITS= {'0', '1', '2', '3', '4', '5', '6', '7',
//            '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
//
//public static final String toHex(byte[] data) {
//    final StringBuffer sb = new StringBuffer(data.length * 2);
//    for (int i = 0; i < data.length; i++) {
//        sb.append(DIGITS[(data[i] >>> 4) & 0x0F]);
//        sb.append(DIGITS[data[i] & 0x0F]);
//    }
//    return sb.toString();
//}
}


