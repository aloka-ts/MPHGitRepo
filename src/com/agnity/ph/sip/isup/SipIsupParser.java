package com.agnity.ph.sip.isup;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import com.genband.isup.datatypes.*;
import com.genband.isup.enumdata.*;
import org.apache.log4j.Logger;

import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.SignalingTypeEnum;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.SipProtocolConfig;
import com.agnity.ph.sip.SipProtocolUtil;
import com.genband.isup.exceptions.InvalidInputException;
import com.genband.isup.messagetypes.ACMMessage;
import com.genband.isup.messagetypes.ANMMessage;
import com.genband.isup.messagetypes.CHGMessage;
import com.genband.isup.messagetypes.CPGMessage;
import com.genband.isup.messagetypes.IAMMessage;
import com.genband.isup.messagetypes.RELMessage;
import com.genband.isup.messagetypes.RLCMessage;
import com.genband.isup.operations.ISUPConstants;
import com.genband.isup.operations.ISUPOperationsCoding;

/**
 * This class is used to parse and create incoming and outgoing ISUP messages
 * @author reeta
 *
 */
public class SipIsupParser {
	
	/**
	 * This method is called by protocol handler to create ISUP ACM message.  
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createAcm(CallData callData) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside createAcmForB2bua");
		}

		ACMMessage acmMessage = new ACMMessage();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Set 6 as ACM Message Type");
		}

		/*
		 *  Message Type
		 */
		acmMessage.setMessageType(new byte[] { 0x06 });

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Set BackwardCallIndicators in ACM Message");
		}

		/*
		 *  Backward Call Indicator
		 */
		acmMessage
			.setBackwardCallIndicators(BwCallIndicators.encodeBwCallInd(
							ChargeIndEnum.NO_INDICATION, CalledPartyStatusIndEnum.NO_INDICATION,
							CalledPartyCatIndEnum.NO_INDICATION,
							EndToEndMethodIndEnum.NO_END_METHOD, InterNwIndEnum.NO_INTER_NW_ENC,
							EndToEndInfoIndEnum.NO_END_INFO_AVAILABLE,
							ISDNUserPartIndEnum.ISDN_USER_PART_USED,
							HoldingIndEnum.HOLDING_NOT_REQUESTED, ISDNAccessIndEnum.ISDN,
							EchoContDeviceIndEnum.DEVICE_NOT_INCLUDED,
							SCCPMethodIndENum.NO_INDICATION));

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
								+ ":: Set Optional BackwardCallIndicators in ACM Message");
			}

			/*
			 *  Optional Backward Call Indicator
			 */
			acmMessage.setOptBwCallIndicators(OptBwCallIndicators
				.encodeOptBwCallInd(InbandInfoIndEnum.INBAND_INFO,
								CallDiversionIndEnum.NO_INDICATION,
								SimpleSegmentationIndEnum.NO_INFORMATION,
								MLPPUserIndEnum.NO_INDICATION));

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set ttcChargingAreaInfo in ACM Message");
			}


		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(acmMessage);
		opCode.add(ISUPConstants.OP_CODE_ACM);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler for creating ANM for media interaction.
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createAnm(CallData callData,ChargeIndEnum chargeEnum) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside createAnmForMediaInteraction");
		}

		ANMMessage anmMessage = new ANMMessage();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Set 9 as ANM Message Type");
		}

		/*
		 *  Message Type
		 */
		anmMessage.setMessageType(new byte[] { 0x09 });

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Set Backward Call Indicators in ANM Message");
		}

		/*
		 *  Backward Call Indicator
		 */
		anmMessage
			.setBackwardCallIndicators(BwCallIndicators.encodeBwCallInd(chargeEnum,//ChargeIndEnum.CHARGE//NO_CHARGE
							CalledPartyStatusIndEnum.NO_INDICATION,
							CalledPartyCatIndEnum.ORDINARY_SUBSCRIBER,
							EndToEndMethodIndEnum.NO_END_METHOD, InterNwIndEnum.NO_INTER_NW_ENC,
							EndToEndInfoIndEnum.NO_END_INFO_AVAILABLE,
							ISDNUserPartIndEnum.ISDN_USER_PART_USED,
							HoldingIndEnum.HOLDING_NOT_REQUESTED, ISDNAccessIndEnum.ISDN,
							EchoContDeviceIndEnum.DEVICE_NOT_INCLUDED,
							SCCPMethodIndENum.NO_INDICATION));

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(anmMessage);
		opCode.add(ISUPConstants.OP_CODE_ANM);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}
	/**
	 * This method is called by protocol handler for creating Rel with cause value.
	 * @param callData represents an instance of CallData
	 * @param causeValue represents integer equivalent of cause value
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createRelWithCauseValue(int causeValue, CallData callData)
					throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside createRelWithCauseValue");
		}

		RELMessage relMessage = new RELMessage();
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Set 12 as REL Message Type");
		}

		/*
		 *  Message Type
		 */
		relMessage.setMessageType(new byte[] { 0x0c });

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Set cause in REL Message with cause value "
							+ causeValue);
		}

		/*
		 *  Cause
		 */
		LocationEnum locationEnum = LocationEnum.TRANSIT_NETWORK;

		byte[] cause = Cause.encodeCauseVal(locationEnum, CodingStndEnum.ITUT_STANDARDIZED_CODING,
						CauseValEnum.fromInt(causeValue));
		relMessage.setCause(cause);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: locationEnum is " + locationEnum);
		}
		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(relMessage);
		opCode.add(ISUPConstants.OP_CODE_REL);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}
	/**
	 * This method is called by protocol handler for creating ISUP RLC message.
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createRlc(CallData callData) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside createRlc");
		}

		RLCMessage rlcMessage = new RLCMessage();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Set 16 as RLC Message Type");
		}

		/*
		 *  Message Type
		 */
		rlcMessage.setMessageType(new byte[] { 0x10 });

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(rlcMessage);
		opCode.add(ISUPConstants.OP_CODE_RLC);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}
	/**
	 * This method is called by protocol handler for parsing REL message and return an instance of 
	 * RELMessage.
	 * @param callData represents an instance of CallData
	 * @param relByteArr represents an instance of byte[]
	 * @param populateCallData represents a boolean flag
	 * @return an instance of RELMessage
	 * @throws InvalidInputException
	 */
	public static RELMessage parseRel(CallData callData, byte[] relByteArr, boolean populateCallData)
					throws InvalidInputException {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside parseRel");
		}

		LinkedList<byte[]> byteArrList = new LinkedList<byte[]>();
		byteArrList.add(relByteArr);

		LinkedList<String> opCodeList = new LinkedList<String>();
		opCodeList.add(ISUPConstants.OP_CODE_REL);

		LinkedList<Object> decodeList = ISUPOperationsCoding.decodeOperations(byteArrList,
						opCodeList);
		RELMessage relMessage = (RELMessage) decodeList.getFirst();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: REL Message is " + relMessage);
		}

		if (populateCallData) {
			/*
			 *  Populate Call Data from IAM
			 */
			int cause = relMessage.getCause().getCauseValEnum().getCode();

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Extracted REL Cause Code is " + cause);
			}
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,cause);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE,cause);
			callData.set(CallDataAttribute.P_ISUP_CAUSE_CODE, cause);
			
			callData.set(CallDataAttribute.NP_RELEASE_LOCATION ,relMessage.getCause().getLocEnum().getCode());
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Extracted REL Location is "
								+ relMessage.getCause().getLocEnum().getCode());
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: REL message parsed successfully");
		}

		return relMessage;
	}

	/**
	 * This method is called by protocol handler for retrieving OLEC from TtcCarrierInfoTrfr 
	 * of ISUP payload.
	 * @param TtcCarrierInfoTrfr represents an instance of TtcCarrierInfoTrfr
	 * @param origLegCallID represents an instance of String
	 * @return integer value of OLEC
	 */
	private static int getOlecCicFromTtcCarrierInfoTrfr(TtcCarrierInfoTrfr TtcCarrierInfoTrfr,
					String origLegCallID) {

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallID + ":: Inside getOlecCicFromTtcCarrierInfoTrfr");
		}
		int olecCic = 0;

		LinkedList<CarrierInformation> carrierInfoList = TtcCarrierInfoTrfr.getCarrierInformation();
		Iterator<CarrierInformation> carrierInfoIter = carrierInfoList.iterator();
		CarrierInformation carrierInfo = null;
		OlecSearch: while (carrierInfoIter.hasNext()) {
			carrierInfo = carrierInfoIter.next();
			if (carrierInfo.getCarrierInfoNameEnum() == CarrierInfoNameEnum.OLEC) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallID + ":: OLEC found in TtcCarrierInformationTransfer");
				}

				LinkedList<CarrierInfoSubordinate> carrierInfoSubList = carrierInfo
					.getCarrierInfoSubordinate();
				Iterator<CarrierInfoSubordinate> carrierInfoSubIter = carrierInfoSubList.iterator();
				CarrierInfoSubordinate carrierInfoSub = null;
				while (carrierInfoSubIter.hasNext()) {
					carrierInfoSub = carrierInfoSubIter.next();
					if (carrierInfoSub.getCarrierInfoSubordinateEnum() == CarrierInfoSubordinateEnum.CARRIER_IDENT_CODE) {
						olecCic = Integer.parseInt(carrierInfoSub.getCarrierIdentificationCode()
							.getCarrierIdentCode());

						if (logger.isDebugEnabled()) {
							logger
								.debug(origLegCallID
												+ ":: OLEC-CIC value in TtcCarrierInformationTransfer is "
												+ olecCic);
						}

						break OlecSearch;
					}
				}
			}
		}
		return olecCic;
	}
	/**
	 * This method is called by protocol handler for parsing GenericNumber.
	 * @param origLegCallId represents an instance of String
	 * @param genericNum represents an instance of GenericNumber
	 * @return an instance of PhoneNumber
	 */
	private static PhoneNumber parseGenericNumber(String origLegCallId, GenericNumber genericNum) {

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside parseGenericNumber");
		}

		PhoneNumber genericNumber = new PhoneNumber();

		// Nature Of Address
		NatureOfAddEnum natureOfAddrEnum = genericNum.getNatureOfAdrs();
		if ((natureOfAddrEnum == NatureOfAddEnum.SPARE)
						|| (natureOfAddrEnum == NatureOfAddEnum.SUBS_NO)
						|| (natureOfAddrEnum == NatureOfAddEnum.NETWORK_SPEC_NO)
						|| (natureOfAddrEnum == NatureOfAddEnum.NATIONAL_NO)) {
			genericNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == NatureOfAddEnum.UNKNOWN) {
			genericNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if (natureOfAddrEnum == NatureOfAddEnum.INTER_NO) {
			genericNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			genericNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Extracted Nature of Address is "
							+ natureOfAddrEnum.getCode());
		}

		/*
		 *  Numbering Plan Indicator
		 */
		NumPlanEnum numPlanIndEnum = genericNum.getNumPlan();
		if (numPlanIndEnum == NumPlanEnum.PRIVATE_NP) {
			genericNumber.setNumberingPlan(PhoneNumber.NP_PRIVATE);
		} else if (numPlanIndEnum == NumPlanEnum.ISDN_NP) {
			genericNumber.setNumberingPlan(PhoneNumber.NP_ISDN);
		} else {
			genericNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Extracted Numbering Plan is "
							+ natureOfAddrEnum.getCode());
		}

		/*
		 *  Address Presentation Restricted Indicator
		 */
		int addrPresentIndicator = genericNum.getAdrsPresntRestd().getCode();
		genericNumber.setPresentationIndicator(addrPresentIndicator);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Extracted Presentation Indicator is "
							+ addrPresentIndicator);
		}

		/*
		 *  Address
		 */
		String addrSignal = genericNum.getAddrSignal();
		genericNumber.setAddress(addrSignal);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Extracted Address Signal is " + addrSignal);
		}

		return genericNumber;
	}
	/**
	 * This method is called by protocol handler for parsing IAM from ISUP payload and
	 * keeps the relevant information into call data instance based on boolean flag passed.
	 * @param callData represents an instance of CallData
	 * @param iamByteArr represents an instance of byte[]
	 * @param populateCallData represents boolean flag 
	 * @return an instance of IAMMessage
	 * @throws InvalidInputException
	 */	
	public static IAMMessage parseIam(CallData callData, byte[] iamByteArr, boolean populateCallData)
					throws InvalidInputException {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside parseIam");
		}

		LinkedList<byte[]> byteArrList = new LinkedList<byte[]>();
		byteArrList.add(iamByteArr);

		LinkedList<String> opCodeList = new LinkedList<String>();
		opCodeList.add(ISUPConstants.OP_CODE_IAM);

		LinkedList<Object> decodeList = ISUPOperationsCoding.decodeOperations(byteArrList,
						opCodeList);
		IAMMessage iamMessage = (IAMMessage) decodeList.getFirst();
		callData.set(CallDataAttribute.P_ISUP_LEG1_IAM_MESSAGE, iamMessage);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: IAM Message is " + iamMessage);
		}

		if (populateCallData) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Fill Call Data with the IAM information");
			}

			/*
			 *  OLEC-CIC
			 */
			int olecCic = 0;
			if (iamMessage.getCarrierInfoTransferBytes() != null) {

				olecCic = getOlecCicFromTtcCarrierInfoTrfr(iamMessage.getCarrierInfoTransfer(),
						origLegCallId);

				legData.set(LegDataAttributes.P_ISUP_OLEC_CIC, olecCic);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted OLEC-CIC from IAM is " + olecCic);
				}
				
				LegData origLegData = SipProtocolUtil.getLegDataForConnectionType(null,
						ConnectionType.ORIG_CONNECTION, callData);

				if (origLegData.get(LegDataAttributes.P_CALLING_PARTY) != null) {
					((PhoneNumber)callData.get(CallDataAttribute.P_CALLING_PARTY)).setLocalExCarrier(olecCic);
				}
				if (origLegData.get(LegDataAttributes.P_CONTRACTOR_NUMBER) != null) {
					((PhoneNumber)callData.get(CallDataAttribute.P_CONTRACTOR_NUMBER)).setLocalExCarrier(olecCic);
				}
			}

			/*
			 *  Charge Area Information
			 */
			if (olecCic > 0 && iamMessage.getChargeAreaInformationBytes() != null) {
				TtcChargeAreaInfo ttcChargeArea = iamMessage.getChargeAreaInformation();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted ttcChargeArea from IAM is "
									+ ttcChargeArea);
				}

				if (ttcChargeArea != null) {
					callData.set(CallDataAttribute.P_CHARGE_NUMBER,ttcChargeArea.getAddrSignal());

					legData.set(LegDataAttributes.P_ISUP_CHARGING_AREA, ttcChargeArea.getAddrSignal());

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Charge Area is "
										+ ttcChargeArea.getAddrSignal());
					}
				}
			}

			/*
			 *  Generic Number
			 */
			if (iamMessage.getGenericNumberBytes() != null) {
				GenericNumber genNumber = iamMessage.getGenericNumber();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted Generic Number from IAM is "
									+ genNumber);
				}

				if (genNumber != null) {
					callData.set(CallDataAttribute.P_GENERIC_NUM,parseGenericNumber(origLegCallId,
									genNumber));
				}
			}

			/*
			 * CalledIN Number
			 */
			if (iamMessage.getCalledINNumberBytes() != null) {
				TtcCalledINNumber ttcCalledINNumber = iamMessage.getCalledINNumber();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted calledINNumber  from IAM is "
									+ ttcCalledINNumber);
				}

				if (ttcCalledINNumber != null) {
					callData.set(CallDataAttribute.P_TTC_CALLED_IN_NUMBER,ttcCalledINNumber.getAddrSignal());
				}
			}
			
			if (iamMessage.getCallingPartyCategoryBytes()!= null) {
				CalgPartyCatgEnum callingPartyCatNumber = iamMessage.getCallingPartyCategory();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted Calling party category  from IAM is "
									+ callingPartyCatNumber);
				}

				if (callingPartyCatNumber != null) {
					
					String cpc=getCpcFromEnum(origLegCallId,callingPartyCatNumber);
					
					String setOliAsCpc = SipProtocolConfig
							.getConfigData(SipProtocolConfig.SET_OLI_AS_CPC);
					
					if (setOliAsCpc != null
							&& setOliAsCpc.equals(PhConstants.TRUE)) {
						
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + ":: Extracted cpc from IAM is "
											+ cpc + " setting this is P_ORIG_LINE_INFO call data attribute");
						}

						callData.set(CallDataAttribute.P_ORIG_LINE_INFO,
								cpc);
					}
					
					callData.set(CallDataAttribute.P_ISUP_CPC,
							cpc);
					legData.set(LegDataAttributes.P_ISUP_CPC, cpc);
				}
			}

			/*
			 * Transmission Medium Requirement
			 */
			if (iamMessage.getTmrBytes() != null) {
				TransmissionMedReqEnum transMedReqEnum = iamMessage.getTmr();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted transMedReqEnum from IAM is "
									+ transMedReqEnum);
				}

				if (transMedReqEnum != null) {
					callData.set(CallDataAttribute.P_TMR, transMedReqEnum.getCode());

					legData.set(LegDataAttributes.P_ISUP_BEARER_CAP, transMedReqEnum.getCode());

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: TMR is " + callData.get(CallDataAttribute.P_TMR));
					}
				}
			}

			/*
			 * Additional Party's category
			 */
			if (iamMessage.getAdditionalPartyCategoryBytes() != null) {
				LinkedList<AdditionalPartyCatPair> addPartyCatPairList = iamMessage
					.getAdditionalPartyCat();
				Iterator<AdditionalPartyCatPair> addPartyCatPairIter = addPartyCatPairList
					.iterator();
				AdditionalPartyCatPair addPartyCatPair = null;
				while (addPartyCatPairIter.hasNext()) {
					addPartyCatPair = addPartyCatPairIter.next();
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
										+ ":: Extracted Additional Party Category Pair is "
										+ addPartyCatPair);
					}

					if (addPartyCatPair.getMobileCategory1Field() != null) {
						callData.set(CallDataAttribute.P_ADD_CPC_1,addPartyCatPair.getMobileCategory1Field().getCode());

						legData.set(LegDataAttributes.P_ISUP_ADD_CLG_PTY_CAT1,
								addPartyCatPair.getMobileCategory1Field().getCode());

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
											+ ":: Mobile Additional Calling Party Category 1 is "
											+ addPartyCatPair.getMobileCategory1Field().getCode());
						}
					} else if (addPartyCatPair.getMobileCategory2Field() != null) {
						callData.set(CallDataAttribute.P_ADD_CPC_2,addPartyCatPair.getMobileCategory2Field()
							.getCode());

						legData.set(LegDataAttributes.P_ISUP_ADD_CLG_PTY_CAT2,
								addPartyCatPair.getMobileCategory2Field().getCode());

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
											+ ":: Mobile Additional Calling Party Category 2 is "
											+ addPartyCatPair.getMobileCategory2Field()
											.getCode());
						}

					} else if (addPartyCatPair.getPSTNCategoryField() != null) {
						callData.set(CallDataAttribute.P_ADD_PSTNC,addPartyCatPair.getPSTNCategoryField()
							.getCode());

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
											+ ":: PSTN Additional Calling Party Category 1 is "
											+ callData.get(CallDataAttribute.P_ADD_PSTNC));
						}
					}
				}
			}

			/*
			 *  DPC Information
			 */
			if (iamMessage.getDpcInfoBytes() != null) {
				MemberStatusIndEnum memberStatusIndEnum = iamMessage.getDPCInfo()
					.getMemberStatusInd();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted Member Status Ind Enum is "
									+ memberStatusIndEnum);
				}

				callData.set(CallDataAttribute.P_DPC_INFO,String.valueOf(memberStatusIndEnum.getCode()));

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: DPCI value is " + memberStatusIndEnum.getCode());
				}
			}

			/*
			 *  FCI
			 */
			if (iamMessage.getForwardCallIndicatorsBytes() != null) {
				FwCallIndicators forwardCallIndicators = iamMessage.getForwardCallIndicators();
				ISDNAccessIndEnum isdnAccessIndEnum = forwardCallIndicators.getIsdnAccessIndEnum();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted Forward Call Indicator from IAM is "
									+ isdnAccessIndEnum);
				}

				if (isdnAccessIndEnum != null) {
					callData.set(CallDataAttribute.P_FWD_CALL_IND,isdnAccessIndEnum.getCode());
				}
			}
			
			/*
			 * Use Calling Party number from IAM for remote control
			 */
			/*
			 * Calling Party Number
			 */
			if (iamMessage.getCallingPartyNumberBytes() != null) {
				CallingPartyNum callingPartyNum = iamMessage.getCallingPartyNumber();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted callingPartyNum from IAM is "
									+ callingPartyNum);
				}

				if (callingPartyNum != null && callingPartyNum.getAddrSignal() != null
								&& !callingPartyNum.getAddrSignal().isEmpty()) {

					callData.set(CallDataAttribute.P_IAM_CALLING_NUM,  callingPartyNum.getAddrSignal());
				}
			}

			/*
			 *  Called Party Number
			 */
			if (iamMessage.getCalledPartyNumberBytes() != null) {
				CalledPartyNum calledPartyNum = iamMessage.getCalledPartyNumber();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted CalledPartyNum from IAM is " + calledPartyNum);
				}

				if (calledPartyNum != null && calledPartyNum.getAddrSignal() != null
						&& !calledPartyNum.getAddrSignal().isEmpty()) {
					legData.set(LegDataAttributes.P_CALLED_PARTY, calledPartyNum.getAddrSignal());
				}

				if (calledPartyNum != null && calledPartyNum.getNatureOfAdrs() != null) {
					legData.set(LegDataAttributes.P_CALLED_PARTY_NOA, calledPartyNum.getNatureOfAdrs().getCode());
				}
			}

			/*
			 * Called Party Sub Address
			 */
			if (iamMessage.getAccessTransport() != null) {
				CalledPartySubaddress calledPartySubaddress =
						iamMessage.getAccessTransport().getCalledPartySubaddress();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Extracted calledPartySubaddress from IAM is "
							+ calledPartySubaddress);
				}

				if (calledPartySubaddress != null && calledPartySubaddress.getSubAddInfo() != null) {
					legData.set(LegDataAttributes.P_ISUP_AT_SUB_ADDR_INFO, calledPartySubaddress.getSubAddInfo());
				}
			}
			
			Map<Integer,byte[]> optionalparam=iamMessage.getOtherOptParams();
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: IAM message parsed successfully");
		}

		return iamMessage;
	}

	/**
	 * This method is called by protocol handler for parsing ACM from ISUP payload and
	 * keeps the relevant information into call data instance based on boolean flag passed.
	 * @param callData represents an instance of CallData
	 * @param acmByteArr represents an instance of byte[]
	 * @param populateCallData populateCallData represents boolean flag 
	 * @return an instance of ACMMessage
	 */
	public static ACMMessage parseAcm(CallData callData, byte[] acmByteArr, boolean populateCallData) {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside parseAcm");
		}

		LinkedList<byte[]> byteArrList = new LinkedList<byte[]>();
		byteArrList.add(acmByteArr);

		LinkedList<String> opCodeList = new LinkedList<String>();
		opCodeList.add(ISUPConstants.OP_CODE_ACM);

		LinkedList<Object> decodeList = ISUPOperationsCoding.decodeOperations(byteArrList,
						opCodeList);
		ACMMessage acmMessage = (ACMMessage) decodeList.getFirst();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: ACM Message is " + acmMessage);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: ACM message parsed successfully");
		}

		return acmMessage;
	}
	/**
	 * This method is called by protocol handler for parsing CPG from ISUP payload and
	 * keeps the relevant information into call data instance based on boolean flag passed.
	 * @param callData represents an instance of CallData
	 * @param cpgByteArr represents an instance of byte[]
	 * @param populateCallData populateCallData represents boolean flag 
	 * @return an instance of CPGMessage
	 */
	public static CPGMessage parseCpg(CallData callData, byte[] cpgByteArr, boolean populateCallData) {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside parseCpg");
		}
		LinkedList<byte[]> byteArrList = new LinkedList<byte[]>();
		byteArrList.add(cpgByteArr);

		LinkedList<String> opCodeList = new LinkedList<String>();
		opCodeList.add(ISUPConstants.OP_CODE_CPG);

		CPGMessage cpgMessage = (CPGMessage) ISUPOperationsCoding.decodeOperations(byteArrList,
						opCodeList).getFirst();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: CPG Message is " + cpgMessage);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: CPG message parsed successfully");
		}
		return cpgMessage;
	}
	/**
	  * This method is called by protocol handler for parsing ANM from ISUP payload and
	 * keeps the relevant information into call data instance based on boolean flag passed.
	 * @param callData represents an instance of CallData
	 * @param anmByteArr represents an instance of byte[]
	 * @param populateCallData populateCallData represents boolean flag 
	 * @return an instance of ANMMessage
	 */
	public static ANMMessage parseAnm(CallData callData, byte[] anmByteArr, boolean populateCallData) {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside parseAnm");
		}

		LinkedList<byte[]> byteArrList = new LinkedList<byte[]>();
		byteArrList.add(anmByteArr);

		LinkedList<String> opCodeList = new LinkedList<String>();
		opCodeList.add(ISUPConstants.OP_CODE_ANM);

		LinkedList<Object> decodeList = ISUPOperationsCoding.decodeOperations(byteArrList,
						opCodeList);
		ANMMessage anmMessage = (ANMMessage) decodeList.getFirst();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: ANM Message is " + anmMessage);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: ANM message parsed successfully");
		}

		return anmMessage;
	}
	/**
	 * This method is called by protocol handler to create Leg 1 CPG from ACM received (from Leg 2)
	 * @param callData represents an instance of CallData
	 * @param leg2AcmMsg represents an instance of ACMMessage
	 * @return represents an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createLeg1CpgFromLeg2Acm(CallData callData, ACMMessage leg2AcmMsg,
					boolean serviceInterestedInNoAnswer) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside createLeg1CpgFromLeg2Acm");
			logger.debug(origLegCallId + " :: Leg2 ACM Message is " + leg2AcmMsg);
			logger.debug(origLegCallId + " :: serviceInterestedInNoAnswer " + serviceInterestedInNoAnswer);
		}

		CPGMessage cpgMessage = new CPGMessage();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Set 2 as CPG Message Type");
		}

		/*
		 *  Message Type
		 */
		cpgMessage.setMessageType(new byte[] { 0x2c });
		Map<Integer, byte[]> otherOptParamsMap = leg2AcmMsg.getOtherOptParams();
		byte[] optBwCallIndicatorsBytes = leg2AcmMsg.getOptBwCallIndicatorsBytes();
		InbandInfoIndEnum inbandInfoInd = null;
		if (optBwCallIndicatorsBytes != null) {
			inbandInfoInd = leg2AcmMsg.getOptBwCallIndicators().getInbandInfoIndEnum();
			if (otherOptParamsMap != null) {
				otherOptParamsMap.put(ISUPConstants.CODE_OPT_BW_CALL_IND, optBwCallIndicatorsBytes);

			} else {
				otherOptParamsMap = new HashMap<Integer, byte[]>();
				otherOptParamsMap.put(ISUPConstants.CODE_OPT_BW_CALL_IND, optBwCallIndicatorsBytes);
			}
		}
		CalledPartyStatusIndEnum calledPartyStatusInd = leg2AcmMsg.getBackwardCallIndicators()
			.getCalledPartyStatusIndEnum();

		EventIndEnum eventInd = EventIndEnum.PROGRESS;
		if ((inbandInfoInd == null || inbandInfoInd == InbandInfoIndEnum.NO_INDICATION)
						&& (calledPartyStatusInd == CalledPartyStatusIndEnum.NO_INDICATION)) {
			/*
			 *  Progress
			 */
			eventInd = EventIndEnum.PROGRESS;
		} else if ((inbandInfoInd == null || inbandInfoInd == InbandInfoIndEnum.NO_INDICATION)
						&& (calledPartyStatusInd == CalledPartyStatusIndEnum.SUBSCRIBER_FREE)) {
			/*
			 *  Alerting
			 */
			eventInd = EventIndEnum.ALERTING;
		} else if ((inbandInfoInd == InbandInfoIndEnum.INBAND_INFO)
						&& ((calledPartyStatusInd == CalledPartyStatusIndEnum.NO_INDICATION) || (calledPartyStatusInd == CalledPartyStatusIndEnum.SUBSCRIBER_FREE))) {
			/*
			 *  In-Band Info
			 */
			eventInd = EventIndEnum.INBAND_INFO;
		}

		/*
		 *  Event Info
		 */
		byte[] eventInfoByteArr = EventInfo.encodeEventInfo(eventInd,
						EventPrsntRestIndEnum.NO_INDICATION);
		cpgMessage.setEvenntInfo(eventInfoByteArr);

		/*
		 *  Backward Call Indicators
		 */
		cpgMessage.setBwCallIndicators(leg2AcmMsg.getBackwardCallIndicatorsBytes());
		Object flexiChargeAppliedObj = callData.get(CallDataAttribute.P_FLEXI_CHRG_APPLIED);
		if (flexiChargeAppliedObj != null && flexiChargeAppliedObj.equals(PhConstants.TRUE)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: Flexible Charging applied");
				logger
					.debug(origLegCallId + " :: Set CPG BCI-Charge Indicator to 00-NO_INDICATION");
			}
			byte[] leg2BWCallIndicatorsByteArr = leg2AcmMsg.getBackwardCallIndicatorsBytes();
			BwCallIndicators leg2BWCallIndicator = BwCallIndicators
				.decodeBwCallInd(leg2BWCallIndicatorsByteArr);
			byte[] leg1BwCallIndicators = BwCallIndicators.encodeBwCallInd(
							ChargeIndEnum.NO_INDICATION, leg2BWCallIndicator
								.getCalledPartyStatusIndEnum(), leg2BWCallIndicator
								.getCalledPartyCatIndEnum(), leg2BWCallIndicator
								.getEndToEndMethodIndEnum(), leg2BWCallIndicator
								.getInterNwIndEnum(), leg2BWCallIndicator.getEndToEndInfoIndEnum(),
							leg2BWCallIndicator.getIsdnUserPartIndEnum(), leg2BWCallIndicator
								.getHoldingIndEnum(), leg2BWCallIndicator.getIsdnAccessIndEnum(),
							leg2BWCallIndicator.getEchoContDeviceIndEnum(), leg2BWCallIndicator
								.getSccpMethodIndENum());
			cpgMessage.setBwCallIndicators(leg1BwCallIndicators);
		}

		/*
		 * Carrier Information Transfer
		 */
		cpgMessage.setCarrierInfoTransfer(leg2AcmMsg.getCarrierInfoTrfrBytes());

		/*
		 * Charge Area Information
		 */
		cpgMessage.setChargeAreaInfo(leg2AcmMsg.getChargeAreaInfoBytes());

		/*
		 *  Cause Indicators
		 */
		/*
		 * Fix for 17935. If Service is interested in No-Answer then while creating leg1 ACM/CPG
		 * from leg2 ACM/CPG, do not copy Cause Indicator
		 */
		if (!serviceInterestedInNoAnswer) {
			cpgMessage.setCauseIndicator(leg2AcmMsg.getCauseIndicatorsBytes());
		}

		/*
		 * Optional Backward Call Indicator set in other params
		 */
		cpgMessage.setOtherOptParams(otherOptParamsMap);

		/*
		 * set JTI parameters here
		 */
		byte[] jtiByteArr = leg2AcmMsg.getJtiBytes();
		if (jtiByteArr != null) {
			Jti leg1Jti = Jti.decodeJti(jtiByteArr);
			if (leg1Jti.getCalledLsSubInd() != null) {
				callData.set(CallDataAttribute.P_CALLED_LS_SUB_IND,String.valueOf(leg1Jti.getCalledLsSubInd().getCode()));
			}
			if (leg1Jti.getTermBypassInd() != null) {
				callData.set(CallDataAttribute.P_TERM_BYPASS_IND,String.valueOf(leg1Jti.getTermBypassInd().getCode()));
			}
			if (leg1Jti.getTermIGSInd() != null) {
				callData.set(CallDataAttribute.P_TERM_IGS_IND,String.valueOf(leg1Jti.getTermIGSInd().getCode()));
			}
			if (leg1Jti.getCldMemStsInfo() != null) {
				callData.set(CallDataAttribute.P_CALLED_MEM_STS_IFO,String.valueOf(leg1Jti.getCldMemStsInfo().getCode()));
			}
			
			cpgMessage.setJti(jtiByteArr);
		}
		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(cpgMessage);
		opCode.add(ISUPConstants.OP_CODE_CPG);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler to create Leg 1 CPG from CPG received (from Leg 2)
	 * It should be used if flexible charging is applied to change
	 * charge indication flag to NO_INDICATION in the CPG message going to leg1
	 * @param callData represents an instance of CallData
	 * @param leg2CpgMsg represents an instance of CPGMessage
	 * @return an instance of byte[]
	 * @throws Exception
	 */	
	public static byte[] createLeg1CpgFromLeg2Cpg(CallData callData, CPGMessage leg2CpgMsg,
					boolean serviceInterestedInNoAnswer) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside createLeg1CpgFromLeg2Cpg");
			logger.debug(origLegCallId + " :: Leg2 CPG Message is " + leg2CpgMsg);
		}
		/*
		 * SBTM-UAT-1282: If BCI is not included in CPG from B-Party, SN does not have to create it
		 * to A-Party side.
		 */
		byte[] leg2BWCallIndicatorsByteArr = leg2CpgMsg.getBwCallIndicatorsBytes();
		if (leg2BWCallIndicatorsByteArr != null) {
			BwCallIndicators leg2BWCallIndicator = BwCallIndicators
				.decodeBwCallInd(leg2BWCallIndicatorsByteArr);
			byte[] leg1BwCallIndicators = BwCallIndicators.encodeBwCallInd(
							ChargeIndEnum.NO_INDICATION, leg2BWCallIndicator
								.getCalledPartyStatusIndEnum(), leg2BWCallIndicator
								.getCalledPartyCatIndEnum(), leg2BWCallIndicator
								.getEndToEndMethodIndEnum(), leg2BWCallIndicator
								.getInterNwIndEnum(), leg2BWCallIndicator.getEndToEndInfoIndEnum(),
							leg2BWCallIndicator.getIsdnUserPartIndEnum(), leg2BWCallIndicator
								.getHoldingIndEnum(), leg2BWCallIndicator.getIsdnAccessIndEnum(),
							leg2BWCallIndicator.getEchoContDeviceIndEnum(), leg2BWCallIndicator
								.getSccpMethodIndENum());
			leg2CpgMsg.setBwCallIndicators(leg1BwCallIndicators);
		}
		
		/*
		 * Fix for 17935. If Service is interested in No-Answer then while creating leg1 ACM/CPG
		 * from leg2 ACM/CPG, do not copy Cause Indicator
		 */
		if (serviceInterestedInNoAnswer) {
			leg2CpgMsg.setCauseIndicator(null);
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Created Leg1 CPG Message is " + leg2CpgMsg);
		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(leg2CpgMsg);
		opCode.add(ISUPConstants.OP_CODE_CPG);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);

		return encodeList.getFirst();
	}
	/**
	 * This method is called by protocol handler to create Leg 1 ACM from ACM received (from Leg 2)
	 * This method should be used if flexible charging is applied to change
	 * charge indication flag to NO_INDICATION in the ACM message going to leg1
	 * @param callData represents an instance of CallData
	 * @param leg2AcmMsg represents an instance of ACMMessage
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createLeg1AcmFromLeg2Acm(CallData callData, ACMMessage leg2AcmMsg,
					boolean serviceInterestedInNoAnswer) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside createLeg1AcmFromLeg2Acm");
			logger.debug(origLegCallId + " :: Leg2 ACM Message is " + leg2AcmMsg);
		}

		byte[] leg2BWCallIndicatorsByteArr = leg2AcmMsg.getBackwardCallIndicatorsBytes();
		BwCallIndicators leg2BWCallIndicator = BwCallIndicators
			.decodeBwCallInd(leg2BWCallIndicatorsByteArr);
		byte[] leg1BwCallIndicators = BwCallIndicators
			.encodeBwCallInd(ChargeIndEnum.NO_INDICATION, leg2BWCallIndicator
				.getCalledPartyStatusIndEnum(), leg2BWCallIndicator.getCalledPartyCatIndEnum(),
							leg2BWCallIndicator.getEndToEndMethodIndEnum(), leg2BWCallIndicator
								.getInterNwIndEnum(), leg2BWCallIndicator.getEndToEndInfoIndEnum(),
							leg2BWCallIndicator.getIsdnUserPartIndEnum(), leg2BWCallIndicator
								.getHoldingIndEnum(), leg2BWCallIndicator.getIsdnAccessIndEnum(),
							leg2BWCallIndicator.getEchoContDeviceIndEnum(), leg2BWCallIndicator
								.getSccpMethodIndENum());
		leg2AcmMsg.setBackwardCallIndicators(leg1BwCallIndicators);
		/*
		 * Fix for 17935. If Service is interested in No-Answer then while creating leg1 ACM/CPG
		 * from leg2 ACM/CPG, do not copy Cause Indicator
		 */
		if (serviceInterestedInNoAnswer) {
			leg2AcmMsg.setCauseIndicators(null);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Created Leg1 ACM Message is " + leg2AcmMsg);
		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(leg2AcmMsg);
		opCode.add(ISUPConstants.OP_CODE_ACM);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);

		return encodeList.getFirst();
	}
	/**
	 * This method is called by protocol handler to create Leg 1 ACM from ACM received (from Leg 2)
	 * This method should be used if flexible charging is applied to change
	 * charge indication flag to NO_INDICATION in the ACM message going to leg1
	 * @param callData represents an instance of CallData
	 * @param leg2AnmMsg represents an instance of ANMMessage
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createLeg1AnmFromLeg2Anm(CallData callData, ANMMessage leg2AnmMsg)
					throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside createLeg1AnmFromLeg2Anm");
			logger.debug(origLegCallId + " :: Leg2 ANM Message is " + leg2AnmMsg);
		}

		//modified for bug 9111
		//leg2AnmMsg.getBackwardCallIndicators().setChargeIndEnum(ChargeIndEnum.CHARGE);
		/*
		 * SBTM-UAT-1239: If flexible charging is applied on the call, then set ChargeInd to CHARGE
		 * otherwise transparently forward B leg ANM to A Leg
		 */
		Object flexiChargeAppliedObj = callData.get(CallDataAttribute.P_FLEXI_CHRG_APPLIED);
		if (flexiChargeAppliedObj != null && flexiChargeAppliedObj.equals(PhConstants.TRUE)) {
			byte[] leg2BWCallIndicatorsByteArr = leg2AnmMsg.getBackwardCallIndicatorsBytes();
			BwCallIndicators leg2BWCallIndicator = BwCallIndicators
				.decodeBwCallInd(leg2BWCallIndicatorsByteArr);
			byte[] leg1BwCallIndicators = BwCallIndicators
				.encodeBwCallInd(ChargeIndEnum.CHARGE, leg2BWCallIndicator
					.getCalledPartyStatusIndEnum(), leg2BWCallIndicator.getCalledPartyCatIndEnum(),
								leg2BWCallIndicator.getEndToEndMethodIndEnum(), leg2BWCallIndicator
									.getInterNwIndEnum(), leg2BWCallIndicator.getEndToEndInfoIndEnum(),
								leg2BWCallIndicator.getIsdnUserPartIndEnum(), leg2BWCallIndicator
									.getHoldingIndEnum(), leg2BWCallIndicator.getIsdnAccessIndEnum(),
								leg2BWCallIndicator.getEchoContDeviceIndEnum(), leg2BWCallIndicator
									.getSccpMethodIndENum());
			leg2AnmMsg.setBackwardCallIndicators(leg1BwCallIndicators);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Created Leg1 ANM Message is " + leg2AnmMsg);
		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(leg2AnmMsg);
		opCode.add(ISUPConstants.OP_CODE_ANM);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}
	/**
	 * This method is called by protocol handler to create chg byte array.
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createChg(CallData callData) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside createChg");
		}

		CHGMessage chgMessage = new CHGMessage();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Set 254 as CHG Message Type");
		}

		/*
		 *  Message Type
		 */
		chgMessage.setMessageType(new byte[] { (byte) 0xFE });

		/*
		 *  Charge Information Type
		 */
		chgMessage.setChargingInfoCategory(ChargingInfoCategory
			.encodeChargingInfoCat(ChargingInfoCatEnum.CHARGING_RATE_TRFR));

		/*
		 *  Charge Information
		 */
		ChargingInfo chargingInfo = new ChargingInfo();

		ChgRateTrfrChargingInfo chgRateTrfrChgInfo = new ChgRateTrfrChargingInfo();
		chgRateTrfrChgInfo.setUnitRateIndEnum(UnitRateIndEnum.YEN_10);
		chgRateTrfrChgInfo.setChgRateInfoCatEnum(ChgRateInfoCatEnum.ORDINARY_CALLING_SUBS);
		chgRateTrfrChgInfo.setInitialCallRate(0);
		LinkedList<Float> chargeIntervalList = new LinkedList<Float>();
		
		float[] chgIntArr = null;
		if (callData.get(CallDataAttribute.P_CHRG_INTERVAL) != null) {
			chgIntArr = (float[]) callData
					.get(CallDataAttribute.P_CHRG_INTERVAL);
		}
		if (chgIntArr != null) {
			for (float chgInt : chgIntArr) {
				chargeIntervalList.add(chgInt);
			}
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Set charging interval in CHG: " + chargeIntervalList);
		}
		chgRateTrfrChgInfo.setChargingPeriods(chargeIntervalList);
		chargingInfo.setChgRateTrfrChargingInfo(chgRateTrfrChgInfo);

		FlexibleChgChargingInfo flexibleChgChargingInfo = new FlexibleChgChargingInfo();
		flexibleChgChargingInfo.setActivationId(1);
		flexibleChgChargingInfo.setChargeRateIndEnum(ChargeRateIndEnum.SECONDS_10_YEN);
		flexibleChgChargingInfo.setChargeCollectionMethodEnum(ChargeCollectionMethodEnum.SPARE);
		flexibleChgChargingInfo.setChargedPartyTypeEnum(ChargedPartyTypeEnum.SPARE);
		flexibleChgChargingInfo.setOperationClassEnum(OperationClassEnum.SPARE);
		flexibleChgChargingInfo.setOperationTypeEnum(OperationTypeEnum.SPARE);
		chargingInfo.setFlexibleChgChargingInfo(flexibleChgChargingInfo);

		chgMessage.setChargingInfo(ChargingInfo.encodeChargingInfo(chargingInfo,
						ChargingInfoCatEnum.CHARGING_RATE_TRFR));

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: CHG Message is " + chgMessage);
		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(chgMessage);
		opCode.add(ISUPConstants.OP_CODE_CHG);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler for creating leg 1 cpg for alternate routing.
	 * @param callData represents an instance of CallData
	 * @return represents an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createLeg1CpgForAlternateRouting(CallData callData) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside createLeg1CpgForAlternateRouting");
		}

		CPGMessage cpgMessage = new CPGMessage();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Set 44 as CPG Message Type");
		}

		/*
		 *  Message Type
		 */
		cpgMessage.setMessageType(new byte[] { 0x2c });

		/*
		 * Event Info
		 */
		byte[] eventInfoByteArr = EventInfo.encodeEventInfo(EventIndEnum.PROGRESS,
						EventPrsntRestIndEnum.NO_INDICATION);
		cpgMessage.setEvenntInfo(eventInfoByteArr);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Created CPG Message is " + cpgMessage);
		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(cpgMessage);
		opCode.add(ISUPConstants.OP_CODE_CPG);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}
	/**
	 * This method is called by protocol handler for creating Leg 2 IAM message
	 * @param callData represents an instance of CallData
	 * @param leg1IamMsg represents an instance of IAMMessage
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createLeg2Iam(CallData callData, IAMMessage leg1IamMsg) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside createLeg2Iam");
		}

		Object destinationNumberObj = callData.get(CallDataAttribute.P_DESTINATION_NUMBER);
		if (destinationNumberObj != null) {
			PhoneNumber destinationNumber = (PhoneNumber) destinationNumberObj;
			String destNumber = destinationNumber.getAddress();

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: Create IAM for destination " + destNumber);
			}

			/*
			 * As per the PH document, the NOA should be 126 in case configured
			 * destination number was direct access subscriber otherwise always
			 * set 3 as nature of address
			 */
			int destNoa = destinationNumber.getNatureOfAddress();
			NatureOfAddEnum natureOfAddrEnum = NatureOfAddEnum.NATIONAL_NO;

			switch (destNoa) {
				case PhoneNumber.NOA_UNKNOWN:
					natureOfAddrEnum = NatureOfAddEnum.UNKNOWN;
					break;
				case PhoneNumber.NOA_NATIONAL:
					natureOfAddrEnum = NatureOfAddEnum.NATIONAL_NO;
					break;
				case PhoneNumber.NOA_INTERNATIONAL:
					natureOfAddrEnum = NatureOfAddEnum.INTER_NO;
					break;
			}

			if ("1".equals(callData.get(CallDataAttribute.P_CALLED_PARTY_TYPE))) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + " :: Change NOA to 126 for QuickLine subscribers");
				}
				natureOfAddrEnum = NatureOfAddEnum.NETWORK_SPEC_NO;
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + " :: Set Service Indicator for QuickLine subscribers");
				}
				/*
				 * SBTM-UAT-1202: If terminating number is "Direct Access Number" then set
				 * "Service Activation parameter" to 123 in IAM that is being sent to destination
				 */
				byte[] serviceActivation = ServiceActivation
						.encodeServiceActivation(FeatureCodeEnum.QUICK_LINE);
				leg1IamMsg.setServiceActivation(serviceActivation);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: Set Called Party Number in IAM Message");
				logger
						.debug(origLegCallId + " :: Set null in correlation id and scf id of IAM Message");
			}

			byte[] calledPartyByteArr = CalledPartyNum.encodeCaldParty(destNumber, natureOfAddrEnum,
					NumPlanEnum.ISDN_NP, IntNwNumEnum.ROUTING_ALLWD);
			leg1IamMsg.setCalledPartyNumber(calledPartyByteArr);

			leg1IamMsg.setCorrelationId(null);
			leg1IamMsg.setScfId(null);

			AddPrsntRestEnum addrPrsntRest = AddPrsntRestEnum.PRSNT_ALLWD;
			if (destinationNumber.getPresentationIndicator() == 1) {
				addrPrsntRest = AddPrsntRestEnum.PRSNT_ALLWD;
			} else {
				addrPrsntRest = AddPrsntRestEnum.PRSNT_RESTD;
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: Destination Address presentation restriction is "
						+ addrPrsntRest);
				logger.debug(origLegCallId + " :: Set Called IN Number in IAM Message");
			}
			if (leg1IamMsg.getCalledINNumberBytes() != null) {
				TtcCalledINNumber ttcCalledInNumber = leg1IamMsg.getCalledINNumber();
				byte[] calledInNumberByteArr = TtcCalledINNumber.encodeTtcCalledINNum(ttcCalledInNumber
						.getAddrSignal(), ttcCalledInNumber.getNatureOfAdrs(), ttcCalledInNumber
						.getNumPlan(), addrPrsntRest);
				leg1IamMsg.setCalledINNumber(calledInNumberByteArr);
			}
		}

		/*
		 * Fix for SBTM-UAT-1387 - Access Transport should be copied from leg1 IAM to leg2 IAM
		 * except sub-address dial in case where sub-address will be appended to access transport
		 */
		
		/*
		 *  Access Transport Parameter
		 */
		if (callData.get(CallDataAttribute.P_SERVICE_IND) == SignalingTypeEnum.SERVICEINDICATOR.SUBADDRESS_DIAL_IN_NSAP
						|| callData.get(CallDataAttribute.P_SERVICE_IND)== SignalingTypeEnum.SERVICEINDICATOR.SUBADDRESS_DIAL_IN_USER) {
			CalledPartySubaddress calledPartySubAddress = new CalledPartySubaddress();
			calledPartySubAddress.setSubAddInfo((String) callData.get(CallDataAttribute.P_CALLED_SUB_ADDRESS));
			if (callData.get(CallDataAttribute.P_SERVICE_IND) == SignalingTypeEnum.SERVICEINDICATOR.SUBADDRESS_DIAL_IN_NSAP) {
				calledPartySubAddress.setTypeOfSubaddress(TypeOfSubaddress.NSAP);
			} else if (callData.get(CallDataAttribute.P_SERVICE_IND)== SignalingTypeEnum.SERVICEINDICATOR.SUBADDRESS_DIAL_IN_USER) {
				calledPartySubAddress.setTypeOfSubaddress(TypeOfSubaddress.USER_SPECIFIED);
			}
			//LinkedList<Byte> otherParams = leg1IamMsg.getAccessTransport().getOtherParams();
			/*
			 * SBTM-UAT-1194
			 */
			LinkedList<Byte> otherParams = null;

			if (leg1IamMsg.getAccessTransportBytes() != null) {
				otherParams = leg1IamMsg.getAccessTransport().getOtherParams();
			}
			leg1IamMsg.setAccessTransport(AccessTransport.encodeAccessTransport(
							calledPartySubAddress, otherParams));
			//	leg1IamMsg.setAccessTransport(AccessTransport.encodeAccessTransport(
			//					calledPartySubAddress, null));
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Created IAM Message is " + leg1IamMsg);
		}
		/*
		 * Fix for bug#13236 : SBTM-UAT-495 Routing failure on the handling of DPC
		 */
		leg1IamMsg.setDPCInfo(null);
		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(leg1IamMsg);
		opCode.add(ISUPConstants.OP_CODE_IAM);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	private static Logger	logger	= Logger.getLogger(SipIsupParser.class);

	/**
	 * This method is used to create a new IAM request
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createIam(CallData callData) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside createIam");
		}
		IAMMessage leg1IamMsg = (IAMMessage) callData.get(CallDataAttribute.P_ISUP_LEG1_IAM_MESSAGE);
		IAMMessage leg2IamMsg = cloneIAM(leg1IamMsg, origLegCallId);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		PhoneNumber destinationNumber = null;
		if (leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER) != null) {
			destinationNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);
		} else if (callData.get(CallDataAttribute.P_DESTINATION_NUMBER) != null) {
			destinationNumber = (PhoneNumber) callData.get(CallDataAttribute.P_DESTINATION_NUMBER);
		}

		if (destinationNumber != null) {
			String destNumber = destinationNumber.getAddress();

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: Create IAM for destination " + destNumber);
			}

			/*
			 * As per the PH document, the NOA should be 126 in case configured
			 * destination number was direct access subscriber otherwise always
			 * set 3 as nature of address
			 */
			int destNoa = destinationNumber.getNatureOfAddress();
			if (leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER_NOA_INT) != null) {
				destNoa = (Integer) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER_NOA_INT);
			}

			NatureOfAddEnum natureOfAddrEnum = NatureOfAddEnum.NATIONAL_NO;

			switch (destNoa) {
				case PhoneNumber.NOA_UNKNOWN:
					natureOfAddrEnum = NatureOfAddEnum.UNKNOWN;
					break;
				case PhoneNumber.NOA_NATIONAL:
					natureOfAddrEnum = NatureOfAddEnum.NATIONAL_NO;
					break;
				case PhoneNumber.NOA_INTERNATIONAL:
					natureOfAddrEnum = NatureOfAddEnum.INTER_NO;
					break;
			}

			if ("1".equals(callData.get(CallDataAttribute.P_CALLED_PARTY_TYPE))) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + " :: Change NOA to 126 for QuickLine subscribers");
				}
				natureOfAddrEnum = NatureOfAddEnum.NETWORK_SPEC_NO;
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + " :: Set Service Indicator for QuickLine subscribers");
				}
				/*
				 *  If terminating number is "Direct Access Number" then set
				 * "Service Activation parameter" to 123 in IAM that is being sent to destination
				 */
				byte[] serviceActivation = ServiceActivation
						.encodeServiceActivation(FeatureCodeEnum.QUICK_LINE);
				leg2IamMsg.setServiceActivation(serviceActivation);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: Set Called Party Number in IAM Message");
				logger
						.debug(origLegCallId + " :: Set null in correlation id and scf id of IAM Message");
			}

			byte[] calledPartyByteArr = CalledPartyNum.encodeCaldParty(destNumber, natureOfAddrEnum,
					NumPlanEnum.ISDN_NP, IntNwNumEnum.ROUTING_ALLWD);
			leg2IamMsg.setCalledPartyNumber(calledPartyByteArr);

			leg2IamMsg.setCorrelationId(null);
			leg2IamMsg.setScfId(null);

			AddPrsntRestEnum addrPrsntRest = AddPrsntRestEnum.PRSNT_ALLWD;
			if (destinationNumber.getPresentationIndicator() == 1) {
				addrPrsntRest = AddPrsntRestEnum.PRSNT_ALLWD;
			} else {
				addrPrsntRest = AddPrsntRestEnum.PRSNT_RESTD;
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: Destination Address presentation restriction is "
						+ addrPrsntRest);
				logger.debug(origLegCallId + " :: Set Called IN Number in IAM Message");
			}
			if (leg2IamMsg.getCalledINNumberBytes() != null) {
				TtcCalledINNumber ttcCalledInNumber = leg2IamMsg.getCalledINNumber();
				byte[] calledInNumberByteArr = TtcCalledINNumber.encodeTtcCalledINNum(ttcCalledInNumber
						.getAddrSignal(), ttcCalledInNumber.getNatureOfAdrs(), ttcCalledInNumber
						.getNumPlan(), addrPrsntRest);
				leg2IamMsg.setCalledINNumber(calledInNumberByteArr);
			}
		}

		/*
		 *  Access Transport should be copied from leg1 IAM to leg2 IAM
		 * except sub-address dial in case where sub-address will be appended to access transport
		 */
		
		/*
		 *  Access Transport Parameter
		 */
		if (callData.get(CallDataAttribute.P_SERVICE_IND) == SignalingTypeEnum.SERVICEINDICATOR.SUBADDRESS_DIAL_IN_NSAP
						|| callData.get(CallDataAttribute.P_SERVICE_IND)== SignalingTypeEnum.SERVICEINDICATOR.SUBADDRESS_DIAL_IN_USER) {
			CalledPartySubaddress calledPartySubAddress = new CalledPartySubaddress();
			calledPartySubAddress.setSubAddInfo((String) callData.get(CallDataAttribute.P_CALLED_SUB_ADDRESS));
			if (callData.get(CallDataAttribute.P_SERVICE_IND) == SignalingTypeEnum.SERVICEINDICATOR.SUBADDRESS_DIAL_IN_NSAP) {
				calledPartySubAddress.setTypeOfSubaddress(TypeOfSubaddress.NSAP);
			} else if (callData.get(CallDataAttribute.P_SERVICE_IND)== SignalingTypeEnum.SERVICEINDICATOR.SUBADDRESS_DIAL_IN_USER) {
				calledPartySubAddress.setTypeOfSubaddress(TypeOfSubaddress.USER_SPECIFIED);
			}
			
			
			LinkedList<Byte> otherParams = null;

			if (leg2IamMsg.getAccessTransportBytes() != null) {
				otherParams = leg2IamMsg.getAccessTransport().getOtherParams();
			}
			leg2IamMsg.setAccessTransport(AccessTransport.encodeAccessTransport(
							calledPartySubAddress, otherParams));
		}

		/*
		 * Access Transport
		 */
		if (leg2Data.get(LegDataAttributes.P_ISUP_AT_SUB_ADDR_INFO) != null
				&& leg1IamMsg.getAccessTransportBytes() != null) {
			AccessTransport accessTransport = leg1IamMsg.getAccessTransport();
			CalledPartySubaddress calledPartySubaddress = new CalledPartySubaddress();
			calledPartySubaddress.setSubAddInfo((String) leg2Data.get(LegDataAttributes.P_ISUP_AT_SUB_ADDR_INFO));

			leg2IamMsg.setAccessTransport(AccessTransport.encodeAccessTransport(
					calledPartySubaddress, accessTransport.getOtherParams()));
		}

		/*
		 * OLEC
		 */
		if (leg2Data.get(LegDataAttributes.P_ISUP_OLEC_CIC) != null
				&& leg1IamMsg.getCarrierInfoTransferBytes() != null) {
			TtcCarrierInfoTrfr carrierInfoTrfr = leg1IamMsg.getCarrierInfoTransfer();

			LinkedList<CarrierInformation> updatedCarrierInfos = new LinkedList<CarrierInformation>();
			for (CarrierInformation carrierInformation : carrierInfoTrfr.getCarrierInformation()) {
				CarrierInformation updatedCarrierInformation = new CarrierInformation();
				updatedCarrierInformation.setCarrierInfoLength(carrierInformation.getCarrierInfoLength());
				updatedCarrierInformation.setCarrierInfoNameEnum(carrierInformation.getCarrierInfoNameEnum());

				// Search for OLEC
				if (carrierInformation.getCarrierInfoNameEnum() == CarrierInfoNameEnum.OLEC) {
					LinkedList<CarrierInfoSubordinate> updatedCarrierInfoSubords = new LinkedList<CarrierInfoSubordinate>();
					for (CarrierInfoSubordinate carrierInfoSubordinate : carrierInformation.getCarrierInfoSubordinate()) {
						// Search for CIC
						if (carrierInfoSubordinate.getCarrierInfoSubordinateEnum() == CarrierInfoSubordinateEnum.CARRIER_IDENT_CODE) {
							CarrierInfoSubordinate updateCarrierInfoSubord = new CarrierInfoSubordinate();
							updateCarrierInfoSubord.setCarrierInfoSubordinateEnum(carrierInfoSubordinate.getCarrierInfoSubordinateEnum());
							updateCarrierInfoSubord.setCarrierInfoSubOrdinateLength(carrierInfoSubordinate.getCarrierInfoSubOrdinateLength());
							updateCarrierInfoSubord.setPoiChargeAreaInfo(carrierInfoSubordinate.getPoiChargeAreaInfo());
							updateCarrierInfoSubord.setPoiLevelInfo(carrierInfoSubordinate.getPoiLevelInfo());

							// Get CIC from leg2 data
							String cic = (String) leg2Data.get(LegDataAttributes.P_ISUP_OLEC_CIC);
							CarrierIdentificationCode carrierIdentificationCode = new CarrierIdentificationCode();
							carrierIdentificationCode.setCarrierIdentCode(cic);
							updateCarrierInfoSubord.setCarrierIdentificationCode(carrierIdentificationCode);

							updatedCarrierInfoSubords.add(updateCarrierInfoSubord);
							// Don't update others
						} else {
							updatedCarrierInfoSubords.add(carrierInfoSubordinate);
						}
					}
					updatedCarrierInformation.setCarrierInfoSubordinate(updatedCarrierInfoSubords);
					// Keep others as it is
				} else {
					updatedCarrierInformation.setCarrierInfoSubordinate(carrierInformation.getCarrierInfoSubordinate());
				}
				updatedCarrierInfos.add(updatedCarrierInformation);
			}

			// Encode and set updated carrier info transfer into leg2 IAM msg
			leg2IamMsg.setCarrierInfoTransfer(
					TtcCarrierInfoTrfr.encodeTtcCarrierInfoTrfr(
							carrierInfoTrfr.getTransitCarrierIndEnum(),
							updatedCarrierInfos
					)
			);
		}

		/*
		 * Charge Area Information
		 */
		if (leg2Data.get(LegDataAttributes.P_ISUP_CHARGING_AREA) != null
				&& leg1IamMsg.getChargeAreaInformationBytes() != null) {
			TtcChargeAreaInfo chargeAreaInfo = leg1IamMsg.getChargeAreaInformation();

			// Encode and set updated charge area info into leg2 IAM msg
			leg2IamMsg.setChargeAreaInformation(
					TtcChargeAreaInfo.encodeTtcChargeAreaInfo(
							(String) leg2Data.get(LegDataAttributes.P_ISUP_CHARGING_AREA),
							chargeAreaInfo.getInfoDiscriminationIndiEnum()
					)
			);
		}

		/*
		 * Calling Party
		 */
		if (leg2Data.get(LegDataAttributes.P_CALLING_PARTY) != null
				&& leg1IamMsg.getCallingPartyNumberBytes() != null) {
			CallingPartyNum callingPartyNumber = leg1IamMsg.getCallingPartyNumber();
			String callingPartyAddress = (String) leg2Data.get(LegDataAttributes.P_CALLING_PARTY);

			leg2IamMsg.setCallingPartyNumber(
				CallingPartyNum.encodeCalgParty(
						callingPartyAddress,
						callingPartyNumber.getNatureOfAdrs(),
						callingPartyNumber.getNumPlan(),
						callingPartyNumber.getAdrsPresntRestd(),
						callingPartyNumber.getScreening(),
						callingPartyNumber.getNumIncomplte()
				)
			);
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Created IAM Message is " + leg2IamMsg);
		}
		/*
		 * Routing failure on the handling of DPC
		 */
		leg2IamMsg.setDPCInfo(null);
		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(leg2IamMsg);
		opCode.add(ISUPConstants.OP_CODE_IAM);

		LinkedList<byte[]> encodeList = ISUPOperationsCoding
			.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * Create a clone of input IAM message
	 * @param iamMessage the input IAM message
	 * @param origLegCallId the original Leg Call ID
	 * @return the cloned IAM message
	 */
	private static IAMMessage cloneIAM(IAMMessage iamMessage, String origLegCallId) {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside cloneIAM");
		}
		IAMMessage clonedIamMsg = new IAMMessage();
		clonedIamMsg.setMessageType(iamMessage.getMessageTypeBytes());
		clonedIamMsg.setProtocol(iamMessage.getProtocol());
		clonedIamMsg.setNatureOfConnIndicators(iamMessage.getNatureOfConnIndicatorsBytes());
		clonedIamMsg.setForwardCallIndicators(iamMessage.getForwardCallIndicatorsBytes());
		clonedIamMsg.setCallingPartyCategory(iamMessage.getCallingPartyCategoryBytes());
		clonedIamMsg.setTmr(iamMessage.getTmrBytes());
		clonedIamMsg.setCalledPartyNumber(iamMessage.getCalledPartyNumberBytes());
		clonedIamMsg.setCallingPartyNumber(iamMessage.getCallingPartyNumberBytes());
		clonedIamMsg.setCorrelationId(iamMessage.getCorrelationIdBytes());
		clonedIamMsg.setScfId(iamMessage.getScfIdBytes());
		clonedIamMsg.setCalledINNumber(iamMessage.getCalledINNumberBytes());
		clonedIamMsg.setAccessTransport(iamMessage.getAccessTransportBytes());
		clonedIamMsg.setDPCInfo(iamMessage.getDpcInfoBytes());
		clonedIamMsg.setAdditionalPartyCat(iamMessage.getAdditionalPartyCategoryBytes());
		clonedIamMsg.setCarrierInfoTransfer(iamMessage.getCarrierInfoTransferBytes());
		clonedIamMsg.setGenericNumber(iamMessage.getGenericNumberBytes());
		clonedIamMsg.setChargeAreaInformation(iamMessage.getChargeAreaInformationBytes());
		clonedIamMsg.setUserServiceInfoByte(iamMessage.getUserServiceInfoBytes());
		clonedIamMsg.setJurisdictionInfoByte(iamMessage.getJurisdictionInfoByte());
		clonedIamMsg.setChargeNumber(iamMessage.getChargeNumberBytes());
		clonedIamMsg.setContractorNumber(iamMessage.getContractorNumberBytes());
		clonedIamMsg.setRedirectingNumber(iamMessage.getRedirectingNumberBytes());
		clonedIamMsg.setOriginalCalledNumber(iamMessage.getOriginalCalledNumberBytes());
		clonedIamMsg.setRedirectingNumber(iamMessage.getRedirectingNumberBytes());
		clonedIamMsg.setOtherOptParams(iamMessage.getOtherOptParams());
		return clonedIamMsg;
	}
	
	/**
	 * This method is used to get number form of cpc from isup cpc enum
	 * @param callingPartyCatNumber
	 * @return
	 */
	private static String getCpcFromEnum(String origLegCallId,CalgPartyCatgEnum callingPartyCatNumber) {
		String cpc = "0";

		switch (callingPartyCatNumber) {
		
		case DATA_CALL:
			cpc = "12";
			break;
		case OPRT_ENG:
		case OPRT_FRENCH:
		case OPRT_GERMAN:
		case OPRT_RUSSIAN:
		case OPRT_SPANISH:
			cpc = "1";
			break;
		case ORD_SUBSR:
			cpc = "10";
			break;
		case TEST_CALL:
			cpc = "13";
			break;
		case PAYPHONE:
			cpc = "15";
			break;
		case SPARE:
			cpc = "14";
			break;
		case UNKNOWN:
			cpc = "0";
			break;
		case SUBSR_PROTY:
			cpc = "11";
			break;
		default:
			cpc = "0";
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Parsed cpc from isup is " + cpc);
		}
		return cpc;
	}

}
