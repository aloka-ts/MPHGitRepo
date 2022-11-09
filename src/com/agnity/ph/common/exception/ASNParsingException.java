/*******************************************************************************
 * Copyright (c) 2011 Agnity, Inc. All rights reserved.
 * <p>
 * This is proprietary source code of Agnity, Inc.
 * <p>
 * Agnity, Inc. retains all intellectual property rights associated
 * with this source code. Use is subject to license terms.
 * <p>
 * This source code contains trade secrets owned by Agnity, Inc.
 * Confidentiality of this computer program must be maintained at
 * all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.ph.common.exception;

/**
 *
 * This class is the exception class used to handle ASN parsing xceptions
 */
public class ASNParsingException extends Exception {

    private static final long serialVersionUID = -5660714504363006358L;
    private MESSAGE inapMessage;
    private FAILTYPE parseFailType;

    public ASNParsingException(String errorString, MESSAGE inapMessage) {
        super(errorString);
        this.setMessage(inapMessage);
        this.setParseFailType(FAILTYPE.DEFAULT);
    }

    public void setMessage(MESSAGE inapMessage) {
        this.inapMessage = inapMessage;
    }

    public ASNParsingException(String errorString, Throwable t, MESSAGE inapMessage) {
        super(errorString, t);
        this.setMessage(inapMessage);
        this.setParseFailType(FAILTYPE.DEFAULT);
    }

    public ASNParsingException(String errorString, MESSAGE inapMessage, FAILTYPE parseFailType) {
        super(errorString);
        this.setMessage(inapMessage);
        this.setParseFailType(parseFailType);
    }

    public ASNParsingException(String errorString, Throwable t, MESSAGE inapMessage, FAILTYPE parseFailType) {
        super(errorString, t);
        this.setMessage(inapMessage);
        this.setParseFailType(parseFailType);
    }

    public ASNParsingException() {
        super();
        this.setMessage(MESSAGE.DEFAULT);
        this.setParseFailType(FAILTYPE.DEFAULT);
    }

    public MESSAGE getInapMessage() {
        return inapMessage;
    }

    public FAILTYPE getParseFailType() {
        return parseFailType;
    }

    public void setParseFailType(FAILTYPE parseFailType) {
        this.parseFailType = parseFailType;
    }

    public static enum MESSAGE {
        IDP,
        ENC,
        ERB,
        ERB_BUSY,
        ERB_NOANS,
        ERB_ANS,
        ERB_TERMSIEZED,
        ERB_DISCONNECT,
        ERB_ABANDON,
        UERROR,
        UREJECT,
        DEFAULT,
        ACR,
        SRR,
        ERB_ROUTESELECTFAILURE,
        ARI,
        INFO_ANALYZE,
        NTWK_BUSY,
        RES_CLR,
        CLOSE,
        INFO_COLLECT,
        NSDM,
        ATI,
        ATSI,
        SRI,
        ATM,
        PA,
        PAC,
        TERM_ATTEMPT,
        AUTH_TERMINATION,
        CALL_INFO_FRM_RESRC,
        BNS_QUERY,
        GN_QUERY,
        LIDB_PROTOCOL_ERR,
        OLNS_QUERY,
        GET_DATA_QUERY,
        CC1_QUERY,
        CC2_QUERY,
        TLNS_QUERY,
        INTERCEPT_QUERY,
        ICDC_QUERY,
        PROVIDE_INSTRUCTION,
        AC_QUERY,
        ISVM_QUERY,
        TERMINATION_NOTIFICATION, 
        ACCOUNT_CODE_QUERY,
	SS_INVOKE
    }

    public static enum FAILTYPE {
        CPC_MISSING,
        TMR_MISSING,
        ACPC_MISSING,
        FCI_MISSING,
        APPLICATION_ERROR,
        PROTOCOL_ERROR,
        DEFAULT
    }


}
