package com.agnity.ph.inapcs2scf.messagehelper;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

/**
 * Created by ankitsinghal on 14/10/16.
 */
public class BCDEncoderHelper {

    private static Logger logger = Logger.getLogger(BCDEncoderHelper.class);

    public static byte[] getBCDEncodedArray(String inputString, int totalOctetLength) {
        //Since 2 input characters are coded into a single byte, so, max size of input string cannot be twice of totalOctetLength.
        int requiredInputByteArraySize = 2 * totalOctetLength;
        if (inputString.length() > requiredInputByteArraySize) {
            throw new IllegalArgumentException(
                    String.format("Input String length exceeded the required length! Output byte array cannot fit in given size! " +
                                          "Input String size: [%s], Max Allowed: [%s]", inputString.length(), requiredInputByteArraySize));
        }
        //Padding with required number of 0's to make the input string as per required size
        inputString = StringUtils.leftPad(inputString, requiredInputByteArraySize, "0");

        byte[] outputByteArray = encodeBCDString(inputString);

        if (logger.isDebugEnabled()) {
            logger.debug(String.format("Encoded input String :[%s] to [%s]", inputString, Hex.encodeHex(outputByteArray)));
        }

        return outputByteArray;
    }

    private static byte[] encodeBCDString(String inputString) {
        byte[] varByteArrayNew = inputString.getBytes();
        int outputByteArrayLength = varByteArrayNew.length >> 1;
        byte[] outputByteArray = new byte[outputByteArrayLength];
        for (int i = 0, outIndex = 0; outIndex < outputByteArrayLength; i += 2, outIndex++) {
            byte n1 = (byte) ((varByteArrayNew[i + 1] - '0') & 0xf);
            byte n2 = (byte) ((varByteArrayNew[i] - '0') & 0xf);
            outputByteArray[outIndex] = (byte) ((n1 << 4) | n2);
        }
        return outputByteArray;
    }
}
