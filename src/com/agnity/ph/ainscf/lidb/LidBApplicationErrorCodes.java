/*
 * package com.agnity.ph.ainscf.lidb;
 * 
 * public enum LidBApplicationErrorCodes { UNEXPECTED_COMPONENT_SEQUENCE(1),
 * UNEXPECTED_DATA_VALUE(2), UNAVAILABLE_NETWORK_RESOURCE(3),
 * MISSING_CUSTOMER_RECORD(4), DATA_UNAVAILABLE(5), MISSING_GROUP(6),
 * VACANT_GROUP(7), NON_PARTICIPATING_GROUP(8), SCREENED_RESPONSE(9);
 * 
 * int code;
 * 
 * LidBApplicationErrorCodes(int code) { this.code = code; }
 * 
 * public int getErrorCode() { return code; }
 * 
 * }
 * 
 * enum ErrorCodeIdentifiers { NATIONAL_TCAP(1), PRIVATE_TCAP(2); int
 * errorCodeIdentifier;
 * 
 * ErrorCodeIdentifiers(int identifier) { this.errorCodeIdentifier = identifier;
 * }
 * 
 * public int getErrorCodeIdentifier() { return errorCodeIdentifier; }
 * 
 * }
 * 
 * enum LidBApplicationProblemTypes { GENERAL(1), INVOKE(2), RETURN_RESULT(3),
 * RETURN_ERROR(4), TRANSACTION_PORTION(5);
 * 
 * int problemType;
 * 
 * LidBApplicationProblemTypes(int problemType) { this.problemType =
 * problemType; }
 * 
 * public int getProblemType() { return problemType; }
 * 
 * }
 * 
 * enum LidBApplicationProblemSpecifiers { G_UNRECOGNIZED_COMPONENT(1),
 * G_INCORRECT_COMPONENT_PORTION(2), G_BADLY_STRUCTURED_COMPONENT(3),
 * G_POERTION(4), I_DUPLICATE_INVOKE_ID(1), I_UNRECOGNIZED_OPERATION(2),
 * I_INCORRECT_PARAMETER(3), I_UNRECOGNIZED_CORELATION_ID(4),
 * RR_UNRECOGNIZED_CORELATION_ID(1), RR_UNEXPECTED_RETURN_RESULT(2),
 * RR_INCORRECT_PARAMETER(3), RE_UNRECOGNIZED_CORELATION_ID(1),
 * RE_UNEXPECTED_RETURN_ERROR(2), RE_UNRECOGNIZED_ERROR(3),
 * RE_UNEXPECTED_ERROR(4), RE_INCORRECT_PARAMETER(5),
 * TP_UNRECOGNIZED_PAKAGE_TYPE(1), TP_INCORRECT_TRANSACTION_PORTION(2),
 * TP_BADLY_STRUCTURED_TRANSACTION_PORTION(3),
 * TP_UNRECOGNIZED_TRANSACTION_ID(4);
 * 
 * int problemSpecifier;
 * 
 * LidBApplicationProblemSpecifiers(int problemSpecifier) {
 * this.problemSpecifier = problemSpecifier; }
 * 
 * public int getProblemSpecifier() { return problemSpecifier; }
 * 
 * }
 */