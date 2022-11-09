package com.agnity.mphdata.common;

import java.io.Serializable;

public class CallPickupData implements Serializable {
    private static final long serialVersionUID = -8950386471372036222L;

    private CallData ringingLegCallData;
    private CallData pickupLegCallData;

    public CallPickupData(CallData ringingLegCallData, CallData pickupLegCallData) {
        this.ringingLegCallData = ringingLegCallData;
        this.pickupLegCallData = pickupLegCallData;
    }

    @Override
    public int hashCode() {
        int result = getRingingLegCallData().hashCode();
        result = 31 * result + getPickupLegCallData().hashCode();
        return result;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
        	return true;
        }
        if (o == null || getClass() != o.getClass()) {
        	return false;
        }

        CallPickupData that = (CallPickupData) o;

        if (!getRingingLegCallData().equals(that.getRingingLegCallData())) {
        	return false;
        }
        return getPickupLegCallData().equals(that.getPickupLegCallData());
    }

    public CallData getRingingLegCallData() {
        return ringingLegCallData;
    }

    public void setRingingLegCallData(CallData ringingLegCallData) {
        this.ringingLegCallData = ringingLegCallData;
    }

    public CallData getPickupLegCallData() {
        return pickupLegCallData;
    }

    public void setPickupLegCallData(CallData pickupLegCallData) {
        this.pickupLegCallData = pickupLegCallData;
    }
}
