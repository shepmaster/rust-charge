use paste::paste;
use rust_ocpp::v1_6::messages::*;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use snafu::prelude::*;
use std::collections::BTreeMap;

pub use rust_ocpp::v1_6::{
    messages::{
        authorize::*, boot_notification::*, change_configuration::*, get_configuration::*,
        heart_beat::*, meter_values::*, remote_start_transaction::*, remote_stop_transaction::*,
        reset::*, start_transaction::*, status_notification::*, stop_transaction::*,
        trigger_message::*,
    },
    types::*,
};

// Renaming to be consistent
use heart_beat as heartbeat;

#[derive(Debug, Snafu)]
pub struct OriginalType<T> {
    value: T,
}

macro_rules! request_response {
    (
        $(#[$req_meta:meta])*
        $req_vis:vis enum $req_name:ty;

        $(#[$resp_meta:meta])*
        $resp_vis:vis enum $resp_name:ty;

        [
            $($name:ident,)*
        ]
    ) => {
        paste! {
            $(#[$req_meta])*
            $req_vis enum $req_name {
                $(
                    $name([< $name:snake >]::[< $name Request >]),
                )*
            }

            $(
                impl From<[< $name:snake >]::[< $name Request >]> for $req_name {
                    fn from(value: [< $name:snake >]::[< $name Request >]) -> Self {
                        $req_name::$name(value)
                    }
                }

                impl TryFrom<$req_name> for [< $name:snake >]::[< $name Request >] {
                    type Error = OriginalType<$req_name>;

                    fn try_from(value: $req_name) -> Result<Self, Self::Error> {
                        match value {
                            $req_name::$name(value) => Ok(value),
                            value => OriginalTypeSnafu { value }.fail(),
                        }
                    }
                }
            )*

            $(#[$resp_meta])*
            $resp_vis enum $resp_name {
                $(
                    $name([< $name:snake >]::[< $name Response >]),
                )*
            }

            $(
                impl From<[< $name:snake >]::[< $name Response >]> for $resp_name {
                    fn from(value: [< $name:snake >]::[< $name Response >]) -> Self {
                        $resp_name::$name(value)
                    }
                }

                impl TryFrom<$resp_name> for [< $name:snake >]::[< $name Response >] {
                    type Error = OriginalType<$resp_name>;

                    fn try_from(value: $resp_name) -> Result<Self, Self::Error> {
                        match value {
                            $resp_name::$name(v) => Ok(v),
                            value => OriginalTypeSnafu { value }.fail(),
                        }
                    }
                }
            )*
        }
    };
}

request_response! {
    #[derive(Debug, Deserialize, Serialize)]
    pub enum RequestFromChargePoint;

    #[derive(Debug, Deserialize, Serialize)]
    #[serde(untagged)]
    pub enum ResponseToChargePoint;

    [
        Authorize,
        BootNotification,
        DataTransfer,
        DiagnosticsStatusNotification,
        FirmwareStatusNotification,
        Heartbeat,
        MeterValues,
        StartTransaction,
        StatusNotification,
        StopTransaction,
    ]
}

request_response! {
    #[derive(Debug, Deserialize, Serialize)]
    pub enum RequestFromCentralSystem;

    #[derive(Debug, Deserialize, Serialize)]
    #[serde(untagged)]
    pub enum ResponseToCentralSystem;

    [
        CancelReservation,
        ChangeAvailability,
        ChangeConfiguration,
        ClearCache,
        ClearChargingProfile,
        GetCompositeSchedule,
        GetConfiguration,
        GetDiagnostics,
        GetLocalListVersion,
        RemoteStartTransaction,
        RemoteStopTransaction,
        ReserveNow,
        Reset,
        SendLocalList,
        SetChargingProfile,
        TriggerMessage,
        UnlockConnector,
        UpdateFirmware,
    ]
}

#[derive(Debug, PartialEq)]
pub struct Call<T> {
    pub unique_id: String,
    pub payload: T,
}

impl<T> Call<T> {
    const CODE: u8 = 2;
}

impl<'de, T> Deserialize<'de> for Call<T>
where
    T: DeserializeOwned,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        type Packet<'a> = (u8, String, String, &'a serde_json::value::RawValue);
        let (code, unique_id, tag, content) = <Packet<'_>>::deserialize(deserializer)?;
        if code != Self::CODE {
            return Err(D::Error::custom(format!("Code must be {}", Self::CODE)));
        }

        let retagged = serde_json::json!({ tag: content });
        let payload = serde_json::from_value(retagged).map_err(D::Error::custom)?;

        Ok(Call { unique_id, payload })
    }
}

impl<T> Serialize for Call<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::Error;
        use serde_json::Value;

        let content = serde_json::to_value(&self.payload).map_err(S::Error::custom)?;

        let Value::Object(o) = content else {
            return Err(S::Error::custom(
                "Did not serialize payload as an externally tagged enum (not an object)",
            ));
        };
        if o.len() != 1 {
            return Err(S::Error::custom(
                "Did not serialize payload as an externally tagged enum (not exactly 1 key)",
            ));
        }
        let (key, content) = o.into_iter().next().unwrap();

        (Self::CODE, &self.unique_id, key, content).serialize(serializer)
    }
}

#[derive(Debug, PartialEq, Serialize)]
#[serde(untagged)]
pub enum CallResponse<T> {
    Ok(CallResult<T>),
    Err(CallError),
}

#[derive(Debug, PartialEq)]
pub struct CallResult<T> {
    pub unique_id: String,
    pub payload: T,
}

impl<T> CallResult<T> {
    const CODE: u8 = 3;
}

impl<T> Serialize for CallResult<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (Self::CODE, &self.unique_id, &self.payload).serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for CallResult<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let (code, unique_id, payload) = <(u8, String, T)>::deserialize(deserializer)?;
        assert_eq!(code, Self::CODE);
        Ok(Self { unique_id, payload })
    }
}

#[derive(Debug, PartialEq)]
pub struct CallError {
    pub unique_id: String,
    pub error_code: u8, // todo enum
    pub error_description: String,
    pub error_details: BTreeMap<String, String>,
}

impl CallError {
    const CODE: u8 = 4;
}

impl Serialize for CallError {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (
            Self::CODE,
            &self.unique_id,
            &self.error_code,
            &self.error_description,
            &self.error_details,
        )
            .serialize(serializer)
    }
}

#[cfg(test)]
mod test {
    use chrono::TimeZone;

    use super::*;

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    enum BootNotificationOnly {
        BootNotification(BootNotificationRequest),
    }

    fn boot_notification_value() -> serde_json::Value {
        serde_json::json!([
            2,
            "19223201",
            "BootNotification",
            {
                "chargeBoxSerialNumber": "charge box serial number",
                "chargePointModel": "GRS-*",
                "chargePointSerialNumber": "GRS- 40110005be00",
                "chargePointVendor": "United Chargers",
                "firmwareVersion": "05.622:9DE5:8BE8",
                "iccid": "-62",
                "imsi": "1",
                "meterSerialNumber": "meter serial number",
                "meterType": "SW",
            },
        ])
    }

    fn boot_notification_call() -> Call<BootNotificationOnly> {
        Call {
            unique_id: "19223201".into(),
            payload: BootNotificationOnly::BootNotification(BootNotificationRequest {
                charge_box_serial_number: Some("charge box serial number".into()),
                charge_point_model: "GRS-*".into(),
                charge_point_serial_number: Some("GRS- 40110005be00".into()),
                charge_point_vendor: "United Chargers".into(),
                firmware_version: Some("05.622:9DE5:8BE8".into()),
                iccid: Some("-62".into()),
                imsi: Some("1".into()),
                meter_serial_number: Some("meter serial number".into()),
                meter_type: Some("SW".into()),
            }),
        }
    }

    #[test]
    fn call_deserialization() {
        let input = serde_json::to_string(&boot_notification_value()).unwrap();
        let actual = serde_json::from_str::<Call<BootNotificationOnly>>(&input).unwrap();
        assert_eq!(actual, boot_notification_call());
    }

    #[test]
    fn call_serialization() {
        let input = boot_notification_call();
        let actual = serde_json::to_value(input).unwrap();
        assert_eq!(actual, boot_notification_value());
    }

    #[test]
    fn call_result_serialization() {
        let input = CallResult {
            unique_id: "19223201".into(),
            payload: BootNotificationResponse {
                current_time: chrono::Utc.timestamp_nanos(1359752012486000000),
                interval: 300,
                status: RegistrationStatus::Accepted,
            },
        };
        let actual = serde_json::to_value(input).unwrap();
        assert_eq!(
            actual,
            serde_json::json!([
                3,
                "19223201",
                {
                    "currentTime": "2013-02-01T20:53:32.486Z",
                    "interval": 300,
                    "status": "Accepted",
                },
            ])
        );
    }
}
