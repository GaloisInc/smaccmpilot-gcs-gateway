-- Mavlink message names: common.xml plus smaccmpilot extensions, more or less.
-- Maintained by hand (todo, generate automatically in smaccm-mavlink)

module SMACCMPilot.GCS.Mavlink.MessageName where

messagename :: Int -> Maybe String
messagename 0xA9 = Just "DATA16"
messagename 0xAA = Just "DATA32"
messagename 0xAB = Just "DATA64"
messagename 0xAD = Just "ALT_HOLD_DEBUG"
messagename 0xAE = Just "VEHICLE_RADIO"
messagename 0xAF = Just "GCS_RADIO"
messagename 0xB9 = Just "VEH_COMMSEC"
messagename 0x00 = Just "HEARTBEAT"
messagename 0x01 = Just "SYS_STATUS"
messagename 0x02 = Just "SYSTEM_TIME"
messagename 0x04 = Just "PING"
messagename 0x05 = Just "CHANGE_OPERATOR_CONTROL"
messagename 0x06 = Just "CHANGE_OPERATOR_CONTROL_ACK"
messagename 0x07 = Just "AUTH_KEY"
messagename 0x0B = Just "SET_MODE"
messagename 0x14 = Just "PARAM_REQUEST_READ"
messagename 0x15 = Just "PARAM_REQUEST_LIST"
messagename 0x16 = Just "PARAM_VALUE"
messagename 0x17 = Just "PARAM_SET"
messagename 0x18 = Just "GPS_RAW_INT"
messagename 0x1A = Just "SCALED_IMU"
messagename 0x1B = Just "RAW_IMU"
messagename 0x1C = Just "RAW_PRESSURE"
messagename 0x1D = Just "SCALED_PRESSURE"
messagename 0x1E = Just "ATTITUDE"
messagename 0x1F = Just "ATTITUDE_QUATERNION"
messagename 0x20 = Just "LOCAL_POSITION_NED"
messagename 0x21 = Just "GLOBAL_POSITION_INT"
messagename 0x22 = Just "RC_CHANNELS_SCALED"
messagename 0x23 = Just "RC_CHANNELS_RAW"
messagename 0x24 = Just "SERVO_OUTPUT_RAW"
messagename 0x25 = Just "MISSION_REQUEST_PARTIAL_LIST"
messagename 0x26 = Just "MISSION_WRITE_PARTIAL_LIST"
messagename 0x27 = Just "MISSION_ITEM"
messagename 0x28 = Just "MISSION_REQUEST"
messagename 0x29 = Just "MISSION_SET_CURRENT"
messagename 0x2A = Just "MISSION_CURRENT"
messagename 0x2B = Just "MISSION_REQUEST_LIST"
messagename 0x2C = Just "MISSION_COUNT"
messagename 0x2D = Just "MISSION_CLEAR_ALL"
messagename 0x2E = Just "MISSION_ITEM_REACHED"
messagename 0x2F = Just "MISSION_ACK"
messagename 0x30 = Just "SET_GPS_GLOBAL_ORIGIN"
messagename 0x31 = Just "GPS_GLOBAL_ORIGIN"
messagename 0x32 = Just "SET_LOCAL_POSITION_SETPOINT"
messagename 0x33 = Just "LOCAL_POSITION_SETPOINT"
messagename 0x34 = Just "GLOBAL_POSITION_SETPOINT_INT"
messagename 0x35 = Just "SET_GLOBAL_POSITION_SETPOINT_INT"
messagename 0x36 = Just "SAFETY_SET_ALLOWED_AREA"
messagename 0x37 = Just "SAFETY_ALLOWED_AREA"
messagename 0x38 = Just "SET_ROLL_PITCH_YAW_THRUST"
messagename 0x39 = Just "SET_ROLL_PITCH_YAW_SPEED_THRUST"
messagename 0x3A = Just "ROLL_PITCH_YAW_THRUST_SETPOINT"
messagename 0x3B = Just "ROLL_PITCH_YAW_SPEED_THRUST_SETPOINT"
messagename 0x3C = Just "SET_QUAD_MOTORS_SETPOINT"
messagename 0x3D = Just "SET_QUAD_SWARM_ROLL_PITCH_YAW_THRUST"
messagename 0x3E = Just "NAV_CONTROLLER_OUTPUT"
messagename 0x3F = Just "SET_QUAD_SWARM_LED_ROLL_PITCH_YAW_THRUST"
messagename 0x40 = Just "STATE_CORRECTION"
messagename 0x42 = Just "REQUEST_DATA_STREAM"
messagename 0x43 = Just "DATA_STREAM"
messagename 0x45 = Just "MANUAL_CONTROL"
messagename 0x46 = Just "RC_CHANNELS_OVERRIDE"
messagename 0x4A = Just "VFR_HUD"
messagename 0x4C = Just "COMMAND_LONG"
messagename 0x4D = Just "COMMAND_ACK"
messagename 0x50 = Just "ROLL_PITCH_YAW_RATES_THRUST_SETPOINT"
messagename 0x51 = Just "MANUAL_SETPOINT"
messagename 0x59 = Just "LOCAL_POSITION_NED_SYSTEM_GLOBAL_OFFSET"
messagename 0x5A = Just "HIL_STATE"
messagename 0x5B = Just "HIL_CONTROLS"
messagename 0x5C = Just "HIL_RC_INPUTS_RAW"
messagename 0x64 = Just "OPTICAL_FLOW"
messagename 0x65 = Just "GLOBAL_VISION_POSITION_ESTIMATE"
messagename 0x66 = Just "VISION_POSITION_ESTIMATE"
messagename 0x67 = Just "VISION_SPEED_ESTIMATE"
messagename 0x68 = Just "VICON_POSITION_ESTIMATE"
messagename 0x69 = Just "HIGHRES_IMU"
messagename 0x6A = Just "OMNIDIRECTIONAL_FLOW"
messagename 0x93 = Just "BATTERY_STATUS"
messagename 0x94 = Just "SETPOINT_8DOF"
messagename 0x95 = Just "SETPOINT_6DOF"
messagename 0xF9 = Just "MEMORY_VECT"
messagename 0xFA = Just "DEBUG_VECT"
messagename 0xFB = Just "NAMED_VALUE_FLOAT"
messagename 0xFC = Just "NAMED_VALUE_INT"
messagename 0xFD = Just "STATUSTEXT"
messagename 0xFE = Just "DEBUG"
messagename    _ = Nothing
