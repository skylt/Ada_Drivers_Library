------------------------------------------------------------------------------
--                              Certyflie                                   --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  MPU6050 I2C device class package
--  Based on MPU9250 device class package
--  Created by Sebastien Bardot
--  Reworked by Jessica Larrieu and Brendan Harley

with Ada.Real_Time;       use Ada.Real_Time;
with Interfaces;          use Interfaces;

with HAL;                 use HAL;
with HAL.I2C;             use HAL.I2C;


package MPU6050 is

	type MPU6050_AD0_Pin_State is (High, Low);
	--  The MPU6050 has a pin that can be set to high or low level to change
	--  its I2C address.

	--  Types and subtypes
	type MPU6050_Device
	  (Port        : HAL.I2C.Any_I2C_Port;
	   I2C_AD0_Pin : MPU6050_AD0_Pin_State)
	is record
		Is_Init : Boolean := False;
		Address : UInt10;
	end record;

	--  Type reprensnting all the different clock sources of the MPU6050.
	--  See the MPU6050 register map section 4.28 for more details.
	type MPU6050_Clock_Source is
	  (Internal_Clk,
	   X_Gyro_Clk,
	   Y_Gyro_Clk,
	   Z_Gyro_Clk,
	   External_32K_Clk,
	   External_19M_Clk,
	   Reserved_Clk,
	   Stop_Clk);
	for MPU6050_Clock_Source use
	  (Internal_Clk     => 16#00#,
	   X_Gyro_Clk       => 16#01#,
	   Y_Gyro_Clk       => 16#02#,
	   Z_Gyro_Clk       => 16#03#,
	   External_32K_Clk => 16#04#,
	   External_19M_Clk => 16#05#,
	   Reserved_Clk     => 16#06#,
  	   Stop_Clk         => 16#07#);
	for MPU6050_Clock_Source'Size use 3;

	--  Type representing the allowed full scale ranges
	--  for MPU6050 gyroscope.
	--  See the MPU6050 register map section 4.4   for more details.
	type MPU6050_FS_Gyro_Range is
	  (MPU6050_Gyro_FS_250,
	   MPU6050_Gyro_FS_500,
	   MPU6050_Gyro_FS_1000,
	   MPU6050_Gyro_FS_2000);
	for MPU6050_FS_Gyro_Range use
	  (MPU6050_Gyro_FS_250  => 16#00#,
	   MPU6050_Gyro_FS_500  => 16#01#,
	   MPU6050_Gyro_FS_1000 => 16#02#,
	   MPU6050_Gyro_FS_2000 => 16#03#);
	for MPU6050_FS_Gyro_Range'Size use 2;

	--  Type representing the allowed full scale ranges
	--  for MPU6050 accelerometer.
	--  See the MPU6050 register map section 4.5 for more details.
	type MPU6050_FS_Accel_Range is
	  (MPU6050_Accel_FS_2,
	   MPU6050_Accel_FS_4,
	   MPU6050_Accel_FS_8,
	   MPU6050_Accel_FS_16);
	for MPU6050_FS_Accel_Range use
	  (MPU6050_Accel_FS_2  => 16#00#,
	   MPU6050_Accel_FS_4  => 16#01#,
	   MPU6050_Accel_FS_8  => 16#02#,
	   MPU6050_Accel_FS_16 => 16#03#);
	for MPU6050_FS_Accel_Range'Size use 2;

	--  Type representing the filter of accelerometer and gyroscope values
	--  See the MPU6050 register map section 4.3 for more details.
	type MPU6050_DLPF_Bandwidth_Mode is
	  (MPU6050_DLPF_BW_256,
	   MPU6050_DLPF_BW_188,
	   MPU6050_DLPF_BW_98,
	   MPU6050_DLPF_BW_42,
	   MPU6050_DLPF_BW_20,
	   MPU6050_DLPF_BW_10,
	   MPU6050_DLPF_BW_5);
	for MPU6050_DLPF_Bandwidth_Mode use
	  (MPU6050_DLPF_BW_256 => 16#00#,
	   MPU6050_DLPF_BW_188 => 16#01#,
	   MPU6050_DLPF_BW_98  => 16#02#,
	   MPU6050_DLPF_BW_42  => 16#03#,
	   MPU6050_DLPF_BW_20  => 16#04#,
	   MPU6050_DLPF_BW_10  => 16#05#,
	   MPU6050_DLPF_BW_5   => 16#06#);
	for MPU6050_DLPF_Bandwidth_Mode'Size use 3;

	--  Use to convert MPU6050 registers in degrees (gyro) and G (acc).
	MPU6050_DEG_PER_LSB_250  : constant := (2.0 * 250.0) / 65536.0;
	MPU6050_DEG_PER_LSB_500  : constant := (2.0 * 500.0) / 65536.0;
	MPU6050_DEG_PER_LSB_1000 : constant := (2.0 * 1000.0) / 65536.0;
	MPU6050_DEG_PER_LSB_2000 : constant := (2.0 * 2000.0) / 65536.0;
	MPU6050_G_PER_LSB_2      : constant := (2.0 * 2.0) / 65536.0;
	MPU6050_G_PER_LSB_4      : constant := (2.0 * 4.0) / 65536.0;
	MPU6050_G_PER_LSB_8      : constant := (2.0 * 8.0) / 65536.0;
	MPU6050_G_PER_LSB_16     : constant := (2.0 * 16.0) / 65536.0;

	--  Procedures and functions

	function MPU6050_Create
	  (MPU6050_Port        : HAL.I2C.Any_I2C_Port;
	   MPU6050_I2C_AD0_Pin : MPU6050_AD0_Pin_State) return MPU6050_Device;

	--  Initialize the MPU6050 Device via I2C.
	procedure MPU6050_Init (Device : in out MPU6050_Device);

	--  Test if the MPU6050 is initialized and connected.
	function MPU6050_Test (Device : MPU6050_Device) return Boolean;

	--  Test if we are connected to MPU6050 via I2C.
	function MPU6050_Test_Connection (Device : MPU6050_Device) return Boolean;

	type Test_Reporter is access
	  procedure (Msg : String; Has_Succeeded : out Boolean);

	--  MPU6050 self test.
	function MPU6050_Self_Test
	  (Device    : in out MPU6050_Device;
	   Do_Report : Boolean;
	   Reporter  : Test_Reporter) return Boolean;

	--  Reset the MPU6050 device.
	--  A small delay of ~50ms may be desirable after triggering a reset.
	procedure MPU6050_Reset (Device : in out MPU6050_Device);

	--  Get raw 6-axis motion sensor readings (accel/gyro).
	--  Retrieves all currently available motion sensor values.
	procedure MPU6050_Get_Motion_6
	  (Device : MPU6050_Device;
	   Acc_X  : out Integer_16;
	   Acc_Y  : out Integer_16;
	   Acc_Z  : out Integer_16;
	   Gyro_X : out Integer_16;
	   Gyro_Y : out Integer_16;
	   Gyro_Z : out Integer_16);

	--  Set clock source setting.
	--  3 bits allowed to choose the source. The different
	--  clock sources are enumerated in the MPU6050 register map.
	procedure MPU6050_Set_Clock_Source
	  (Device       : in out MPU6050_Device;
	   Clock_Source : MPU6050_Clock_Source);

	--  Set digital low-pass filter configuration.
	procedure MPU6050_Set_DLPF_Mode
	  (Device    : in out MPU6050_Device;
	   DLPF_Mode : MPU6050_DLPF_Bandwidth_Mode);

	--  Set full-scale gyroscope range.
	procedure MPU6050_Set_Full_Scale_Gyro_Range
	  (Device   : in out MPU6050_Device;
	   FS_Range : MPU6050_FS_Gyro_Range);

	--  Set full-scale acceler range.
	procedure MPU6050_Set_Full_Scale_Accel_Range
	  (Device   : in out MPU6050_Device;
	   FS_Range : MPU6050_FS_Accel_Range);

	--  Set I2C bypass enabled status.
	--  When this bit is equal to 1 and I2C_MST_EN (Register 106 bit[5]) is
	--  equal to 0, the host application processor
	--  will be able to directly access the
	--  auxiliary I2C bus of the MPU-60X0. When this bit is equal to 0,
	--  the host application processor will not be able to directly
	--  access the auxiliary I2C bus of the MPU-60X0 regardless of the state
	--  of I2C_MST_EN (Register 106 bit[5]).
	procedure MPU6050_Set_I2C_Bypass_Enabled
	  (Device : in out MPU6050_Device;
	   Value  : Boolean);

	--  Set interrupts enabled status.
	procedure MPU6050_Set_Int_Enabled
	  (Device : in out MPU6050_Device;
	   Value  : Boolean);

	--  Set gyroscope sample rate divider
	procedure MPU6050_Set_Rate
	  (Device   : in out MPU6050_Device;
	   Rate_Div : Uint8);

	--  Set sleep mode status.
	procedure MPU6050_Set_Sleep_Enabled
	  (Device : in out MPU6050_Device;
	   Value  : Boolean);

	--  Set temperature sensor enabled status.
	procedure MPU6050_Set_Temp_Sensor_Enabled
	  (Device : in out MPU6050_Device;
	   Value  : Boolean);

	--  Get temperature sensor enabled status.
	function MPU6050_Get_Temp_Sensor_Enabled
	  (Device : MPU6050_Device) return Boolean;

private



	subtype T_Bit_Pos_8 is Natural  range 0 .. 7;
	subtype T_Bit_Pos_16 is Natural range 0 .. 15;

	--  Global variables and constants

	--  MPU6050 Device ID. Use to test if we are connected via I2C
	MPU6050_DEVICE_ID        : constant := 16#71#;  -- The defaut value is 16#68#
	--  Address pin low (GND), default for InvenSense evaluation board
	MPU6050_ADDRESS_AD0_LOW  : constant := 16#68#;
	--  Address pin high (VCC)
	MPU6050_ADDRESS_AD0_HIGH : constant := 16#69#;

	MPU6050_STARTUP_TIME_MS  : constant Time_Span := Milliseconds (1_000);

	--  MPU6050 register adresses and other defines

	--  MPU6050_REV_C4_ES : constant := 16#14#;
	--  MPU6050_REV_C5_ES : constant := 16#15#;
	--  MPU6050_REV_D6_ES : constant := 16#16#;
	--  MPU6050_REV_D7_ES : constant := 16#17#;
	--  MPU6050_REV_D8_ES : constant := 16#18#;
	--  MPU6050_REV_C4 : constant := 16#54#;
	--  MPU6050_REV_C5 : constant := 16#55#;
	--  MPU6050_REV_D6 : constant := 16#56#;
	--  MPU6050_REV_D7 : constant := 16#57#;
	--  MPU6050_REV_D8 : constant := 16#58#;
	--  MPU6050_REV_D9 : constant := 16#59#;


	MPU6050_RA_XG_OFFS_TC         : constant := 16#00#; --  [7] PWR_MODE, [6:1] XG_OFFS_TC, [0] OTP_BNK_VLD
	MPU6050_RA_YG_OFFS_TC         : constant := 16#01#; --  [7] PWR_MODE, [6:1] YG_OFFS_TC, [0] OTP_BNK_VLD
	MPU6050_RA_ZG_OFFS_TC         : constant := 16#02#; --  [7] PWR_MODE, [6:1] ZG_OFFS_TC, [0] OTP_BNK_VLD
	MPU6050_RA_X_FINE_GAIN        : constant := 16#03#; --  [7:0] X_FINE_GAIN
	MPU6050_RA_Y_FINE_GAIN        : constant := 16#04#; --  [7:0] Y_FINE_GAIN
	MPU6050_RA_Z_FINE_GAIN        : constant := 16#05#; --  [7:0] Z_FINE_GAIN
	MPU6050_RA_XA_OFFS_H          : constant := 16#06#; --  [15:0] XA_OFFS
	MPU6050_RA_XA_OFFS_L_TC       : constant := 16#07#;
	MPU6050_RA_YA_OFFS_H          : constant := 16#08#; --  [15:0] YA_OFFS
	MPU6050_RA_YA_OFFS_L_TC       : constant := 16#09#;
	MPU6050_RA_ZA_OFFS_H          : constant := 16#0A#; --  [15:0] ZA_OFFS
	MPU6050_RA_ZA_OFFS_L_TC       : constant := 16#0B#;

	MPU6050_RA_ST_X               : constant := 16#0D#;
	MPU6050_RA_ST_Y               : constant := 16#0E#;
	MPU6050_RA_ST_Z               : constant := 16#0F#;
	MPU6050_RA_ST_A               : constant := 16#10#;
	MPU6050_RA_XG_OFFS_USRH       : constant := 16#13#; --  [15:0] XG_OFFS_USR
	MPU6050_RA_XG_OFFS_USRL       : constant := 16#14#;
	MPU6050_RA_YG_OFFS_USRH       : constant := 16#15#; --  [15:0] YG_OFFS_USR
	MPU6050_RA_YG_OFFS_USRL       : constant := 16#16#;
	MPU6050_RA_ZG_OFFS_USRH       : constant := 16#17#; --  [15:0] ZG_OFFS_USR
	MPU6050_RA_ZG_OFFS_USRL       : constant := 16#18#;
	MPU6050_RA_SMPLRT_DIV         : constant := 16#19#;
	MPU6050_RA_CONFIG             : constant := 16#1A#;
	MPU6050_RA_GYRO_CONFIG        : constant := 16#1B#;
	MPU6050_RA_ACCEL_CONFIG       : constant := 16#1C#;
	MPU6050_RA_FF_THR             : constant := 16#1D#;
	MPU6050_RA_FF_DUR             : constant := 16#1E#;
	MPU6050_RA_MOT_THR            : constant := 16#1F#;
	MPU6050_RA_MOT_DUR            : constant := 16#20#;
	MPU6050_RA_ZRMOT_THR          : constant := 16#21#;
	MPU6050_RA_ZRMOT_DUR          : constant := 16#22#;
	MPU6050_RA_FIFO_EN            : constant := 16#23#;
	MPU6050_RA_I2C_MST_CTRL       : constant := 16#24#;
	MPU6050_RA_I2C_SLV0_ADDR      : constant := 16#25#;
	MPU6050_RA_I2C_SLV0_REG       : constant := 16#26#;
	MPU6050_RA_I2C_SLV0_CTRL      : constant := 16#27#;
	MPU6050_RA_I2C_SLV1_ADDR      : constant := 16#28#;
	MPU6050_RA_I2C_SLV1_REG       : constant := 16#29#;
	MPU6050_RA_I2C_SLV1_CTRL      : constant := 16#2A#;
	MPU6050_RA_I2C_SLV2_ADDR      : constant := 16#2B#;
	MPU6050_RA_I2C_SLV2_REG       : constant := 16#2C#;
	MPU6050_RA_I2C_SLV2_CTRL      : constant := 16#2D#;
	MPU6050_RA_I2C_SLV3_ADDR      : constant := 16#2E#;
	MPU6050_RA_I2C_SLV3_REG       : constant := 16#2F#;
	MPU6050_RA_I2C_SLV3_CTRL      : constant := 16#30#;
	MPU6050_RA_I2C_SLV4_ADDR      : constant := 16#31#;
	MPU6050_RA_I2C_SLV4_REG       : constant := 16#32#;
	MPU6050_RA_I2C_SLV4_DO        : constant := 16#33#;
	MPU6050_RA_I2C_SLV4_CTRL      : constant := 16#34#;
	MPU6050_RA_I2C_SLV4_DI        : constant := 16#35#;
	MPU6050_RA_I2C_MST_STATUS     : constant := 16#36#;
	MPU6050_RA_INT_PIN_CFG        : constant := 16#37#;
	MPU6050_RA_INT_ENABLE         : constant := 16#38#;
	MPU6050_RA_DMP_INT_STATUS     : constant := 16#39#;
	MPU6050_RA_INT_STATUS         : constant := 16#3A#;
	MPU6050_RA_ACCEL_XOUT_H       : constant := 16#3B#;
	MPU6050_RA_ACCEL_XOUT_L       : constant := 16#3C#;
	MPU6050_RA_ACCEL_YOUT_H       : constant := 16#3D#;
	MPU6050_RA_ACCEL_YOUT_L       : constant := 16#3E#;
	MPU6050_RA_ACCEL_ZOUT_H       : constant := 16#3F#;
	MPU6050_RA_ACCEL_ZOUT_L       : constant := 16#40#;
	MPU6050_RA_TEMP_OUT_H         : constant := 16#41#;
	MPU6050_RA_TEMP_OUT_L         : constant := 16#42#;
	MPU6050_RA_GYRO_XOUT_H        : constant := 16#43#;
	MPU6050_RA_GYRO_XOUT_L        : constant := 16#44#;
	MPU6050_RA_GYRO_YOUT_H        : constant := 16#45#;
	MPU6050_RA_GYRO_YOUT_L        : constant := 16#46#;
	MPU6050_RA_GYRO_ZOUT_H        : constant := 16#47#;
	MPU6050_RA_GYRO_ZOUT_L        : constant := 16#48#;
	MPU6050_RA_EXT_SENS_DATA_00   : constant := 16#49#;
	MPU6050_RA_EXT_SENS_DATA_01   : constant := 16#4A#;
	MPU6050_RA_EXT_SENS_DATA_02   : constant := 16#4B#;
	MPU6050_RA_EXT_SENS_DATA_03   : constant := 16#4C#;
	MPU6050_RA_EXT_SENS_DATA_04   : constant := 16#4D#;
	MPU6050_RA_EXT_SENS_DATA_05   : constant := 16#4E#;
	MPU6050_RA_EXT_SENS_DATA_06   : constant := 16#4F#;
	MPU6050_RA_EXT_SENS_DATA_07   : constant := 16#50#;
	MPU6050_RA_EXT_SENS_DATA_08   : constant := 16#51#;
	MPU6050_RA_EXT_SENS_DATA_09   : constant := 16#52#;
	MPU6050_RA_EXT_SENS_DATA_10   : constant := 16#53#;
	MPU6050_RA_EXT_SENS_DATA_11   : constant := 16#54#;
	MPU6050_RA_EXT_SENS_DATA_12   : constant := 16#55#;
	MPU6050_RA_EXT_SENS_DATA_13   : constant := 16#56#;
	MPU6050_RA_EXT_SENS_DATA_14   : constant := 16#57#;
	MPU6050_RA_EXT_SENS_DATA_15   : constant := 16#58#;
	MPU6050_RA_EXT_SENS_DATA_16   : constant := 16#59#;
	MPU6050_RA_EXT_SENS_DATA_17   : constant := 16#5A#;
	MPU6050_RA_EXT_SENS_DATA_18   : constant := 16#5B#;
	MPU6050_RA_EXT_SENS_DATA_19   : constant := 16#5C#;
	MPU6050_RA_EXT_SENS_DATA_20   : constant := 16#5D#;
	MPU6050_RA_EXT_SENS_DATA_21   : constant := 16#5E#;
	MPU6050_RA_EXT_SENS_DATA_22   : constant := 16#5F#;
	MPU6050_RA_EXT_SENS_DATA_23   : constant := 16#60#;
	MPU6050_RA_MOT_DETECT_STATUS  : constant := 16#61#;
	MPU6050_RA_I2C_SLV0_DO        : constant := 16#63#;
	MPU6050_RA_I2C_SLV1_DO        : constant := 16#64#;
	MPU6050_RA_I2C_SLV2_DO        : constant := 16#65#;
	MPU6050_RA_I2C_SLV3_DO        : constant := 16#66#;
	MPU6050_RA_I2C_MST_DELAY_CTRL : constant := 16#67#;
	MPU6050_RA_SIGNAL_PATH_RESET  : constant := 16#68#;
	MPU6050_RA_MOT_DETECT_CTRL    : constant := 16#69#;
	MPU6050_RA_USER_CTRL          : constant := 16#6A#;
	MPU6050_RA_PWR_MGMT_1         : constant := 16#6B#;
	MPU6050_RA_PWR_MGMT_2         : constant := 16#6C#;
	MPU6050_RA_BANK_SEL           : constant := 16#6D#;
	MPU6050_RA_MEM_START_ADDR     : constant := 16#6E#;
	MPU6050_RA_MEM_R_W            : constant := 16#6F#;
	MPU6050_RA_DMP_CFG_1          : constant := 16#70#;
	MPU6050_RA_DMP_CFG_2          : constant := 16#71#;
	MPU6050_RA_FIFO_COUNTH        : constant := 16#72#;
	MPU6050_RA_FIFO_COUNTL        : constant := 16#73#;
	MPU6050_RA_FIFO_R_W           : constant := 16#74#;
	MPU6050_RA_WHO_AM_I           : constant := 16#75#;

	MPU6050_TC_PWR_MODE_BIT    : constant := 7;
	MPU6050_TC_OFFSET_BIT      : constant := 6;
	MPU6050_TC_OFFSET_LENGTH   : constant := 6;
	MPU6050_TC_OTP_BNK_VLD_BIT : constant := 0;

	MPU6050_VDDIO_LEVEL_VLOGIC : constant := 0;
	MPU6050_VDDIO_LEVEL_VDD    : constant := 1;

	MPU6050_CFG_EXT_SYNC_SET_BIT    : constant := 5;
	MPU6050_CFG_EXT_SYNC_SET_LENGTH : constant := 3;
	MPU6050_CFG_DLPF_CFG_BIT        : constant := 2;
	MPU6050_CFG_DLPF_CFG_LENGTH     : constant := 3;

	MPU6050_EXT_SYNC_DISABLED     : constant := 16#0#;
	MPU6050_EXT_SYNC_TEMP_OUT_L   : constant := 16#1#;
	MPU6050_EXT_SYNC_GYRO_XOUT_L  : constant := 16#2#;
	MPU6050_EXT_SYNC_GYRO_YOUT_L  : constant := 16#3#;
	MPU6050_EXT_SYNC_GYRO_ZOUT_L  : constant := 16#4#;
	MPU6050_EXT_SYNC_ACCEL_XOUT_L : constant := 16#5#;
	MPU6050_EXT_SYNC_ACCEL_YOUT_L : constant := 16#6#;
	MPU6050_EXT_SYNC_ACCEL_ZOUT_L : constant := 16#7#;

	MPU6050_GCONFIG_FS_SEL_BIT    : constant := 4;
	MPU6050_GCONFIG_FS_SEL_LENGTH : constant := 2;

	MPU6050_ACONFIG_XA_ST_BIT        : constant := 7;
	MPU6050_ACONFIG_YA_ST_BIT        : constant := 6;
	MPU6050_ACONFIG_ZA_ST_BIT        : constant := 5;
	MPU6050_ACONFIG_AFS_SEL_BIT      : constant := 4;
	MPU6050_ACONFIG_AFS_SEL_LENGTH   : constant := 2;
	MPU6050_ACONFIG_ACCEL_HPF_BIT    : constant := 2;
	MPU6050_ACONFIG_ACCEL_HPF_LENGTH : constant := 3;

	MPU6050_DHPF_RESET : constant := 16#00#;
	MPU6050_DHPF_5     : constant := 16#01#;
	MPU6050_DHPF_2P5   : constant := 16#02#;
	MPU6050_DHPF_1P25  : constant := 16#03#;
	MPU6050_DHPF_0P63  : constant := 16#04#;
	MPU6050_DHPF_HOLD  : constant := 16#07#;

	MPU6050_TEMP_FIFO_EN_BIT  : constant := 7;
	MPU6050_XG_FIFO_EN_BIT    : constant := 6;
	MPU6050_YG_FIFO_EN_BIT    : constant := 5;
	MPU6050_ZG_FIFO_EN_BIT    : constant := 4;
	MPU6050_ACCEL_FIFO_EN_BIT : constant := 3;
	MPU6050_SLV2_FIFO_EN_BIT  : constant := 2;
	MPU6050_SLV1_FIFO_EN_BIT  : constant := 1;
	MPU6050_SLV0_FIFO_EN_BIT  : constant := 0;

	MPU6050_MULT_MST_EN_BIT    : constant := 7;
	MPU6050_WAIT_FOR_ES_BIT    : constant := 6;
	MPU6050_SLV_3_FIFO_EN_BIT  : constant := 5;
	MPU6050_I2C_MST_P_NSR_BIT  : constant := 4;
	MPU6050_I2C_MST_CLK_BIT    : constant := 3;
	MPU6050_I2C_MST_CLK_LENGTH : constant := 4;

	MPU6050_CLOCK_DIV_348 : constant := 16#0#;
	MPU6050_CLOCK_DIV_333 : constant := 16#1#;
	MPU6050_CLOCK_DIV_320 : constant := 16#2#;
	MPU6050_CLOCK_DIV_308 : constant := 16#3#;
	MPU6050_CLOCK_DIV_296 : constant := 16#4#;
	MPU6050_CLOCK_DIV_286 : constant := 16#5#;
	MPU6050_CLOCK_DIV_276 : constant := 16#6#;
	MPU6050_CLOCK_DIV_267 : constant := 16#7#;
	MPU6050_CLOCK_DIV_258 : constant := 16#8#;
	MPU6050_CLOCK_DIV_500 : constant := 16#9#;
	MPU6050_CLOCK_DIV_471 : constant := 16#A#;
	MPU6050_CLOCK_DIV_444 : constant := 16#B#;
	MPU6050_CLOCK_DIV_421 : constant := 16#C#;
	MPU6050_CLOCK_DIV_400 : constant := 16#D#;
	MPU6050_CLOCK_DIV_381 : constant := 16#E#;
	MPU6050_CLOCK_DIV_364 : constant := 16#F#;

	MPU6050_I2C_SLV_RW_BIT       : constant := 7;
	MPU6050_I2C_SLV_ADDR_BIT     : constant := 6;
	MPU6050_I2C_SLV_ADDR_LENGTH  : constant := 7;
	MPU6050_I2C_SLV_EN_BIT       : constant := 7;
	MPU6050_I2C_SLV_Uint8_SW_BIT : constant := 6;
	MPU6050_I2C_SLV_REG_DIS_BIT  : constant := 5;
	MPU6050_I2C_SLV_GRP_BIT      : constant := 4;
	MPU6050_I2C_SLV_LEN_BIT      : constant := 3;
	MPU6050_I2C_SLV_LEN_LENGTH   : constant := 4;

	MPU6050_I2C_SLV4_RW_BIT         : constant := 7;
	MPU6050_I2C_SLV4_ADDR_BIT       : constant := 6;
	MPU6050_I2C_SLV4_ADDR_LENGTH    : constant := 7;
	MPU6050_I2C_SLV4_EN_BIT         : constant := 7;
	MPU6050_I2C_SLV4_INT_EN_BIT     : constant := 6;
	MPU6050_I2C_SLV4_REG_DIS_BIT    : constant := 5;
	MPU6050_I2C_SLV4_MST_DLY_BIT    : constant := 4;
	MPU6050_I2C_SLV4_MST_DLY_LENGTH : constant := 5;

	MPU6050_MST_PASS_THROUGH_BIT  : constant := 7;
	MPU6050_MST_I2C_SLV4_DONE_BIT : constant := 6;
	MPU6050_MST_I2C_LOST_ARB_BIT  : constant := 5;
	MPU6050_MST_I2C_SLV4_NACK_BIT : constant := 4;
	MPU6050_MST_I2C_SLV3_NACK_BIT : constant := 3;
	MPU6050_MST_I2C_SLV2_NACK_BIT : constant := 2;
	MPU6050_MST_I2C_SLV1_NACK_BIT : constant := 1;
	MPU6050_MST_I2C_SLV0_NACK_BIT : constant := 0;

	MPU6050_INTCFG_INT_LEVEL_BIT       : constant := 7;
	MPU6050_INTCFG_INT_OPEN_BIT        : constant := 6;
	MPU6050_INTCFG_LATCH_INT_EN_BIT    : constant := 5;
	MPU6050_INTCFG_INT_RD_CLEAR_BIT    : constant := 4;
	MPU6050_INTCFG_FSYNC_INT_LEVEL_BIT : constant := 3;
	MPU6050_INTCFG_FSYNC_INT_EN_BIT    : constant := 2;
	MPU6050_INTCFG_I2C_BYPASS_EN_BIT   : constant := 1;
	MPU6050_INTCFG_CLKOUT_EN_BIT       : constant := 0;

	MPU6050_INTMODE_ACTIVEHIGH : constant := 16#00#;
	MPU6050_INTMODE_ACTIVELOW  : constant := 16#01#;

	MPU6050_INTDRV_PUSHPULL  : constant := 16#00#;
	MPU6050_INTDRV_OPENDRAIN : constant := 16#01#;

	MPU6050_INTLATCH_50USPULSE : constant := 16#00#;
	MPU6050_INTLATCH_WAITCLEAR : constant := 16#01#;

	MPU6050_INTCLEAR_STATUSREAD : constant := 16#00#;
	MPU6050_INTCLEAR_ANYREAD    : constant := 16#01#;

	MPU6050_INTERRUPT_FF_BIT          : constant := 7;
	MPU6050_INTERRUPT_MOT_BIT         : constant := 6;
	MPU6050_INTERRUPT_ZMOT_BIT        : constant := 5;
	MPU6050_INTERRUPT_FIFO_OFLOW_BIT  : constant := 4;
	MPU6050_INTERRUPT_I2C_MST_INT_BIT : constant := 3;
	MPU6050_INTERRUPT_PLL_RDY_INT_BIT : constant := 2;
	MPU6050_INTERRUPT_DMP_INT_BIT     : constant := 1;
	MPU6050_INTERRUPT_DATA_RDY_BIT    : constant := 0;

	MPU6050_DMPINT_5_BIT : constant := 5;
	MPU6050_DMPINT_4_BIT : constant := 4;
	MPU6050_DMPINT_3_BIT : constant := 3;
	MPU6050_DMPINT_2_BIT : constant := 2;
	MPU6050_DMPINT_1_BIT : constant := 1;
	MPU6050_DMPINT_0_BIT : constant := 0;

	MPU6050_MOTION_MOT_XNEG_BIT  : constant := 7;
	MPU6050_MOTION_MOT_XPOS_BIT  : constant := 6;
	MPU6050_MOTION_MOT_YNEG_BIT  : constant := 5;
	MPU6050_MOTION_MOT_YPOS_BIT  : constant := 4;
	MPU6050_MOTION_MOT_ZNEG_BIT  : constant := 3;
	MPU6050_MOTION_MOT_ZPOS_BIT  : constant := 2;
	MPU6050_MOTION_MOT_ZRMOT_BIT : constant := 0;

	MPU6050_DELAYCTRL_DELAY_ES_SHADOW_BIT : constant := 7;
	MPU6050_DELAYCTRL_I2C_SLV4_DLY_EN_BIT : constant := 4;
	MPU6050_DELAYCTRL_I2C_SLV3_DLY_EN_BIT : constant := 3;
	MPU6050_DELAYCTRL_I2C_SLV2_DLY_EN_BIT : constant := 2;
	MPU6050_DELAYCTRL_I2C_SLV1_DLY_EN_BIT : constant := 1;
	MPU6050_DELAYCTRL_I2C_SLV0_DLY_EN_BIT : constant := 0;

	MPU6050_PATHRESET_GYRO_RESET_BIT  : constant := 2;
	MPU6050_PATHRESET_ACCEL_RESET_BIT : constant := 1;
	MPU6050_PATHRESET_TEMP_RESET_BIT  : constant := 0;

	MPU6050_DETECT_ACCEL_ON_DELAY_BIT    : constant := 5;
	MPU6050_DETECT_ACCEL_ON_DELAY_LENGTH : constant := 2;
	MPU6050_DETECT_FF_COUNT_BIT          : constant := 3;
	MPU6050_DETECT_FF_COUNT_LENGTH       : constant := 2;
	MPU6050_DETECT_MOT_COUNT_BIT         : constant := 1;
	MPU6050_DETECT_MOT_COUNT_LENGTH      : constant := 2;

	MPU6050_DETECT_DECREMENT_RESET : constant := 16#0#;
	MPU6050_DETECT_DECREMENT_1     : constant := 16#1#;
	MPU6050_DETECT_DECREMENT_2     : constant := 16#2#;
	MPU6050_DETECT_DECREMENT_4     : constant := 16#3#;

	MPU6050_USERCTRL_DMP_EN_BIT         : constant := 7;
	MPU6050_USERCTRL_FIFO_EN_BIT        : constant := 6;
	MPU6050_USERCTRL_I2C_MST_EN_BIT     : constant := 5;
	MPU6050_USERCTRL_I2C_IF_DIS_BIT     : constant := 4;
	MPU6050_USERCTRL_DMP_RESET_BIT      : constant := 3;
	MPU6050_USERCTRL_FIFO_RESET_BIT     : constant := 2;
	MPU6050_USERCTRL_I2C_MST_RESET_BIT  : constant := 1;
	MPU6050_USERCTRL_SIG_COND_RESET_BIT : constant := 0;

	MPU6050_PWR1_DEVICE_RESET_BIT : constant := 7;
	MPU6050_PWR1_SLEEP_BIT        : constant := 6;
	MPU6050_PWR1_CYCLE_BIT        : constant := 5;
	MPU6050_PWR1_TEMP_DIS_BIT     : constant := 3;
	MPU6050_PWR1_CLKSEL_BIT       : constant := 2;
	MPU6050_PWR1_CLKSEL_LENGTH    : constant := 3;

	MPU6050_CLOCK_INTERNAL   : constant := 16#00#;
	MPU6050_CLOCK_PLL_XGYRO  : constant := 16#01#;
	MPU6050_CLOCK_PLL_YGYRO  : constant := 16#02#;
	MPU6050_CLOCK_PLL_ZGYRO  : constant := 16#03#;
	MPU6050_CLOCK_PLL_EXT32K : constant := 16#04#;
	MPU6050_CLOCK_PLL_EXT19M : constant := 16#05#;
	MPU6050_CLOCK_KEEP_RESET : constant := 16#07#;

	MPU6050_PWR2_LP_WAKE_CTRL_BIT    : constant := 7;
	MPU6050_PWR2_LP_WAKE_CTRL_LENGTH : constant := 2;
	MPU6050_PWR2_STBY_XA_BIT         : constant := 5;
	MPU6050_PWR2_STBY_YA_BIT         : constant := 4;
	MPU6050_PWR2_STBY_ZA_BIT         : constant := 3;
	MPU6050_PWR2_STBY_XG_BIT         : constant := 2;
	MPU6050_PWR2_STBY_YG_BIT         : constant := 1;
	MPU6050_PWR2_STBY_ZG_BIT         : constant := 0;

	MPU6050_WAKE_FREQ_1P25 : constant := 16#0#;
	MPU6050_WAKE_FREQ_2P5  : constant := 16#1#;
	MPU6050_WAKE_FREQ_5    : constant := 16#2#;
	MPU6050_WAKE_FREQ_10   : constant := 16#3#;

	MPU6050_BANKSEL_PRFTCH_EN_BIT     : constant := 6;
	MPU6050_BANKSEL_CFG_USER_BANK_BIT : constant := 5;
	MPU6050_BANKSEL_MEM_SEL_BIT       : constant := 4;
	MPU6050_BANKSEL_MEM_SEL_LENGTH    : constant := 5;

	MPU6050_WHO_AM_I_BIT    : constant := 6;
	MPU6050_WHO_AM_I_LENGTH : constant := 6;

	MPU6050_DMP_MEMORY_BANKS      : constant := 8;
	MPU6050_DMP_MEMORY_BANK_SIZE  : constant := 256;
	MPU6050_DMP_MEMORY_CHUNK_SIZE : constant := 16;

	MPU6050_ST_GYRO_LOW           : constant := (-14.0);
	MPU6050_ST_GYRO_HIGH          : constant := 14.0;
	MPU6050_ST_ACCEL_LOW          : constant := (-14.0);
	MPU6050_ST_ACCEL_HIGH         : constant := 14.0;

	MPU6050_FT_ACC : constant array (1 .. 32) of Uint16
	  := (
	   1347, 1393, 1440, 1488, 1538, 1590, 1644, 1699, 1757,
	   1816, 1877, 1941, 2006, 2074, 2144, 2216, 2291, 2368,
	   2448, 2531, 2616, 2704, 2795, 2890, 2987, 3088, 3192,
	   3300, 3411, 3526, 3645, 3768
	  );

	MPU6050_FT_GYR : constant array (1 .. 32) of Uint16
	  := (
	   3131, 3275, 3426, 3583, 3748, 3920, 4101, 4289, 4487,
	   4693, 4909, 5135, 5371, 5618, 5877, 6147, 6430, 6725,
	   7035, 7358, 7697, 8051, 8421, 8809, 9214, 9638, 10081,
	   10545, 11030, 11537, 12068, 12623
	  );

	--  Procedures and functions

	--  Read data to the specified MPU6050 register
	procedure MPU6050_Read_Register
	  (Device      : MPU6050_Device;
	   Reg_Addr    : Uint8;
	   Data        : in out I2C_Data);

	--  Read one Uint8 at the specified MPU6050 register
	procedure MPU6050_Read_Uint8_At_Register
	  (Device   : MPU6050_Device;
	   Reg_Addr : Uint8;
	   Data     : out Uint8);

	--  Read one but at the specified MPU6050 register
	function MPU6050_Read_Bit_At_Register
	  (Device    : MPU6050_Device;
    	   Reg_Addr  : Uint8;
	   Bit_Pos   : T_Bit_Pos_8) return Boolean;

	--  Write data to the specified MPU6050 register
	procedure MPU6050_Write_Register
	  (Device      : MPU6050_Device;
	   Reg_Addr    : Uint8;
	   Data        : I2C_Data);

	--  Write one Uint8 at the specified MPU6050 register
	procedure MPU6050_Write_Uint8_At_Register
	  (Device   : MPU6050_Device;
	   Reg_Addr : Uint8;
	   Data     : Uint8);

	--  Write one bit at the specified MPU6050 register
	procedure MPU6050_Write_Bit_At_Register
	  (Device    : MPU6050_Device;
	   Reg_Addr  : Uint8;
	   Bit_Pos   : T_Bit_Pos_8;
	   Bit_Value : Boolean);

	--  Write data in the specified register, starting from the
	--  bit specified in Start_Bit_Pos
	procedure MPU6050_Write_Bits_At_Register
	  (Device        : MPU6050_Device;
	   Reg_Addr      : Uint8;
	   Start_Bit_Pos : T_Bit_Pos_8;
	   Data          : Uint8;
	   Length        : T_Bit_Pos_8);

	function Fuse_Low_And_High_Register_Parts
	  (High : Uint8;
	   Low  : Uint8) return Integer_16;
	   pragma Inline (Fuse_Low_And_High_Register_Parts);

end MPU6050;
