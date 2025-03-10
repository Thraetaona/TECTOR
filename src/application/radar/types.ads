-------------------------------------------------------------------------------
--  SPDX-License-Identifier: GPL-3.0-or-later
--  Radar.Types provides base definitions for the TECTOR radar project.
--  It defines unit-aware types (Frequency_Hz, Wavelength_m, etc.) and
--  fundamental data structures (e.g. Frame_Type) shared across the system.
-------------------------------------------------------------------------------

with Interfaces.C;

package Radar.Types is

   -- Physical quantity types for strong unit checking
   type Frequency_Hz is new Long_Float;
   type Wavelength_m is new Long_Float;
   type Power_dBm    is new Long_Float;
   type Gain_dB      is new Long_Float;

   -- If desired, you can define subtypes for radio ranges or other bands:
   --   subtype Radio_Frequency is Frequency_Hz range 3.0E3 .. 3.0E9;  -- 3 kHz to 3 GHz
   --   ... additional subtypes if needed

   -- Example radar frame data type (e.g., raw ADC samples).
   -- Adjust the array type/range to match actual sensor output size.
   subtype Sample is Interfaces.C.short;  -- 16-bit integer samples from the sensor
   
   type Frame_Type is array (Positive range <>) of Sample
     with Convention => C;
     -- This array can be sized/finalized dynamically at runtime 
     -- or replaced with a fixed-size array if needed.

end Radar.Types; 