-------------------------------------------------------------------------------
--  SPDX-License-Identifier: GPL-3.0-or-later
--  Radar.Conversions is a utility package for electromagnetic formulas and
--  conversions between different RF units (Hz ↔ m, dBm ↔ mW), etc.
-------------------------------------------------------------------------------

with Radar.Types;

package Radar.Conversions is

   -- Convert a frequency in Hz to wavelength in meters:
   function To_Wavelength (F : Radar.Types.Frequency_Hz)
     return Radar.Types.Wavelength_m;

   -- Convert a wavelength in meters to frequency in Hz:
   function To_Frequency (Lambda : Radar.Types.Wavelength_m)
     return Radar.Types.Frequency_Hz;

   -- Convert power in dBm to linear milliwatts (mW):
   function dBm_to_mW (P_dBm : Radar.Types.Power_dBm)
     return Long_Float;

   -- Convert power in milliwatts (mW) to dBm:
   function mW_to_dBm (P_mW : Long_Float)
     return Radar.Types.Power_dBm;

end Radar.Conversions; 