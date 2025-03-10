-------------------------------------------------------------------------------
--  SPDX-License-Identifier: GPL-3.0-or-later
--  Body of Radar.Conversions: Implementation of unit conversion formulas.
-------------------------------------------------------------------------------

with Ada.Numerics;  use Ada.Numerics;
with Radar.Types;

package body Radar.Conversions is

   Speed_Of_Light : constant Long_Float := 2.998_0e8;
   -- You can use 2.99792458e8 or a more precise constant as needed.

   function To_Wavelength (F : Radar.Types.Frequency_Hz)
     return Radar.Types.Wavelength_m
   is
   begin
      -- λ = c / f
      return Radar.Types.Wavelength_m(Speed_Of_Light / Long_Float(F));
   end To_Wavelength;

   function To_Frequency (Lambda : Radar.Types.Wavelength_m)
     return Radar.Types.Frequency_Hz
   is
   begin
      -- f = c / λ
      return Radar.Types.Frequency_Hz(Speed_Of_Light / Long_Float(Lambda));
   end To_Frequency;

   function dBm_to_mW (P_dBm : Radar.Types.Power_dBm)
     return Long_Float
   is
   begin
      -- P(mW) = 10^(P_dBm/10)
      return 10.0 ** (Long_Float(P_dBm) / 10.0);
   end dBm_to_mW;

   function mW_to_dBm (P_mW : Long_Float)
     return Radar.Types.Power_dBm
   is
   begin
      -- P(dBm) = 10 * log10(P_mW)
      return Radar.Types.Power_dBm(10.0 * Log10(P_mW));
   end mW_to_dBm;

end Radar.Conversions; 