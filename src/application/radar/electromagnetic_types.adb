-- ---------------------------------------------------------------------
-- SPDX-License-Identifier: GPL-3.0-or-later                          --
-- electromagnetic_types.adb is a part of TECTOR.                     --
-- ---------------------------------------------------------------------

package body Electromagnetic_Types with SPARK_Mode is

   function To_Wavelength (Freq : Hertz) return Wavelength is
   begin
      return Wavelength(Speed_Of_Light) / Wavelength(Freq);
   end To_Wavelength;

   function To_Frequency (Wave : Wavelength) return Hertz is
   begin
      return Hertz(Speed_Of_Light) / Hertz(Wave);
   end To_Frequency;

end Electromagnetic_Types; 