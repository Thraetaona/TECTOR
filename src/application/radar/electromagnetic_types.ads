-- ---------------------------------------------------------------------
-- SPDX-License-Identifier: GPL-3.0-or-later                          --
-- electromagnetic_types.ads is a part of TECTOR.                     --
-- ---------------------------------------------------------------------

with Ada.Numerics;

package Electromagnetic_Types with SPARK_Mode is
   -- Base type for frequency in Hz
   type Hertz is new Long_Float range 0.0 .. 1.0E20;
   
   -- Derived frequency units
   type Megahertz is new Long_Float range 0.0 .. 1.0E6;
   type Gigahertz is new Long_Float range 0.0 .. 1.0E3;
   
   -- Wavelength in meters
   type Meters is new Long_Float range 1.0E-12 .. 1.0E3;
   subtype Wavelength is Meters;
   
   -- Speed (for speed of light)
   type Meters_Per_Second is new Long_Float range 0.0 .. 3.0E8;
   
   -- Constants
   Speed_Of_Light : constant Meters_Per_Second := 2.99792458E8;
   
   -- Conversion functions
   function To_Wavelength (Freq : Hertz) return Wavelength with
     Pre => Freq > 0.0,
     Post => To_Wavelength'Result > 0.0;
   
   function To_Frequency (Wave : Wavelength) return Hertz with
     Pre => Wave > 0.0,
     Post => To_Frequency'Result > 0.0;
   
   -- Common frequency bands as subtypes
   subtype Radio_Frequency is Hertz range 3.0E3 .. 3.0E9;      -- 3 kHz to 3 GHz
   subtype Microwave_Frequency is Hertz range 3.0E9 .. 3.0E11; -- 3 GHz to 300 GHz
   subtype Infrared_Frequency is Hertz range 3.0E11 .. 4.0E14; -- 300 GHz to 400 THz
   subtype Visible_Light is Hertz range 4.0E14 .. 7.9E14;      -- 400-790 THz
   subtype Ultraviolet is Hertz range 7.9E14 .. 3.0E16;        -- 790 THz to 30 PHz
   subtype X_Ray is Hertz range 3.0E16 .. 3.0E19;             -- 30 PHz to 30 EHz
   subtype Gamma_Ray is Hertz range 3.0E19 .. 1.0E20;         -- > 30 EHz
   
end Electromagnetic_Types; 