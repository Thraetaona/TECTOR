------------------------------------------------------------------------
-- SPDX-License-Identifier: GPL-3.0-or-later                          --
-- typedefs.ads is a part of TECTOR.                                  --
------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Numerics;

with System.Dim.Generic_Mks; -- GNAT extension

--  Ada (and VHDL) are the only two language to support "physical"
--  types; for example, you can define a unit of meter per second (m/s)
--  for your "velocity" type, as well as meters (m) and seconds (s) for
--  your "distance" and "time" types, respectively.  Then, if you try
--  to assign a division of mass (kilograms; kg) by seconds (s) to a
--  variable of type velocity, the compiler detects this m/s != kg/s
--  mismatch, raising a compile-time error!
--  (This needs Ada 2012 and GNAT's "Dimension_System" aspect.)
--  https://www.adacore.com/gems/gem-136-how-tall-is-a-kilogram
--  https://blog.adacore.com/uploads/dc.pdf
--  https://blog.adacore.com/physical-units-pass-the-generic-test
--  http://archive.adaic.com/tools/CKWG/Dimension/Universe.html
--  https://blog.marbu.eu/posts/
--    2022-04-03-physicsal-units-in-programming/
--  https://gcc.gnu.org/onlinedocs/gnat_ugn/
--    Performing-Dimensionality-Analysis-in-GNAT.html
--  https://docs.adacore.com/live/wave/gnat_rm/html/
--    gnat_rm/gnat_rm/implementation_defined_aspects.html
--  https://inbox.sourceware.org/gcc-patches/
--    20180821150316.GA56931@adacore.com/T/
--
--  Many physical units can be derived from the base meter, kilogram,
--  and second (i.e., "MKS") units; for example, force = mass * acceler-
--  ation has a unit of (kilogram * meter / second^2), which happens to
--  have a shortened alias of "Newton" (N) due to its common usage.
--      To accomodate electromagnetic units, this MKS system was later
--  extended to include amperes (A) as the fourth base unit, in 1954;
--  this is sometimes called the "MKSA" system.  In the 1960s, this
--  system was further extended by adding kelvin (K) and candela (cd)
--  as base units, thus forming the international system of units (SI).
--  Since 1971, the mole (mol) has been added as the 7th base unit.
--  https://en.wikipedia.org/wiki/MKS_units
--  https://en.wikipedia.org/wiki/International_System_of_Units
--
--  The GNAT compiler defines meter, kilogram, second, ampere, kelvin,
--  mole, and candela as base units under the Mks_Type type.  Based on
--  these "dimensions," we can then specify the exponent for each unit.
--  For example, the dimension aspect of velocity would (m/s) would be
--  specified as (Meter => 1, Second => -1, others =>).
--  https://github.com/gcc-mirror/gcc/blob/
--    master/gcc/ada/libgnat/s-dim.ads
--  https://github.com/gcc-mirror/gcc/blob/
--    master/gcc/ada/libgnat/s-digemk.ads
package Mksa_System is new System.Dim.Generic_Mks
  (Float_Type => Long_Float);

with Mksa_System;
package MKS.Frequency_Prefixes is
   -- same definitions
   use MKS; 

   --  https://en.wikipedia.org/wiki/Hertz
   --  https://en.wikipedia.org/wiki/Orders_of_magnitude_(frequency)
   kHz : constant Frequency := 1.0E3 * Hz;
   MHz : constant Frequency := 1.0E6 * Hz;
   GHz : constant Frequency := 1.0E9 * Hz;
   THz : constant Frequency := 1.0E12 * Hz;

   --  https://www.nasa.gov/directorates/somd/space-communications-
   --    navigation-program/radio-vs-optical-spectrum/
end MKS.Frequency_Prefixes;

package Electromagnetic_Types is
   --  The "Pure" pragma is used to let the compiler know that this
   --  entity (package) holds no internal state and has no side effects.
   --  https://stackoverflow.com/questions/19353228
   pragma Pure (Numerics);

   --  NOTE: The GNAT compiler supports ANSI ("upper" ASCII) and even
   --  unicode characters as identifiers in source code.  (In fact, it
   --  is even mandated by the Ada standard to do so.)  However, I don't
   --  think it is a good idea to use non-ASCII characters (e.g., the
   --  \Phi symbol for magnetic flux or \Theta for angle) in identifiers
   --  so I will be transliterating them to plain English characters.

   --  Ada defines a few mathematical constants (e.g., Pi, e) in the
   --  Ada.Numerics package.  However, that package (even when extended
   --  by the GNAT compiler's MKS types) do not define some constants
   --  such as the speed of light (c); I'll be defining those here.
   --  https://en.wikibooks.org/wiki/Ada_Programming/
   --    Libraries/Ada.Numerics

   --  Speed of Light (c)
   --  https://caps.gsfc.nasa.gov/mburger/Reference/
   --    PhysicalConstants.html
   --  https://en.wikibooks.org/wiki/Ada_Programming/Constants
   --  https://en.wikipedia.org/wiki/Physical_constant
   c : constant Speed := 299_792_458.0; --  Speed of Light (c)


   subtype Power is Mks_Type
     with
       Dimension => (
         Symbol   => "W",
         Meter    => 2,
         Kilogram => 1,
         Second   => -3,
         others   => 0
       );

   subtype Pressure is Mks_Type
     with
       Dimension => (
         Symbol   => "Pa",
         Meter    => -1,
         Kilogram => 1,
         Second   => -2,
         others   => 0
       );

   --  Base type for frequency (in Hertz)
   --  https://en.wikipedia.org/wiki/Hertz
   type Hertz is new Mks_Type range 0.0 .. 1.0E20
     with Dimension => (0, 0, -1, 0, 0, 0, 0);

   -- Derived frequency types (all have the same dimension as Hertz)
   type Megahertz is new Mks_Type range 0.0 .. 1.0E6
     with Dimension => (0, 0, -1, 0, 0, 0, 0);
   type Gigahertz is new Mks_Type range 0.0 .. 1.0E3
     with Dimension => (0, 0, -1, 0, 0, 0, 0);
   type Kilohertz is new Mks_Type range 0.0 .. 1.0E8
     with Dimension => (0, 0, -1, 0, 0, 0, 0);
   type Terahertz is new Mks_Type range 0.0 .. 1.0E2
     with Dimension => (0, 0, -1, 0, 0, 0, 0);
   type Radians_Per_Second is new Mks_Type range 0.0 .. 1.0E20
     with Dimension => (0, 0, -1, 0, 0, 0, 0);
   type Revolutions_Per_Minute is new Mks_Type range 0.0 .. 1.0E20
     with Dimension => (0, 0, -1, 0, 0, 0, 0);

   -- Wavelength expressed in meters
   type Meters is new Long_Float range 1.0E-12 .. 1.0E3;
   subtype Wavelength is Meters;

   -- Speed (for example, speed of light)
   type Meters_Per_Second is new Long_Float range 0.0 .. 3.0E8;
   Speed_Of_Light : constant Meters_Per_Second := 2.99792458E8;

   -- Conversion functions between frequency and wavelength
   function To_Wavelength (Freq : Hertz) return Wavelength is
      (Wavelength(Speed_Of_Light) / Wavelength(Freq));
   function To_Frequency (Wave : Wavelength) return Hertz is
      (Hertz(Speed_Of_Light) / Hertz(Wave));

   -- Conversion functions for additional frequency units
   function To_Kilohertz (F : Hertz) return Kilohertz is
      (Kilohertz(F / 1.0E3));
   function To_Hertz_From_Kilohertz (KH : Kilohertz) return Hertz is
      (Hertz(KH * 1.0E3));

   function To_Terahertz (F : Hertz) return Terahertz is
      (Terahertz(F / 1.0E12));
   function To_Hertz_From_Terahertz (TH : Terahertz) return Hertz is
      (Hertz(TH * 1.0E12));

   function To_Radians (F : Hertz) return Radians_Per_Second is
      (Radians_Per_Second(2.0 * 3.14159265358979323846 * F));
   function To_Hertz_From_Radians (W : Radians_Per_Second) return Hertz is
      (Hertz(W / (2.0 * 3.14159265358979323846)));

   function To_RPM (F : Hertz) return Revolutions_Per_Minute is
      (Revolutions_Per_Minute(F * 60.0));
   function To_Hertz_From_RPM (R : Revolutions_Per_Minute) return Hertz is
      (Hertz(R / 60.0));

   --  Frequency bands defined as subtypes
   --  https://www.nasa.gov/directorates/somd/
   --    space-communications-navigation-program/radio-vs-optical-spectrum/
   subtype Radio_Frequency is Hertz range 3.0E3 .. 3.0E9;      -- 3 kHz to 3 GHz
   subtype Microwave_Frequency is Hertz range 3.0E9 .. 3.0E11;   -- 3 GHz to 300 GHz
   subtype Infrared_Frequency is Hertz range 3.0E11 .. 4.0E14;     -- 300 GHz to 400 THz
   subtype Visible_Light is Hertz range 4.0E14 .. 7.9E14;          -- 400-790 THz
   subtype Ultraviolet is Hertz range 7.9E14 .. 3.0E16;            -- 790 THz to 30 PHz
   subtype X_Ray is Hertz range 3.0E16 .. 3.0E19;                  -- 30 PHz to 30 EHz
   subtype Gamma_Ray is Hertz range 3.0E19 .. 1.0E20;              -- > 30 EHz

private
   -- Any additional implementation details can be added here if needed

end Electromagnetic_Types;
