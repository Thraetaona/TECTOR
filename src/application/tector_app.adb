------------------------------------------------------------------------
-- SPDX-License-Identifier: GPL-3.0-or-later                          --
-- tector.adb is a part of TECTOR.                                    --
------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Command_Line;
--  with Ada.Numerics;

--  https://www.adacore.com/gems/gem-138-gnatcoll.command-line
procedure TECTOR_App is
begin
   Ada.Text_IO.Put_Line ("TECTOR Electromagnetic Analysis Tool");
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
end TECTOR_App;

------------------------------------------------------------------------
-- END OF FILE: tector.adb                                            --
------------------------------------------------------------------------
