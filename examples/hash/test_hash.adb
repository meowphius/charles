with String_Integer_Maps;  use String_Integer_Maps;
with Ada.Command_Line;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with To_Key;

procedure Test_Hash is

   Map : String_Integer_Maps.Container_Type;

   N : Natural;

   Last : Natural;

begin

   if Ada.Command_Line.Argument_Count = 0 then
      N := 2**16;
   else
      Get (Ada.Command_Line.Argument (1), N, Last);
   end if;

   Resize (Map, N);

   for I in Integer range 1 .. N loop
      Insert (Map, To_Key (I, Base => 16), I);
      if I rem 10000 = 0 then
         Put ("i=");
         Put (I, Width => 0);
         New_Line;
      end if;
   end loop;

--   declare
--      I : Iterator_Type := First (Map);
--      J : constant Iterator_Type := Back (Map);
--   begin
--      while I /= J loop
--         Put (Key (I));
--         Put (":");
--         Put (Element (I), Width => 0);
--         New_Line;
--         I := Succ (I);
--      end loop;
--
--      New_Line;
--   end;

   Put ("map.length=");
   Put (Length (Map), Width => 0);
   New_Line;

   declare
      Count : Integer'Base := 0;
   begin
      for I in reverse Integer range 1 .. N loop
         if Is_In (To_Key (I, Base => 10), Map) then
            Count := Count + 1;
         end if;
      end loop;

      Put ("map.count=");
      Put (Count, Width => 0);
      New_Line;
   end;

end Test_Hash;
