with String_Integer_Maps;  use String_Integer_Maps;
with Ada.Command_Line;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with To_Key;

procedure Test_Hash is

   M1, M2 : String_Integer_Maps.Container_Type;

   N : Positive;

   type Element_Access is access all String_Integer_Maps.Element_Subtype;
   for Element_Access'Storage_Size use 0;

   function To_Access is new Generic_Element (Element_Access);

begin

   if Ada.Command_Line.Argument_Count = 0 then
      N := 10;
   else
      declare
         Last : Natural;
      begin
         Get (Ada.Command_Line.Argument (1), N, Last);
      end;
   end if;

   Resize (M1, 10000);

   Put_Line ("start");

   for I in Integer range 0 .. 9999 loop
      Insert (M1, "foo_" & To_Key (I, Base => 10), I);
   end loop;

   Put ("m1[foo_1]=");
   Put (Element (M1, "foo_1"), Width => 0);
   New_Line;

   Put ("m1[foo_9999]=");
   Put (Element (M1, "foo_9999"), Width => 0);
   New_Line;

   Resize (M2, Length (M1));


   for I in Integer range 1 .. N loop
      Put (I, Width => 0); New_Line;

      declare
         I1 : Iterator_Type := First (M1);
         J1 : constant Iterator_Type := Back (M1);

         I2 : Iterator_Type;
         B : Boolean;
      begin
         while I1 /= J1 loop
            Insert (M2, Key (I1), 0, I2, B);

            declare
               E : Element_Subtype renames To_Access (I2).all;
            begin
               E := E + Element (I1);
            end;

            I1 := Succ (I1);
         end loop;
      end;
   end loop;

   Put ("m2[foo_1]=");
   Put (Element (M2, "foo_1"), Width => 0);
   New_Line;

   Put ("m2[foo_9999]=");
   Put (Element (M2, "foo_9999"), Width => 0);
   New_Line;

end Test_Hash;
