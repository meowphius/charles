with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;

with Charles.Strings.Unbounded;  use Charles.Strings.Unbounded;
with Charles.Algorithms.Generic_Quicksort;

with String_Integer_Maps;  use String_Integer_Maps;

procedure Wordfreq is

   Line  : String (1 .. 133);
   Last  : Natural;

   Word : Charles.Strings.Unbounded.Container_Type;
   Map : String_Integer_Maps.Container_Type;

   type Integer_Access is access all Integer;
   for Integer_Access'Storage_Size use 0;

   function To_Access is
      new Generic_Element (Integer_Access);

   procedure Insert is
      I : Iterator_Type;
      B : Boolean;
   begin
      if Is_Empty (Word) then
         return;
      end if;

      Insert (Map, To_String (Word), 0, I, B);

      declare
         Count : Integer renames To_Access (I).all;
      begin
         Count := Count + 1;

         Put (Key (I));
         Put (':');
         Put (Count, Width => 0);
         New_Line;
      end;

      Clear (Word);
   end Insert;

begin

   while not End_Of_File loop

      Get_Line (Line, Last);

      for I in Line'First .. Last loop
         if Is_Alphanumeric (Line (I)) then
            Append (Word, To_Lower (Line (I)));
         else
            Insert;
         end if;
      end loop;

      if Last < Line'Last then
         Insert;
      end if;

   end loop;

   declare
      I : Iterator_Type := First (Map);
      J : constant Iterator_Type := Back (Map);
   begin
      while I /= J loop
         Put (Key (I));
         Put (':');
         Put (Element (I), Width => 0);
         New_Line;

         I := Succ (I);
      end loop;
   end;

   New_Line;

   Put ("map.length=");
   Put (Length (Map), Width => 0);
   New_Line;

   declare
      A : array (1 .. Length (Map)) of Iterator_Type;

      function Is_Less (R, L : Positive) return Boolean is
         RI : constant Iterator_Type := A (R);
         LI : constant Iterator_Type := A (L);
      begin
         if Element (RI) = Element (LI) then
            return Key (RI) > Key (LI);
         else
            return Element (RI) > Element (LI);
         end if;
      end Is_Less;

      procedure Swap (I, J : Positive) is
         E : constant Iterator_Type := A (I);
      begin
         A (I) := A (J);
         A (J) := E;
      end;

      procedure Sort is
         new Charles.Algorithms.Generic_Quicksort (Positive);

      I : Iterator_Type := First (Map);
   begin
      for Index in A'Range loop
         A (Index) := I;
         I := Succ (I);
      end loop;

      Sort (First => A'First, Back => A'First + A'Length);

      for Index in A'Range loop
         Put (Element (A (Index)), Width => 0);
         Put (':');
         Put (Key (A (Index)));
         New_Line;
      end loop;
   end;

end Wordfreq;
