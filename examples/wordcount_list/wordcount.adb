with Wordcount_Maps;           use Wordcount_Maps;
--with Wordcount_Lists;          use Wordcount_Lists;
with Wordcount_Single_Lists;   use Wordcount_Single_Lists;

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Wordcount is

   Map : Wordcount_Maps.Container_Type;

   procedure Insert (Word : String) is

      I : Wordcount_Maps.Iterator_Type;
      B : Boolean;

      type Element_Access is
         access all Wordcount_Maps.Element_Subtype;

      for Element_Access'Storage_Size use 0;

      function To_Access is
        new Wordcount_Maps.Generic_Element (Element_Access);

   begin -- Insert

      Insert (Map, To_Lower (Word), 0, I, B);

      declare
         E : Wordcount_Maps.Element_Subtype renames To_Access (I).all;
      begin
         E := E + 1;
      end;

   end Insert;

   File : File_Type;

   Word_Chars : constant Character_Set :=
     To_Set (Character_Ranges'(('a', 'z'), ('A', 'Z')));

   Line : String (1 .. 2000);

   Line_First : Positive;
   Line_Last : Natural;

   Word_First : Positive;
   Word_Last : Natural;

   procedure Put_Line (I : Wordcount_Maps.Iterator_Type) is
   begin
      Put (Key (I));
      Put (' ');
      Put (Element (I), Width => 0);
      New_Line;
   end;

   package Wordcount_Lists renames Wordcount_Single_Lists;

   procedure Put is
      new Wordcount_Lists.Generic_Select_Elements (Put_Line);

   List : Wordcount_Lists.Container_Type;

begin

   if Argument_Count = 0 then
      Put_Line (Command_Name & " <file>");
      return;
   end if;

   if Argument_Count > 1 then
      Put_Line ("too many command-line arguments");
      return;
   end if;

   begin
      Open (File, In_File, Name => Argument (1));
   exception
      when Name_Error =>
         Put_Line ("unable to open file");
         return;
   end;

   while not End_Of_File (File) loop

      Get_Line (File, Line, Line_Last);
      pragma Assert (Line_Last < Line'Last);

      Line_First := Line'First;

      loop

         Find_Token
           (Source => Line (Line_First .. Line_Last),
            Set    => Word_Chars,
            Test   => Ada.Strings.Inside,
            First  => Word_First,
            Last   => Word_Last);

         exit when Word_Last = 0;

         Insert (Word => Line (Word_First .. Word_Last));

         Line_First := Word_Last + 1;

      end loop;

   end loop;

   Populate_List:
   declare
      procedure Process (I : Wordcount_Maps.Iterator_Type) is
      begin
         Append (List, New_Item => I);
      end;

      procedure Iterate is
         new Wordcount_Maps.Generic_Iteration;
   begin
      Iterate (First (Map), Back (Map));
   end Populate_List;

   Put_Line ("pre-sort:");
   Put (First (List), Back (List));
   New_Line;

   declare
      function "<" (L, R : Wordcount_Maps.Iterator_Type)
         return Boolean is

         LI : constant Natural := Element (L);
         RI : constant Natural := Element (R);
      begin
         return LI > RI;  --yes
      end;

      procedure Sort is new Wordcount_Lists.Generic_Quicksort;
   begin
      Sort (List);
   end;

   Put_Line ("post-sort:");
   Put (First (List), Back (List));
   New_Line;

   Print_Top_10:
   declare
      I : Wordcount_Lists.Iterator_Type := First (List);
      J : constant Wordcount_Lists.Iterator_Type := Back (List);
   begin
      for Index in 1 .. 10 loop
         exit when I = J;
         Put_Line (Element (I));
         Increment (I);
      end loop;
   end Print_Top_10;

end Wordcount;

