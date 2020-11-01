with Wordcount_Maps;           use Wordcount_Maps;
with Map_Iterator_Vectors;     use Map_Iterator_Vectors;
with Charles.Algorithms.Generic_Quicksort;
--with Charles.Algorithms.Generic_Reverse;

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Characters.Handling;  use Ada.Characters.Handling;   
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Wordcount is

   Map : Wordcount_Maps.Container_Type;

   procedure Insert (Word : String) is   
      I : Iterator_Type;
      B : Boolean;
      
      type Element_Access is access all Wordcount_Maps.Element_Subtype;
      for Element_Access'Storage_Size use 0;
      
      function To_Access is 
         new Wordcount_Maps.Generic_Element (Element_Access);         
   begin
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

   procedure Print_Wordcount (I : Iterator_Type) is      
   begin
      Put (Key (I));
      Put (' ');
      Put (Element (I), Width => 0);
      New_Line;
   end;

   procedure Print is 
      new Map_Iterator_Vectors.Generic_Select_Elements (Print_Wordcount);
      
   Vector : Map_Iterator_Vectors.Container_Type;   

   procedure Swap (L, R : Index_Subtype) is
   begin
      Swap (Vector, L, R);
   end;
      
begin -- Wordcount

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
   
   Populate_Vector:
   declare
      procedure Process (I : Iterator_Type) is
      begin
         Append (Vector, I);
      end;
      
      procedure Iterate is
         new Wordcount_Maps.Generic_Iteration;
   begin
      Resize (Vector, Length (Map));
      Iterate (First (Map), Back (Map));
   end Populate_Vector;
   
   Put_Line ("pre sort");
   Print (Vector, First (Vector), Back (Vector));
   New_Line;

   Sort_Vector:
   declare
      function Is_Less (L, R : Index_Subtype) return Boolean is 
         LI : constant Iterator_Type := Element (Vector, L);
         RI : constant Iterator_Type := Element (Vector, R);
         
         LC : constant Natural := Element (LI);
         RC : constant Natural := Element (RI);         
      begin
         return LC > RC;
      end;
      
      procedure Sort is
         new Charles.Algorithms.Generic_Quicksort (Index_Subtype);
   begin
      Sort (First (Vector), Back (Vector));
   end Sort_Vector;


   Put_Line ("post sort - forward");
   Print (Vector, First (Vector), Back (Vector));
   New_Line;

--   Reverse_Vector:
--   declare
--      procedure Reverse_Vector is
--         new Charles.Algorithms.Generic_Reverse
--           (Index_Subtype,
--            Index_Subtype'Succ,
--            Index_Subtype'Pred);
--   begin
--      Reverse_Vector (First (Vector), Back (Vector));
--   end Reverse_Vector;         
--           
--   Put_Line ("post sort - reverse");
--   Print_Vector;
--   New_Line;

   Print (Vector, First (Vector), First (Vector) + 10);
 
end Wordcount;

