with Integer_Sorted_Sets;  use Integer_Sorted_Sets;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Test_Sorted_Sets is

   procedure Check_Sorted (S : Container_Type) is
      I : Iterator_Type := First (S);
      J : Iterator_Type;
   begin
      while I /= Last (S) loop
         J := Succ (I);
         --Put (Element (I), Width => 0); 
         --Put (' '); 
         --Put (Element (J), Width => 0); 
         --New_Line;
         pragma Assert (Element (I) < Element (J));
         I := J;
      end loop;
   end;
   
   subtype Integer_Subtype is Integer range 1 .. 1000;
   
   package Random_Numbers is
      new Ada.Numerics.Discrete_Random (Integer_Subtype);

   use Random_Numbers;
   
   G : Generator;
   
   S : Container_Type;
   
   E : Integer;
   I, J : Iterator_Type;
   B : Boolean;

   N : Natural;

begin

   --Reset (G, 0); --seed with 0 for now
   Reset (G);
  
   pragma Assert (First (S) = Back (S));
   pragma Assert (Succ (First (S)) = First (S));
   pragma Assert (Pred (First (S)) = First (S));
   pragma Assert (Succ (Back (S)) = Back (S));
   pragma Assert (Pred (Back (S)) = Back (S));
   pragma Assert (Length (S) = 0);
   
   Insert (S, 42, I, B);
   pragma Assert (Length (S) = 1);
   pragma Assert (B);
   pragma Assert (Element (I) = 42);
   pragma Assert (I = First (S));
   pragma Assert (I = Last (S));
   pragma Assert (Pred (I) = Back (S));
   pragma Assert (Succ (I) = Back (S));
   
   J := Find (S, 42);
   pragma Assert (J /= Back (S));
   pragma Assert (Element (J) = 42);
   
   Put_Line ("inserting");
   
   loop
   
      Check_Sorted (S);

      N := Length (S);
      
      exit when N > 900;
      
      E := Random (G);
      
      Insert (S, E, I, B);
      
      Put (E, Width => Integer_Subtype'Width);
      Put (' ');
      Put (Boolean'Image (B));
      New_Line;

      pragma Assert (Pred (First (S)) = Back (S));
      pragma Assert (Succ (Back (S)) = First (S));
      pragma Assert (Element (I) = E);
      
      J := Find (S, E);
      pragma Assert (J /= Back (S));
      pragma Assert (Element (J) = E);
      
      if I = First (S) then
         pragma Assert (Element (I) < Element (Succ (I)));
         null;
      elsif I = Last (S) then
         pragma Assert (Element (I) > Element (Pred (I)));
         null;
      else
         pragma Assert (Element (I) > Element (Pred (I)));
         pragma Assert (Element (I) < Element (Succ (I)));
         null;
      end if;
      
      if B then      
         pragma Assert (Length (S) = N + 1);
         null;        
      else
         pragma Assert (Length (S) = N);
         null;
      end if;
                  
   end loop;
   
   Put_Line ("deleting");

   while not Is_Empty (S) loop
         
      I := Succ (First (S), Offset => Random (G) mod Length (S)); 
      E := Element (I);
           
      Put (Length (S), Width => 0);
      Put (' ');
      Put (E, Width => 0);
      
      if I = First (S) then
         Put (" first");
         
         if I = Last (S) then
            Put (" last");
         end if;
         
      elsif I = Last (S) then
         Put (" last");
         
      end if;

      New_Line;
      
      
      Delete (S, I);
      
      pragma Assert (Pred (First (S)) = Back (S));
      pragma Assert (Succ (Back (S)) = First (S));
      pragma Assert (not Is_In (E, S));
      
      Check_Sorted (S);
      
   end loop;

end Test_Sorted_Sets;

