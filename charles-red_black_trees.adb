pragma License (Modified_GPL);

------------------------------------------------------------------------------
--                                                                          --
--                      CHARLES CONTAINER LIBRARY                           --
--                                                                          --
--              Copyright (C) 2001-2003 Matthew J Heaney                    --
--                                                                          --
-- The Charles Container Library ("Charles") is free software; you can      --
-- redistribute it and/or modify it under terms of the GNU General Public   --
-- License as published by the Free Software Foundation; either version 2,  --
-- or (at your option) any later version.  Charles is distributed in the    --
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even the  --
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. --
-- See the GNU General Public License for more details.  You should have    --
-- received a copy of the GNU General Public License distributed with       --
-- Charles;  see file COPYING.TXT.  If not, write to the Free Software      --
-- Foundation,  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                          --
-- As a special exception, if other files instantiate generics from this    --
-- unit, or you link this unit with other files to produce an executable,   --
-- this unit does not by itself cause the resulting executable to be        --
-- covered by the GNU General Public License.  This exception does not      --
-- however invalidate any other reasons why the executable file might be    --
-- covered by the GNU Public License.                                       --
--                                                                          --
-- Charles is maintained by Matthew J Heaney.                               --
--                                                                          --
-- http://home.earthlink.net/~matthewjheaney/index.html                     --
-- mailto:matthewjheaney@earthlink.net                                      --
--                                                                          --
------------------------------------------------------------------------------
with Charles.Algorithms.Generic_Lexicographical_Compare;
with Charles.Algorithms.Generic_Compare;

package body Charles.Red_Black_Trees is


   function Root (Tree : Tree_Type) return Node_Access is
   begin
      return Parent (Tree.Back);
   end;
   
   procedure Set_Root
     (Tree : Tree_Type;
      Root : Node_Access) is
   begin
      Set_Parent (Tree.Back, Root);
   end;

   
   function First (Tree : Tree_Type) return Node_Access is
   begin
      return Left (Tree.Back);
   end;
   
   procedure Set_First
     (Tree  : Tree_Type;
      First : Node_Access) is
   begin
      Set_Left (Tree.Back, First);
   end;

   
   function Last (Tree : Tree_Type) return Node_Access is
   begin
      return Right (Tree.Back);
   end;
   
   procedure Set_Last
     (Tree : Tree_Type;
      Last : Node_Access) is
   begin
      Set_Right (Tree.Back, Last);
   end;

   procedure Swap (Left, Right : in out Tree_Type) is
      L_Length : constant Natural := Left.Length;
      L_Back : constant Node_Access := Left.Back;
   begin
      Left.Length := Right.Length;
      Left.Back := Right.Back;
      
      Right.Length := L_Length;
      Right.Back := L_Back;
   end;

   
   procedure Initialize (Tree : in out Tree_Type) is
   begin
      pragma Assert (Tree.Back /= Null_Node);
      pragma Assert (Color (Tree.Back) = Red);

      Set_Parent (Tree.Back, Null_Node);      
      Set_Left (Tree.Back, Tree.Back);
      Set_Right (Tree.Back, Tree.Back);      
            
      Tree.Length := 0;      
   end;

   
   function Min (Node : Node_Access) return Node_Access is
   
      --CLR p248
      
      X : Node_Access := Node;
      Y : Node_Access;
      
   begin
   
      loop
      
         Y := Left (X);
         
         if Y = Null_Node then
            return X;
         end if;

         X := Y;
         
      end loop;

   end Min;

   
   function Max (Node : Node_Access) return Node_Access is
   
      --CLR p248
      
      X : Node_Access := Node;
      Y : Node_Access;

   begin
   
      loop
      
         Y := Right (X);
         
         if Y = Null_Node then
            return X;
         end if;
         
         X := Y;

      end loop;
      
   end Max;


--   procedure Generic_Adjust (Tree : in out Tree_Type) is

--      Back : constant Node_Access := Tree.Back;
--      Length : constant Natural := Tree.Length;
--      
--      X : constant Node_Access := Root (Tree);

--   begin
--   
--      Tree.Back := Null_Node;
--      Tree.Length := 0;
--      
--      Initialize (Tree);

--      if X /= Null_Node then
--         Set_Root (Tree, Copy_Tree (X));
--         Set_First (Tree, Min (Root (Tree)));
--         Set_Last (Tree, Max (Root (Tree))); 
--         Tree.Length := Length;
--      end if;       
--      
--   end Generic_Adjust;


--   procedure Assign 
--     (Tree : in out Tree_Type;
--      Root : in     Node_Access) is
--   begin
--      if Root /= Null_Node then
--         Set_Root (Tree, Copy_Tree (X));
--         Set_First (Tree, Min (Root (Tree)));
--         Set_Last (Tree, Max (Root (Tree))); 
--         Tree.Length := Length;
--      end if;       


   function Succ (Node : Node_Access) return Node_Access is   
   begin

      --CLR p249
      
      if Right (Node) /= Null_Node then
      
         if Color (Node) = Red then
         
            --This is possibly the Back node.
         
            declare
               P : constant Node_Access := Parent (Node);
            begin
               if P = Null_Node then
                  --This is the Back of an empty tree.
                  return Node;
               end if;
               
               declare
                  PP : constant Node_Access := Parent (P);
               begin
                  if PP = Node then
                     --This is the Back of a non-empty tree.
                     pragma Assert (Color (P) = Black);
                     return Left (Node);  --First   
                  end if;
               end;
            end;
            
         end if;

         return Min (Right (Node));
         
      end if;
      
      declare   
         X : Node_Access := Node;
         Y : Node_Access := Parent (Node);
      begin      
         while X = Right (Y) loop
            X := Y;
            Y := Parent (Y);
         end loop;
         
         if Right (X) /= Y then
            return Y;
         else
            return X;
         end if;
      end;
      
   end Succ;
   
   function Succ (Node : Node_Access; Offset : Natural) return Node_Access is
   
      Result : Node_Access := Node;
   begin
      for I in 1 .. Offset loop
         Result := Succ (Result);
      end loop;
   
      return Result;
   end;
   
   
   function Pred (Node : Node_Access) return Node_Access is
   begin
   
      if Left (Node) /= Null_Node then
      
         if Color (Node) = Red then
         
            --This is possibly the Back node.
            
            declare
               P : constant Node_Access := Parent (Node);
            begin
               if P = Null_Node then
                  --This is the Back of an empty tree.
                  return Node;
               end if;
               
               declare
                  PP : constant Node_Access := Parent (P);
               begin
                  if PP = Node then
                     --This is the Back of a non-empty tree.
                     pragma Assert (Color (P) = Black);
                     return Right (Node);  --Last
                  end if;
               end;
            end;
            
         end if;

         return Max (Left (Node));
         
      end if;
      
      declare
         X : Node_Access := Node;
         Y : Node_Access := Parent (Node);
      begin
         while X = Left (Y) loop
            X := Y;
            Y := Parent (Y);
         end loop;
         
         if Left (X) /= Y then
            return Y;
         else
            return X;
         end if;
      end;
      
   end Pred;


   function Pred (Node : Node_Access; Offset : Natural) return Node_Access is
   
      Result : Node_Access := Node;
   begin
      for I in 1 .. Offset loop
         Result := Pred (Result);
      end loop;
   
      return Result;
   end;
   

   procedure Check_Invariant (Tree : Tree_Type) is

      function Check (Node : Node_Access) return Natural is
      begin
         if Node = Null_Node then
            return 0;
         end if;
         
         if Color (Node) = Red then

            declare
               L : constant Node_Access := Left (Node);
            begin
               pragma Assert (L = Null_Node or else Color (L) = Black);
               null;
            end;

            declare
               R : constant Node_Access := Right (Node);
            begin
               pragma Assert (R = Null_Node or else Color (R) = Black);
               null;
            end;
         
            declare
               NL : constant Natural := Check (Left (Node));
               NR : constant Natural := Check (Right (Node));
            begin
               pragma Assert (NL = NR);               
               return NL;
            end;

         end if;
         
         declare
            NL : constant Natural := Check (Left (Node));
            NR : constant Natural := Check (Right (Node));
         begin
            pragma Assert (NL = NR);               
            return NL + 1;
         end;
      end Check;

      Root : constant Node_Access := Red_Black_Trees.Root (Tree);
      
   begin
   
      pragma Assert (Color (Tree.Back) = Red);
      
      if Root = Null_Node then
         pragma Assert (First (Tree) = Tree.Back);
         pragma Assert (Last (Tree) = Tree.Back);
         pragma Assert (Tree.Length = 0);
         null;
      else   
         pragma Assert (Color (Root) = Black);
         pragma Assert (Tree.Length > 0);
         
         declare
            L : constant Node_Access := Left (Root);
            R : constant Node_Access := Right (Root);
            
            NL : constant Natural := Check (L);
            NR : constant Natural := Check (R);
         begin
            pragma Assert (NL = NR);      
            null;
         end;
      end if;

   end Check_Invariant;

   
   procedure Left_Rotate
     (Tree : in out Tree_Type;
      X    : in     Node_Access) is
      
      --CLR p266
               
      Y : Node_Access := Right (X);
      pragma Assert (Y /= Null_Node);
      
   begin
   
      Set_Right (X, Left (Y));
      
      if Left (Y) /= Null_Node then
         Set_Parent (Left (Y), X);
      end if;
      
      Set_Parent (Y, Parent (X));
      
      if X = Root (Tree) then
         Set_Root (Tree, Y);

      elsif X = Left (Parent (X)) then
         Set_Left (Parent (X), Y);
         
      else 
         pragma Assert (X = Right (Parent (X)));
         Set_Right (Parent (X), Y);
         
      end if;   
   
      Set_Left (Y, X);
      Set_Parent (X, Y);
    
   end Left_Rotate;

   
   procedure Right_Rotate
     (Tree : in out Tree_Type;
      Y    : in     Node_Access) is

      X : Node_Access := Left (Y);
      pragma Assert (X /= Null_Node);
      
   begin
      
      Set_Left (Y, Right (X));
      
      if Right (X) /= Null_Node then
         Set_Parent (Right (X), Y);
      end if;
      
      Set_Parent (X, Parent (Y));
      
      if Y = Root (Tree) then
         Set_Root (Tree, X);
         
      elsif Y = Left (Parent (Y)) then
         Set_Left (Parent (Y), X);
         
      else
         pragma Assert (Y = Right (Parent (Y)));
         Set_Right (Parent (Y), X);
                  
      end if;
      
      Set_Right (X, Y);
      Set_Parent (Y, X);
      
   end Right_Rotate;


   procedure Rebalance_For_Insert
     (Tree : in out Tree_Type;
      Node : in     Node_Access) is

      --CLR p.268
      
      X : Node_Access := Node;
      pragma Assert (X /= Null_Node);
      pragma Assert (Color (X) = Red);

      Y : Node_Access;      
      
   begin
   
      --Set_Color (X, Red);
      
      while X /= Root (Tree) and then Color (Parent (X)) = Red loop

         if Parent (X) = Left (Parent (Parent (X))) then

            Y := Right (Parent (Parent (X)));
            
            if Y /= Null_Node and then Color (Y) = Red then
            
               Set_Color (Parent (X), Black);
               Set_Color (Y, Black);
               Set_Color (Parent (Parent (X)), Red);
               X := Parent (Parent (X));
 
            else

               if X = Right (Parent (X)) then  
                  X := Parent (X);
                  Left_Rotate (Tree, X);
               end if;
               
               Set_Color (Parent (X), Black);
               Set_Color (Parent (Parent (X)), Red);
               Right_Rotate (Tree, Parent (Parent (X)));  
               
            end if;
      
         else

            pragma Assert (Parent (X) = Right (Parent (Parent (X))));

            Y := Left (Parent (Parent (X)));
            
            if Y /= Null_Node and then Color (Y) = Red then
            
               Set_Color (Parent (X), Black);
               Set_Color (Y, Black);
               Set_Color (Parent (Parent (X)), Red);
               X := Parent (Parent (X));
 
            else

               if X = Left (Parent (X)) then  
                  X := Parent (X);
                  Right_Rotate (Tree, X);
               end if;

               Set_Color (Parent (X), Black);
               Set_Color (Parent (Parent (X)), Red);
               Left_Rotate (Tree, Parent (Parent (X)));
               
            end if;
            
         end if;

      end loop;   
      
      Set_Color (Root (Tree), Black);
      
   end Rebalance_For_Insert;


   function Generic_Equal (Left, Right : Tree_Type) return Boolean is
   
      function Compare is
         new Algorithms.Generic_Compare (Node_Access, Succ, Is_Equal, "=");

   begin

      if Left.Length /= Right.Length then
         return False;
      end if;
      
      return Compare (First (Left), Left.Back, First (Right));
      
   end Generic_Equal;      


   function Generic_Less (Left, Right : Tree_Type) return Boolean is
   
      function Lexicographical_Compare is
         new Algorithms.Generic_Lexicographical_Compare (Node_Access, Succ);

   begin
   
      if Left.Length > Right.Length then
         return False;
      end if;
      
      return Lexicographical_Compare 
              (First (Left), Left.Back, 
               First (Right), Right.Back);

   end Generic_Less;
   

   function Offset (From, To : Node_Access) 
      return Natural is
   
      Result : Integer'Base := 0;
      
      Node : Node_Access := From;
   begin
      while Node /= To loop
         Result := Result + 1;
         Node := Succ (Node);
      end loop;
      
      return Result;
   end;


   procedure Delete_Fixup 
     (Tree : in out Tree_Type;
      Node : in     Node_Access) is
      
      --CLR p274
      
      X : Node_Access := Node;
      W : Node_Access;

   begin
      
      while X /= Root (Tree) and Color (X) = Black loop
      
         if X = Left (Parent (X)) then
         
            W :=  Right (Parent (X));
            
            if Color (W) = Red then
               Set_Color (W, Black);
               Set_Color (Parent (X), Red);
               Left_Rotate (Tree, Parent (X));
               W := Right (Parent (X));
            end if;
            
            if (Left (W) = Null_Node or else Color (Left (W)) = Black) 
              and (Right (W) = Null_Node or else Color (Right (W)) = Black)
            then

               Set_Color (W, Red);
               X := Parent (X);
            
            else

               if Right (W) = Null_Node
                 or else Color (Right (W)) = Black 
               then
                  if Left (W) /= Null_Node then
                     Set_Color (Left (W), Black);
                  end if;

                  Set_Color (W, Red);
                  Right_Rotate (Tree, W);
                  W := Right (Parent (X));
               end if;
               
               Set_Color (W, Color (Parent (X)));
               Set_Color (Parent (X), Black);
               Set_Color (Right (W), Black);
               Left_Rotate  (Tree, Parent (X));
               X := Root (Tree);
               
            end if;               
         
         else
         
            pragma Assert (X = Right (Parent (X)));
            
            W :=  Left (Parent (X));
            
            if Color (W) = Red then
               Set_Color (W, Black);
               Set_Color (Parent (X), Red);
               Right_Rotate (Tree, Parent (X));
               W := Left (Parent (X));
            end if;
            
            if (Left (W) = Null_Node or else Color (Left (W)) = Black)
              and (Right (W) = Null_Node or else Color (Right (W)) = Black)
            then

               Set_Color (W, Red);
               X := Parent (X);
            
            else

               if Left (W) = Null_Node 
                 or else Color (Left (W)) = Black 
               then
                  if Right (W) /= Null_Node then
                     Set_Color (Right (W), Black);
                  end if;

                  Set_Color (W, Red);
                  Left_Rotate (Tree, W);
                  W := Left (Parent (X));
               end if;
               
               Set_Color (W, Color (Parent (X)));
               Set_Color (Parent (X), Black);
               Set_Color (Left (W), Black);
               Right_Rotate (Tree, Parent (X));
               X := Root (Tree);
               
            end if;
            
         end if;
      
      end loop;
      
      Set_Color (X, Black);
      
   end Delete_Fixup;

   

   procedure Delete_Swap
     (Tree : in out Tree_Type;
      Z, Y : in     Node_Access) is

      pragma Assert (Z /= Y);
      pragma Assert (Parent (Y) /= Z);
      
      Y_Parent : constant Node_Access := Parent (Y);
      Y_Color : constant Color_Type := Color (Y);
   begin
      Set_Parent (Y, Parent (Z));
      Set_Left (Y, Left (Z));
      Set_Right (Y, Right (Z));
      Set_Color (Y, Color (Z));
      
      if Root (Tree) = Z then
         Set_Root (Tree, Y);
         
      elsif Right (Parent (Y)) = Z then
         Set_Right (Parent (Y), Y);
         
      else
         pragma Assert (Left (Parent (Y)) = Z);
         Set_Left (Parent (Y), Y);
         
      end if;
      
      if Right (Y) /= Null_Node then
         Set_Parent (Right (Y), Y);
      end if;
      
      if Left (Y) /= Null_Node then
         Set_Parent (Left (Y), Y);
      end if;
      
      Set_Parent (Z, Y_Parent);
      Set_Color (Z, Y_Color);
      Set_Left (Z, Null_Node);
      Set_Right (Z, Null_Node);
   end Delete_Swap;
   

   procedure Delete
     (Tree : in out Tree_Type;
      Node : in     Node_Access) is
      
      --CLR p273

      X, Y : Node_Access;
      Z : Node_Access := Node;

   begin
   
      pragma Assert (Z /= Null_Node);
      pragma Assert (Z /= Tree.Back);
      pragma Assert (Tree.Length > 0);
      pragma Assert (Parent (Z) /= Null_Node);
            
      if Left (Z) = Null_Node then 
      
         if Right (Z) = Null_Node then
            
            if Z = First (Tree) then
               Set_First (Tree, Parent (Z));
            end if;

            if Z = Last (Tree) then
               Set_Last (Tree, Parent (Z));
            end if;
            
            if Color (Z) = Black then
               Delete_Fixup (Tree, Z);
            end if;

            pragma Assert (Left (Z) = Null_Node);
            pragma Assert (Right (Z) = Null_Node);
            
            if Z = Root (Tree) then
               pragma Assert (Tree.Length = 1);
               pragma Assert (Parent (Z) = Tree.Back);
               Set_Root (Tree, Null_Node);
               
            elsif Z = Left (Parent (Z)) then
               Set_Left (Parent (Z), Null_Node);
               
            else
               pragma Assert (Z = Right (Parent (Z)));
               Set_Right (Parent (Z), Null_Node);
               
            end if;         
            
         else

            pragma Assert (Z /= Last (Tree));
            
            X := Right (Z);
            
            if Z = First (Tree) then
               Set_First (Tree, Min (X));
            end if;
            
            if Z = Root (Tree) then
               Set_Root (Tree, X);
               
            elsif Z = Left (Parent (Z)) then
               Set_Left (Parent (Z), X);
               
            else
               pragma Assert (Z = Right (Parent (Z)));
               Set_Right (Parent (Z), X);
               
            end if;
            
            Set_Parent (X, Parent (Z));
            
            if Color (Z) = Black then
               Delete_Fixup (Tree, X);
            end if;

         end if;
                  
      elsif Right (Z) = Null_Node then
      
         pragma Assert (Z /= First (Tree));
         
         X := Left (Z);
         
         if Z = Last (Tree) then
            Set_Last (Tree, Max (X));
         end if;            

         if Z = Root (Tree) then
            Set_Root (Tree, X);
               
         elsif Z = Left (Parent (Z)) then
            Set_Left (Parent (Z), X);
            
         else
            pragma Assert (Z = Right (Parent (Z)));
            Set_Right (Parent (Z), X);
            
         end if;
         
         Set_Parent (X, Parent (Z));

         if Color (Z) = Black then
            Delete_Fixup (Tree, X);
         end if;
         
      else
      
         pragma Assert (Z /= First (Tree));
         pragma Assert (Z /= Last (Tree));

         Y := Succ (Z);
         pragma Assert (Left (Y) = Null_Node);

         X := Right (Y);

         if X = Null_Node then

            if Y = Left (Parent (Y)) then
               pragma Assert (Parent (Y) /= Z);
               Delete_Swap (Tree, Z, Y);
               Set_Left (Parent (Z), Z);
            else
               pragma Assert (Y = Right (Parent (Y)));
               pragma Assert (Parent (Y) = Z);
               Set_Parent (Y, Parent (Z));
               
               if Z = Root (Tree) then
                  Set_Root (Tree, Y);
                  
               elsif Z = Left (Parent (Z)) then
                  Set_Left (Parent (Z), Y);
                  
               else
                  pragma Assert (Z = Right (Parent (Z)));
                  Set_Right (Parent (Z), Y);
                  
               end if;

               Set_Left (Y, Left (Z));
               Set_Parent (Left (Y), Y);
               Set_Right (Y, Z);
               Set_Parent (Z, Y);
               Set_Left (Z, Null_Node);
               Set_Right (Z, Null_Node);

               declare
                  Y_Color : constant Color_Type := Color (Y);
               begin
                  Set_Color (Y, Color (Z));
                  Set_Color (Z, Y_Color);
               end;
            end if;                       
            
            if Color (Z) = Black then
               Delete_Fixup (Tree, Z);
            end if;
            
            pragma Assert (Left (Z) = Null_Node);
            pragma Assert (Right (Z) = Null_Node);
            
            if Z = Right (Parent (Z)) then
               Set_Right (Parent (Z), Null_Node);
            else
               pragma Assert (Z = Left (Parent (Z)));
               Set_Left (Parent (Z), Null_Node);
            end if;
               
         else
               
            if Y = Left (Parent (Y)) then

               pragma Assert (Parent (Y) /= Z);
            
               Delete_Swap (Tree, Z, Y);
            
               Set_Left (Parent (Z), X);
               Set_Parent (X, Parent (Z));
               
            else

               pragma Assert (Y = Right (Parent (Y)));
               pragma Assert (Parent (Y) = Z);

               Set_Parent (Y, Parent (Z));
               
               if Z = Root (Tree) then
                  Set_Root (Tree, Y);
                  
               elsif Z = Left (Parent (Z)) then
                  Set_Left (Parent (Z), Y);
                  
               else
                  pragma Assert (Z = Right (Parent (Z)));
                  Set_Right (Parent (Z), Y);
                  
               end if;

               Set_Left (Y, Left (Z));
               Set_Parent (Left (Y), Y);

               declare
                  Y_Color : constant Color_Type := Color (Y);
               begin
                  Set_Color (Y, Color (Z));
                  Set_Color (Z, Y_Color);
               end;

            end if;
            
            if Color (Z) = Black then
               Delete_Fixup (Tree, X);
            end if;

         end if;

      end if;
      
      Tree.Length := Tree.Length - 1;

   end Delete;

         

   package body Generic_Keys is

      generic
         with function New_Node return Node_Access;      
      procedure Generic_Insert_Post
        (Tree : in out Tree_Type;
         X, Y : in     Node_Access;
         Key  : in     Key_Type;
         Z    :    out Node_Access);
         
      procedure Generic_Insert_Post
        (Tree : in out Tree_Type;
         X, Y : in     Node_Access;
         Key  : in     Key_Type;
         Z    :    out Node_Access) is
      
      begin
         
         if Y = Tree.Back 
           or else X /= Null_Node 
           or else Is_Less_Key_Node (Key, Y)
         then
         
            pragma Assert (Y = Tree.Back or else Left (Y) = Null_Node);
         
            --Delay allocation as long as we can, in order to defend
            --against exceptions propagated by relational operators.
            
            Z := New_Node;
            
            pragma Assert (Z /= Null_Node);
            pragma Assert (Color (Z) = Red);
         
            Set_Left (Y, Z);
            
            if Y = Tree.Back then
               Set_Root (Tree, Z);
               Set_Last (Tree, Z);
               
            elsif Y = First (Tree) then
               Set_First (Tree, Z);
               
            end if;
            
         else
         
            pragma Assert (Right (Y) = Null_Node);
            
            --Delay allocation as long as we can, in order to defend
            --against exceptions propagated by relational operators.
            
            Z := New_Node;
            
            pragma Assert (Z /= Null_Node);
            pragma Assert (Color (Z) = Red);

            Set_Right (Y, Z);
            
            if Y = Last (Tree) then
               Set_Last (Tree, Z);
            end if;
            
         end if;
         
         Set_Parent (Z, Y);
         
         Rebalance_For_Insert (Tree, Z);
         
         Tree.Length := Tree.Length + 1;

      end Generic_Insert_Post;

            

      procedure Generic_Conditional_Insert
        (Tree    : in out Tree_Type;
         Key     : in     Key_Type;
         Node    :    out Node_Access;
         Success :    out Boolean) is
         
         Y : Node_Access := Tree.Back;
         X : Node_Access := Root (Tree);
         
         procedure Insert_Post is 
            new Generic_Insert_Post (New_Node);

      begin
               
         Success := True;

         while X /= Null_Node loop                  
         
            Y := X;
            
            Success := Is_Less_Key_Node (Key, X);
            
            if Success then
               X := Left (X);
            else
               X := Right (X);
            end if;                       
            
         end loop;
              
         Node := Y;

         if Success then            
         
            if Node = First (Tree) then
            
               Insert_Post (Tree, X, Y, Key, Node);
               
               return;
               
            end if;

            Node := Pred (Node);            
            
         end if;
         
         if Is_Less_Node_Key (Node, Key) then         
         
            Insert_Post (Tree, X, Y, Key, Node);                        
            Success := True;
            
            return;
            
         end if;
         
         Success := False;

      end Generic_Conditional_Insert;


      procedure Generic_Conditional_Insert_With_Hint
        (Tree     : in out Tree_Type;
         Position : in     Node_Access;
         Key      : in     Key_Type;
         Node     :    out Node_Access;
         Success  :    out Boolean) is
         
         procedure Insert_Sans_Hint is 
            new Generic_Conditional_Insert (New_Node);
            
         procedure Insert_Post is
            new Generic_Insert_Post (New_Node);
         
      begin
      
         if Position = Null_Node then         
            Insert_Sans_Hint (Tree, Key, Node, Success);
            return;
         end if;

         if Position = Tree.Back then            
         
            if Tree.Length > 0 
              and then Is_Less_Node_Key (Last (Tree), Key) 
            then
               Insert_Post (Tree, Null_Node, Last (Tree), Key, Node);
               Success := True;
            else
               Insert_Sans_Hint (Tree, Key, Node, Success);
            end if;
            
            return;

         end if;
         
         pragma Assert (Tree.Length > 0);
                          
         if Is_Less_Key_Node (Key, Position) then
         
            if Position = First (Tree) then

               Insert_Post (Tree, Position, Position, Key, Node);
               Success := True;
               
               return;               
               
            end if;
                  
            declare
               Before : constant Node_Access := Pred (Position);
            begin
               if Is_Less_Node_Key (Before, Key) then
               
                  if Right (Before) = Null_Node then
                     Insert_Post (Tree, Null_Node, Before, Key, Node);
                  else
                     Insert_Post (Tree, Position, Position, Key, Node);
                  end if;
                 
                  Success := True;
                 
               else
                  Insert_Sans_Hint (Tree, Key, Node, Success);
               end if;
            end;
            
            return;
           
         end if;
         
         if Is_Less_Node_Key (Position, Key) then
         
            if Position = Last (Tree) then
            
               Insert_Post (Tree, Null_Node, Last (Tree), Key, Node);
               Success := True;
               
               return;
               
            end if;
            
            declare
               After : constant Node_Access := Succ (Position);
            begin
               if Is_Less_Key_Node (Key, After) then
              
                  if Right (Position) = Null_Node then
                     Insert_Post (Tree, Null_Node, Position, Key, Node);
                  else
                     Insert_Post (Tree, After, After, Key, Node);
                  end if;
                 
                  Success := True;
                 
               else
                  Insert_Sans_Hint (Tree, Key, Node, Success);
               end if;
            end;
            
            return;
            
         end if;

         Node := Position;
         Success := False;
                              
      end Generic_Conditional_Insert_With_Hint;


      procedure Generic_Unconditional_Insert
        (Tree : in out Tree_Type;
         Key  : in     Key_Type;
         Node :    out Node_Access) is
         
         Y : Node_Access := Tree.Back;
         X : Node_Access := Root (Tree);
         
         procedure Insert_Post is 
            new Generic_Insert_Post (New_Node);

      begin
      
         while X /= Null_Node loop
         
            Y := X;
            
            if Is_Less_Key_Node (Key, X) then
               X := Left (X);
            else
               X := Right (X);
            end if;
            
         end loop;
         
         Insert_Post (Tree, X, Y, Key, Node);

      end Generic_Unconditional_Insert;
      
         
      procedure Generic_Unconditional_Insert_With_Hint
        (Tree     : in out Tree_Type;
         Position : in     Node_Access;
         Key      : in     Key_Type;
         Node     :    out Node_Access) is
         
         procedure Insert_Sans_Hint is 
            new Generic_Unconditional_Insert (New_Node);
            
         procedure Insert_Post is
            new Generic_Insert_Post (New_Node);
         
      begin
      
         if Position = Null_Node then         
            Insert_Sans_Hint (Tree, Key, Node);
            return;
         end if;

         if Position = Tree.Back then            
         
            if Tree.Length = 0
              or else Is_Less_Key_Node (Key, Last (Tree)) 
            then
               Insert_Sans_Hint (Tree, Key, Node);
            else
               Insert_Post (Tree, Null_Node, Last (Tree), Key, Node);
            end if;
            
            return;

         end if;
         
         pragma Assert (Tree.Length > 0);
         
         if Is_Less_Node_Key (Position, Key) then
            Insert_Sans_Hint (Tree, Key, Node);
            return;
         end if;
         
         if Position = First (Tree) then
            Insert_Post (Tree, Position, Position, Key, Node);
            return;
         end if;
               
         declare
            Before : constant Node_Access := Pred (Position);
         begin
            if Is_Less_Key_Node (Key, Before) then
               Insert_Sans_Hint (Tree, Key, Node);
               return;
            end if;
            
            if Right (Before) = Null_Node then
               Insert_Post (Tree, Null_Node, Before, Key, Node);
            else
               Insert_Post (Tree, Position, Position, Key, Node);
            end if;                               
         end;            
         
         --TODO: interrogate node *after* position, too.
         
      end Generic_Unconditional_Insert_With_Hint;
      
   
      function Find 
        (Tree : Tree_Type;
         Key  : Key_Type) return Node_Access is
         
         Y : Node_Access := Tree.Back;
         X : Node_Access := Root (Tree);
      begin
         while X /= Null_Node loop
            if Is_Less_Node_Key (X, Key) then
               X := Right (X);
            else
               Y := X;
               X := Left (X);
            end if;
         end loop;
         
         if Y = Tree.Back then
            return Tree.Back;
         end if;
         
         if Is_Less_Key_Node (Key, Y) then
            return Tree.Back;
         end if;
         
         return Y;
      end Find;                 


      function Lower_Bound
        (Tree : Tree_Type;
         Key  : Key_Type) return Node_Access is
         
         Y : Node_Access := Tree.Back;
         X : Node_Access := Root (Tree);
      begin
         while X /= Null_Node loop
            if Is_Less_Node_Key (X, Key) then
               X := Right (X);
            else
               Y := X;
               X := Left (X);
            end if;
         end loop;
         
         return Y;
      end Lower_Bound;
      
         
      function Upper_Bound
        (Tree : Tree_Type;
         Key  : Key_Type) return Node_Access is
         
         Y : Node_Access := Tree.Back;
         X : Node_Access := Root (Tree);
      begin
         while X /= Null_Node loop
            if Is_Less_Key_Node (Key, X) then
               Y := X;
               X := Left (X);
            else
               X := Right (X);
            end if;
         end loop;
         
         return Y;
      end Upper_Bound;


      procedure Equal_Range
        (Tree        : in     Tree_Type;
         Key         : in     Key_Type;
         First, Back :    out Node_Access) is
      begin
         First := Lower_Bound (Tree, Key);
         Back := Upper_Bound (Tree, Key);
      end;


      function Count
        (Tree : Tree_Type;
         Key  : Key_Type) return Natural is
         
         First, Back : Node_Access;
      begin
         Equal_Range (Tree, Key, First, Back);
         return Offset (First, Back);
      end;
      

   end Generic_Keys;
   

end Charles.Red_Black_Trees;
