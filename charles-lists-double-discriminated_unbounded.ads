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

with Ada.Finalization;

with Charles.Double_Lists;
pragma Elaborate_All (Charles.Double_Lists);

generic

   type Discriminant_Type is (<>);

   type Element_Type (Discriminant : Discriminant_Type) is private;
      
   with function "=" (L, R : Element_Type) return Boolean is <>;
   
package Charles.Lists.Double.Discriminated_Unbounded is

   pragma Preelaborate;
   
   subtype Element_Subtype is Element_Type;
   
   function Compare_Elements (Left, Right : Element_Type)
      return Boolean renames "=";
   

   type Container_Type is private;
   
   type Iterator_Type is private;
   
   Null_Iterator : constant Iterator_Type;
   

   function "=" (Left, Right : Container_Type) return Boolean;
   
   generic
      with function "<" (L, R : Element_Type) return Boolean is <>;
   function Generic_Less
     (Left, Right : Container_Type) return Boolean;

   function Length (Container : Container_Type) return Natural;
   
   function Is_Empty (Container : Container_Type) return Boolean;
   
   procedure Clear (Container : in out Container_Type);
   
   procedure Swap (Left, Right : in out Container_Type);
   
   
--   function To_Container 
--     (First  : Iterator_Type;
--      Back   : Iterator_Type) return Container_Type;

--   procedure Assign
--     (Target : in out Container_Type;
--      Source : in     Container_Type);


   procedure Prepend
     (Container    : in out Container_Type;
      Discriminant : in     Discriminant_Type);

   procedure Prepend
     (Container : in out Container_Type;
      New_Item  : in     Element_Type);
         
      
   procedure Append
     (Container    : in out Container_Type;
      Discriminant : in     Discriminant_Type);

   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Element_Type);
      
   
   procedure Insert
     (Container    : in out Container_Type;
      Before       : in     Iterator_Type;
      Discriminant : in     Discriminant_Type);

   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      New_Item  : in     Element_Type);
     
   procedure Insert
     (Container    : in out Container_Type;
      Before       : in     Iterator_Type;
      Discriminant : in     Discriminant_Type;
      Iterator     :    out Iterator_Type);

   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type);



   procedure Delete
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type);
      
   procedure Delete_First (Container : in out Container_Type);
   
   procedure Delete_Last (Container : in out Container_Type);


   procedure Delete
     (Container : in out Container_Type; 
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type);
      
   procedure Delete
     (Container : in out Container_Type;
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type;
      Item      : in     Element_Type;
      Count     :    out Natural);

   procedure Delete
     (Container : in out Container_Type;
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type;
      Item      : in     Element_Type);

   procedure Delete
     (Container : in out Container_Type;
      Item      : in     Element_Type;
      Count     :    out Natural);

   procedure Delete
     (Container : in out Container_Type;
      Item      : in     Element_Type);

      
   generic
      with function Predicate (Element : Element_Type)
        return Boolean is <>;
   procedure Generic_Delete 
     (Container : in out Container_Type;
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type;
      Count     :    out Natural);


   procedure Reverse_List (First, Back : in Iterator_Type);         
   
   procedure Reverse_List (Container : in Container_Type);
   
   
   procedure Unique (Container : in out Container_Type);
   
   procedure Unique 
     (Container : in out Container_Type;
      First     : in     Iterator_Type;
      Back      : in     Iterator_Type);

   generic
      with function Predicate (L, R : Element_Type) return Boolean is <>;
   procedure Generic_Unique 
     (Container : in out Container_Type;
      First     : in     Iterator_Type;
      Back      : in     Iterator_Type);

   generic
      with function "<" (L, R : Element_Type) return Boolean is <>;
   procedure Generic_Merge 
     (Container : in out Container_Type;
      Source    : in out Container_Type;
      First     : in     Iterator_Type;
      Back      : in     Iterator_Type);
      
   generic
      with function "<" (L, R : Element_Type) return Boolean is <>;
   procedure Generic_Sort (First, Back : in Iterator_Type);

   
   function First 
     (Container : Container_Type) return Iterator_Type;
   
   function First_Element
     (Container : Container_Type) return Element_Type;
     
   function Last 
     (Container : Container_Type) return Iterator_Type;
   
   function Last_Element
     (Container : Container_Type) return Element_Type;
     
   function Back 
     (Container : Container_Type) return Iterator_Type;
   
   function Element 
     (Iterator : Iterator_Type) return Element_Type;
   
   generic
      type Element_Access is access all Element_Type;
   function Generic_Element
     (Iterator : Iterator_Type) return Element_Access;
   
   procedure Replace_Element
     (Iterator : Iterator_Type;
      By       : Element_Type);
      
   procedure Copy_Element
     (Iterator : in     Iterator_Type;
      Item     :    out Element_Type);

   procedure Swap_Element
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type);

   generic
      with procedure Swap 
       (Left, Right : in out Element_Type) is <>;
   procedure Generic_Swap_Element
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type);
      
   procedure Swap (Left, Right : in Iterator_Type);
   
   generic
      with procedure Swap (Left, Right : in out Element_Type) is <>;
   procedure Generic_Swap (Left, Right : in Iterator_Type);
   
   procedure Swap_Iterator (Left, Right : in out Iterator_Type);

   
   function Is_Equal (Left, Right : Iterator_Type) return Boolean;
   
   function Is_Equal 
     (Left  : Iterator_Type;
      Right : Element_Type) return Boolean;
      
   function Is_Equal 
     (Left  : Element_Type;
      Right : Iterator_Type) return Boolean;

   generic
      with procedure Process 
        (Element : in Element_Type) is <>;
   procedure Generic_Select_Element
     (Iterator : in Iterator_Type);
     
   generic
      with procedure Process 
        (Element : in out Element_Type) is <>;
   procedure Generic_Modify_Element
     (Iterator : in Iterator_Type);
     
   generic
      with procedure Process 
        (Element : access Element_Type) is <>;
   procedure Generic_Access_Element
     (Iterator : in Iterator_Type);
     
   generic
      with procedure Process 
        (Iterator  : in Iterator_Type) is <>;
   procedure Generic_Iteration
     (First : in Iterator_Type;
      Back  : in Iterator_Type);
     
   generic
      with procedure Process 
        (Iterator  : in Iterator_Type) is <>;
   procedure Generic_Reverse_Iteration
     (First : in Iterator_Type;
      Back  : in Iterator_Type);
     

   generic
      with procedure Process 
        (Element : in Element_Type) is <>;
   procedure Generic_Select_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type);
     
   generic
      with procedure Process 
        (Element : in out Element_Type) is <>;
   procedure Generic_Modify_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type);
     
   generic
      with procedure Process 
        (Element : access Element_Type) is <>;
   procedure Generic_Access_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type);
     
   generic
      with procedure Process 
        (Element : in Element_Type) is <>;
   procedure Generic_Reverse_Select_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type);
     
   generic
      with procedure Process 
        (Element : in out Element_Type) is <>;
   procedure Generic_Reverse_Modify_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type);
     
   generic
      with procedure Process 
        (Element : access Element_Type) is <>;
   procedure Generic_Reverse_Access_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type);
     
     
   function Find 
     (First : Iterator_Type;
      Back  : Iterator_Type;
      Item  : Element_Type) return Iterator_Type;
      
   function Find 
     (Container : Container_Type;
      Item      : Element_Type) return Iterator_Type;
         
   generic
      with function Predicate (Element : Element_Type)
         return Boolean is <>;
   function Generic_Find 
     (First : Iterator_Type;
      Back  : Iterator_Type) return Iterator_Type;
      
   function Reverse_Find 
     (First : Iterator_Type;
      Back  : Iterator_Type;
      Item  : Element_Type) return Iterator_Type;
      
   function Reverse_Find 
     (Container : Container_Type;
      Item      : Element_Type) return Iterator_Type;
      
   generic
      with function Predicate (Element : Element_Type)
         return Boolean is <>;
   function Generic_Reverse_Find 
     (First : Iterator_Type;
      Back  : Iterator_Type) return Iterator_Type;
      
      
   procedure Splice
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Source    : in out Container_Type);

   procedure Splice
     (Container : in Container_Type;
      Before    : in Iterator_Type;
      Iterator  : in Iterator_Type);

   procedure Splice
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Source    : in out Container_Type;
      Iterator  : in     Iterator_Type);

   procedure Splice
     (Container : in Container_Type;
      Before    : in Iterator_Type;
      First     : in Iterator_Type;
      Back      : in Iterator_Type);

   procedure Splice
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Source    : in out Container_Type;
      First     : in     Iterator_Type;
      Back      : in     Iterator_Type);


   function Succ 
     (Iterator : Iterator_Type) return Iterator_Type;
     
   function Succ 
     (Iterator : Iterator_Type;
      Offset   : Natural) return Iterator_Type;
     
   function Pred 
     (Iterator : Iterator_Type) return Iterator_Type;
     
   function Pred 
     (Iterator : Iterator_Type;
      Offset   : Natural) return Iterator_Type;
     
   procedure Increment (Iterator : in out Iterator_Type);

   procedure Increment
     (Iterator : in out Iterator_Type;
      Offset   : in     Natural);
      
   procedure Decrement (Iterator : in out Iterator_Type);
   
   procedure Decrement 
     (Iterator : in out Iterator_Type;
      Offset   : in     Natural);
      
   function Offset
     (From, To : Iterator_Type) return Natural;
     
   
private

   type Node_Type;   
   type Node_Access is access Node_Type;

   procedure Set_Succ
     (Node : Node_Access;
      Succ : Node_Access);   
   pragma Inline (Set_Succ);
   
   procedure Set_Pred
     (Node : Node_Access;
      Pred : Node_Access);
   pragma Inline (Set_Pred);
   
   function Succ (Node : Node_Access) return Node_Access;
   pragma Inline (Succ);
   
   function Pred (Node : Node_Access) return Node_Access;
   pragma Inline (Pred);   

   procedure Free (X : in out Node_Access);
   pragma Inline (Free);   

   package Node_Lists is
      new Charles.Double_Lists (Node_Access, null);   

   type Container_Type is 
      new Ada.Finalization.Controlled with record
         List : aliased Node_Lists.List_Type;
      end record;
      
   procedure Initialize (Container : in out Container_Type);
   
   procedure Adjust (Container : in out Container_Type);

   procedure Finalize (Container : in out Container_Type);   
      

   type Iterator_Type is
      record
         Node : Node_Access;
      end record;
      
   Null_Iterator : constant Iterator_Type := (Node => null);
 
end Charles.Lists.Double.Discriminated_Unbounded;
