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
with Charles.Red_Black_Trees;
pragma Elaborate_All (Charles.Red_Black_Trees);

with Ada.Finalization;

generic

   type Element_Type is private;

   with function "<" (L, R : String) return Boolean is <>;

   with function "=" (L, R : Element_Type) return Boolean is <>;

package Charles.Maps.Sorted.Strings.Unbounded is

   pragma Preelaborate;

   subtype Key_Subtype is String;

   subtype Element_Subtype is Element_Type;

   function Compare_Keys (Left, Right : String)
      return Boolean renames "<";

   function Compare_Elements (Left, Right : Element_Type)
      return Boolean renames "=";


   type Container_Type is private;

   type Iterator_Type is private;

   Null_Iterator : constant Iterator_Type;
      

   function "=" (Left, Right : Container_Type) return Boolean;

   function Length (Container : Container_Type) return Natural;

   function Is_Empty (Container : Container_Type) return Boolean;

   procedure Clear (Container : in out Container_Type);

   procedure Swap (Left, Right : in out Container_Type);

   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type);

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean);

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type);

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Success   :    out Boolean);

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      New_Item  : in     Element_Type);


   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean);

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      Iterator  :    out Iterator_Type);

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      Success   :    out Boolean);

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String);


   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean);

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type);

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Success   :    out Boolean);

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      New_Item  : in     Element_Type);


   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean);

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      Iterator  :    out Iterator_Type);

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      Success   :    out Boolean);

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String);


   procedure Replace_Element
     (Container : in out Container_Type;
      Key       : in     String;
      By        : in     Element_Type;
      Iterator  :    out Iterator_Type);

   procedure Replace_Element
     (Container : in out Container_Type;
      Key       : in     String;
      By        : in     Element_Type);

   procedure Replace_Element
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      By        : in     Element_Type;
      Iterator  :    out Iterator_Type);

   procedure Replace_Element
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      By        : in     Element_Type);


   procedure Delete
     (Container : in out Container_Type;
      Key       : in     String);

   procedure Delete
     (Container : in out Container_Type;
      Key       : in     String;
      Count     :    out Natural);

   procedure Delete
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type);

   procedure Delete_Sans_Increment
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type);

   procedure Delete_Sans_Assign
     (Container : in out Container_Type;
      Iterator  : in     Iterator_Type);

   procedure Delete_First (Container : in out Container_Type);
   
   procedure Delete_Last (Container : in out Container_Type);
   
   procedure Delete
     (Container : in out Container_Type;
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type);

   function Is_In
     (Key       : String;
      Container : Container_Type) return Boolean;

   function Find
     (Container : Container_Type;
      Key       : String) return Iterator_Type;

   function Count
     (Container : Container_Type;
      Key       : String) return Natural;

   function Lower_Bound
     (Container : Container_Type;
      Key       : String) return Iterator_Type;

   function Upper_Bound
     (Container : Container_Type;
      Key       : String) return Iterator_Type;

   procedure Equal_Range
     (Container : in     Container_Type;
      Key       : in     String;
      First     :    out Iterator_Type;
      Back      :    out Iterator_Type);


   function First (Container : Container_Type) return Iterator_Type;

   function Last (Container : Container_Type) return Iterator_Type;

   function Back (Container : Container_Type) return Iterator_Type;

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

   function Offset
     (From, To : Iterator_Type) return Natural;

   procedure Increment (Iterator : in out Iterator_Type);

   procedure Increment
     (Iterator : in out Iterator_Type;
      Offset   : in     Natural);

   procedure Decrement (Iterator : in out Iterator_Type);

   procedure Decrement
     (Iterator : in out Iterator_Type;
      Offset   : in     Natural);


   function Key (Iterator : Iterator_Type) return String;

--TODO:
--   generic
--      type Key_Access is access constant Key_Type;
--   function Generic_Key
--      (Iterator : Iterator_Type) return Key_Access;

--   generic
--      type Key_Access is access all Key_Type;
--   function Generic_Modify_Key
--      (Iterator : Iterator_Type) return Key_Access;

   function Element (Iterator : Iterator_Type) return Element_Type;

   function Element
     (Container : Container_Type;
      Key       : String) return Element_Type;

   generic
      type Element_Access is access all Element_Type;
   function Generic_Element
     (Iterator : Iterator_Type) return Element_Access;

   function First_Key (Container : Container_Type) return String;

   function First_Element (Container : Container_Type) return Element_Type;

   function Last_Key (Container : Container_Type) return String;

   function Last_Element (Container : Container_Type) return Element_Type;

   procedure Replace_Element
     (Iterator : Iterator_Type;
      By       : Element_Type);

   procedure Copy_Key
     (Iterator : in     Iterator_Type;
      Key      :    out String;
      Last     :    out Integer); --?

   procedure Copy_Element
     (Iterator : in     Iterator_Type;
      Item     :    out Element_Type);

   function Is_Less (Left, Right : Iterator_Type) return Boolean;

   function Is_Less
     (Left  : Iterator_Type;
      Right : String) return Boolean;

   function Is_Less
     (Left  : String;
      Right : Iterator_Type) return Boolean;

   function Is_Equal (Left, Right : Iterator_Type) return Boolean;

   function Is_Equal
     (Left  : Iterator_Type;
      Right : Element_Type) return Boolean;

   function Is_Equal
     (Left  : Element_Type;
      Right : Iterator_Type) return Boolean;


   procedure Swap
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type);

   generic
      with procedure Swap
         (L, R : in out Element_Type) is <>;
   procedure Generic_Swap
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type);

   procedure Swap_Element (Left, Right : in Iterator_Type);

   generic
      with procedure Swap
         (L, R : in out Element_Type) is <>;
   procedure Generic_Swap_Element
      (Left, Right : in Iterator_Type);

   procedure Swap_Iterator (Left, Right : in out Iterator_Type);


   generic
      with procedure Process (Key : in String) is <>;
   procedure Generic_Select_Key
      (Iterator : in Iterator_Type);

   generic
      with procedure Process (Element : in Element_Type) is <>;
   procedure Generic_Select_Element
      (Iterator : in Iterator_Type);

   generic
      with procedure Process (Element : in out Element_Type) is <>;
   procedure Generic_Modify_Element
      (Iterator : in Iterator_Type);

   generic
      with procedure Process (Element : access Element_Type) is <>;
   procedure Generic_Access_Element
      (Iterator : in Iterator_Type);

   generic
      with procedure Process (Iterator : in Iterator_Type) is <>;
   procedure Generic_Iteration
      (First, Back : in Iterator_Type);

   generic
      with procedure Process
         (Iterator  : in Iterator_Type) is <>;
   procedure Generic_Reverse_Iteration
     (First, Back : in Iterator_Type);


   generic
      with procedure Process
         (Key : String) is <>;
   procedure Generic_Select_Keys
     (First, Back : in Iterator_Type);


   generic
      with procedure Process
         (Element : in Element_Type) is <>;
   procedure Generic_Select_Elements
     (First, Back : in Iterator_Type);

   generic
      with procedure Process
         (Element : in out Element_Type) is <>;
   procedure Generic_Modify_Elements
     (First, Back : in Iterator_Type);

   generic
      with procedure Process
         (Element : access Element_Type) is <>;
   procedure Generic_Access_Elements
     (First, Back : in Iterator_Type);


   generic
      with procedure Process
         (Key : String) is <>;
   procedure Generic_Reverse_Select_Keys
     (First, Back : in Iterator_Type);


   generic
      with procedure Process
         (Element : in Element_Type) is <>;
   procedure Generic_Reverse_Select_Elements
     (First, Back : in Iterator_Type);

   generic
      with procedure Process
         (Element : in out Element_Type) is <>;
   procedure Generic_Reverse_Modify_Elements
     (First, Back : in Iterator_Type);

   generic
      with procedure Process
         (Element : access Element_Type) is <>;
   procedure Generic_Reverse_Access_Elements
     (First, Back : in Iterator_Type);

private

   type Color_Type is (Red, Black);

   type Node_Type;
   type Node_Access is access all Node_Type;

   function Parent (Node : Node_Access)
      return Node_Access;
   pragma Inline (Parent);

   function Left (Node : Node_Access)
      return Node_Access;
   pragma Inline (Left);

   function Right (Node : Node_Access)
      return Node_Access;
   pragma Inline (Right);

   function Color (Node : Node_Access)
      return Color_Type;
   pragma Inline (Color);

   procedure Set_Parent
     (Node   : Node_Access;
      Parent : Node_Access);
   pragma Inline (Set_Parent);

   procedure Set_Left
     (Node : Node_Access;
      Left : Node_Access);
   pragma Inline (Set_Left);

   procedure Set_Right
     (Node  : Node_Access;
      Right : Node_Access);
   pragma Inline (Set_Right);

   procedure Set_Color
     (Node  : Node_Access;
      Color : Color_Type);
   pragma Inline (Set_Color);

   package Trees is
      new Charles.Red_Black_Trees
        (Node_Access => Node_Access,
         Color_Type  => Color_Type,
         Null_Node   => null,
         Red         => Red,
         Black       => Black);

   type Container_Type is
      new Ada.Finalization.Controlled with record
         Tree : Trees.Tree_Type;
      end record;

   procedure Initialize (Container : in out Container_Type);

   procedure Adjust (Container : in out Container_Type);

   procedure Finalize (Container : in out Container_Type);

   type Iterator_Type is
      record
         Node : Node_Access;
      end record;

   Null_Iterator : constant Iterator_Type := (Node => null);

end Charles.Maps.Sorted.Strings.Unbounded;
