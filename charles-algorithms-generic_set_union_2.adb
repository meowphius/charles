------------------------------------------------------------------------------
--                                                                          --
--                      CHARLES CONTAINER LIBRARY                           --
--                                                                          --
--            Charles.Algorithms.Generic_Set_Union_2 (body)                 --
--                                                                          --
--                                                                          --
--              Copyright (C) 2001-2002 Matthew J Heaney                    --
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
procedure Charles.Algorithms.Generic_Set_Union_2
  (Left_First, Left_Back   : Left_Type;
   Right_First, Right_Back : Right_Type) is

   L : Left_Type := Left_First;
   R : Right_Type := Right_First;
   
begin

   loop
   
      if L = Left_Back then
      
         while R /= Right_Back loop
            Process (R);
            R := Succ (R);
         end loop;
         
         return;
         
      end if;
      
      if R = Right_Back then
      
         while L /= Left_Back loop
            Process (L);
            L := Succ (L);
         end loop;
         
         return;
         
      end if;
            
      if Is_Less (L, R) then

         Process (L);
         L := Succ (L);
      
      elsif Is_Less (R, L) then

         Process (R);
         R := Succ (R);

      else

         Process (L);
         L := Succ (L);
         R := Succ (R);

      end if;         
         
   end loop;   
   
end Charles.Algorithms.Generic_Set_Union_2;

