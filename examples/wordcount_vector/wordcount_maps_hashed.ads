with Charles.Maps.Hashed.Unbounded;
pragma Elaborate_All (Charles.Maps.Hashed.Unbounded);

with Charles.Hash_Unbounded_String;

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Wordcount_Maps_Hashed is
   new Charles.Maps.Hashed.Unbounded
         (Key_Type => Unbounded_String,
          Element_Type => Natural, 
          Hash => Charles.Hash_Unbounded_String); 
          
pragma Preelaborate (Wordcount_Maps_Hashed);

