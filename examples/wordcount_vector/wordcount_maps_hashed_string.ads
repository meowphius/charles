with Charles.Maps.Hashed.Strings.Unbounded;
pragma Elaborate_All (Charles.Maps.Hashed.Strings.Unbounded);

with Charles.Hash_String;

package Wordcount_Maps_Hashed_String is
   new Charles.Maps.Hashed.Strings.Unbounded
         (Element_Type => Natural, 
          Hash => Charles.Hash_String); 

pragma Preelaborate (Wordcount_Maps_Hashed_String);
