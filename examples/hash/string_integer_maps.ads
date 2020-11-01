with Charles.Maps.Hashed.Strings.Unbounded;
pragma Elaborate_All (Charles.Maps.Hashed.Strings.Unbounded);

with Charles.Hash_String;

package String_Integer_Maps is
   new Charles.Maps.Hashed.Strings.Unbounded 
     (Element_Type => Integer,
      Hash => Charles.Hash_String);
      
pragma Preelaborate (String_Integer_Maps);
      
      