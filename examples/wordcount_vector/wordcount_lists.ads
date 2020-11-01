with Wordcount_Maps;  use Wordcount_Maps;

with Charles.Lists.Double.Unbounded;
pragma Elaborate_All (Charles.Lists.Double.Unbounded);

package Wordcount_Lists is
   new Charles.Lists.Double.Unbounded
     (Wordcount_Maps.Iterator_Type);
      

   
