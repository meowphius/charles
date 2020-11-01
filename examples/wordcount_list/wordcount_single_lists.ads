with Wordcount_Maps;  use Wordcount_Maps;

with Charles.Lists.Single.Unbounded;
pragma Elaborate_All (Charles.Lists.Single.Unbounded);

package Wordcount_Single_Lists is
   new Charles.Lists.Single.Unbounded (Wordcount_Maps.Iterator_Type);
