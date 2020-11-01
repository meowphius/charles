with Charles.Vectors.Unbounded;
pragma Elaborate_All (Charles.Vectors.Unbounded);

with Wordcount_Maps;  use Wordcount_Maps;

package Wordcount_Vectors_Unbounded is
   new Charles.Vectors.Unbounded (Positive, Wordcount_Maps.Iterator_Type);
   