with Charles.Hash_Integer;

with Charles.Sets.Hashed.Unbounded;
pragma Elaborate_All (Charles.Sets.Hashed.Unbounded);

package Integer_Hashed_Sets is
   new Charles.Sets.Hashed.Unbounded (Integer, Charles.Hash_Integer);
