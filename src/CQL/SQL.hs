module CQL.SQL where

import CQL.Core
import Data.List (intercalate)

-- Translation algebra: assembles the final SQL code without extracting intermediate entities
sqlAlgebra :: QueryF String -> String
sqlAlgebra (ReadTable tab) = 
    "SELECT * FROM " ++ tab

sqlAlgebra (TableRef name) = 
    name

sqlAlgebra (Project fields q) = 
    "SELECT " ++ intercalate ", " fields ++ " FROM (" ++ q ++ ") AS t"

sqlAlgebra (Filter cond q) = 
    "SELECT * FROM (" ++ q ++ ") AS t WHERE " ++ cond

sqlAlgebra (InnerJoin q1 q2 cond) = 
    "SELECT * FROM (" ++ q1 ++ ") AS left_t INNER JOIN (" ++ q2 ++ ") AS right_t ON " ++ cond

sqlAlgebra (WithRecursive name anchor recStep body) =
    "WITH RECURSIVE " ++ name ++ " AS (\n" ++
    "  " ++ anchor ++ "\n" ++
    "  UNION ALL\n" ++
    "  " ++ recStep ++ "\n" ++
    ")\n" ++ body

