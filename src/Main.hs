module Main where

import CQL.Core
import CQL.Optimizer
import CQL.SQL

main :: IO ()
main = do
    putStrLn "=== CQL Recursive Compiler (Fix + Hylo) ==="
    
-- Example: Building a hierarchy of subordinates (schema dependency graph)
-- WITH RECURSIVE Subordinates AS (
-- SELECT * FROM employees WHERE id = 42
-- UNION ALL
-- SELECT * FROM employees INNER JOIN Subordinates ON ...
-- ) SELECT name FROM Subordinates
    let graphQuery :: CQL
        graphQuery = Fix $ WithRecursive 
            "Subordinates"
            (Fix $ Filter "1=1" (Fix $ Filter "id = 42" (Fix $ ReadTable "employees"))) -- Filter chain for optimization test
            (Fix $ InnerJoin (Fix $ ReadTable "employees") (Fix $ TableRef "Subordinates") "employees.manager_id = Subordinates.id")
            (Fix $ Project ["name"] (Fix $ TableRef "Subordinates"))

-- Running hylomorphism: expand unFix -> optimize -> collapse to SQL
-- Intermediate Fix nodes are NOT allocated in memory.
    let compiledSQL = hylo sqlAlgebra (optimizeCoalg . unFix) graphQuery

    putStrLn "\nGenerated SQL query:"
    putStrLn "--------------------------------------------------"
    putStrLn compiledSQL
    putStrLn "--------------------------------------------------"

