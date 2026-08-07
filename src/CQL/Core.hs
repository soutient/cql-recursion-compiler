{-# LANGUAGE DeriveFunctor #-}
module CQL.Core where

-- Realistic AST for the relational subset of CQL
data QueryF a
    = ReadTable String
    | Project [String] a
    | Filter String a
    | InnerJoin a a String
    -- Recursive CTE: WithRecursive cteName anchorStep recursiveStep body
    | WithRecursive String a a a
    -- A regular alias (a placeholder for references within a recursive step)
    | TableRef String
    deriving (Show, Functor)

-- Fixed Point Infrastructure
newtype Fix f = Fix { unFix :: f (Fix f) }

type CQL = Fix QueryF

-- Basic hylomorphism for merging passes (Condition 2)
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg x = alg (fmap (hylo alg coalg) (coalg x))

