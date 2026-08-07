module CQL.Optimizer where

import CQL.Core

-- Optimizes the AST node on the fly before passing it to the text generation algebra
optimizeCoalg :: QueryF CQL -> QueryF CQL
optimizeCoalg (Filter "1=1" next) = unFix next -- Skip the dummy filter layer
optimizeCoalg (Filter cond (Fix (Filter cond2 next))) 
  | cond == cond2                 = unFix next -- Collapse duplicate filters
optimizeCoalg other               = other

