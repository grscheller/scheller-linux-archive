-- MonadicIdeas.hs

-- Source code used for monadicIdeas.md
-- Todo: turn into a module

-- | A function that illustrates that monadic computations can
--   be made generic to any monad.

foo :: (Monad m, Ord b, Num b) => m b -> m b
foo yy = do
  y <- yy
  let r | y < 20 = 0
        | otherwise = y * 2
  return r

