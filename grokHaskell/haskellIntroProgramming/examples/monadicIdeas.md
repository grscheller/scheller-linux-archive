## Monadic Ideas
Source code: [MonadicIdeas.hs](MonadicIdeas.hs)

### 
The following function does not care what the monad in
question is.  Well, the monad in question does need to be
capable of containing something in the Ord and Num type
classes.
```
   foo :: (Monad m, Ord b, Num b) => m b -> m b
   foo yy = do
     y <- yy
     let r | y < 20 = 0
           | otherwise = y * 2
     return r
```
For the Maybe Monad,
```
   Prelude> foo $ Just 10
   Just 0
   Prelude> foo $ Just 21
   Just 42
   Prelude> foo Nothing
   Nothing
```
For the List Monad,
```
   Prelude> foo [5,10,15,20,21,25]
   [0,0,0,40,42,50]
```
