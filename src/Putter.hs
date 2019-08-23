-- >>> [1, 2, 3] & append .~ 4
-- [1,2,3,4]
-- >>> [1, 2, 3] & prepend .~ 0
-- [0,1,2,3]
-- >>> [1, 2, 3] & insertAt 1 .~ 1.5
-- [1.0,1.5,2.0,3.0]

-------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
module Putter where

import Control.Lens

type Putter' s a = Setter s s () a
-- type Putter s t a = Setter s t () a

append :: forall s a. Snoc s s a a => Putter' s a
append = _Snoc . sets putter
  where
    putter :: (() -> a) -> (s, a) -> (s, a)
    putter f (s, a) = (s |> a, f ())

prepend :: forall s a. Cons s s a a => Putter' s a
prepend = _Cons . sets putter
  where
    putter :: (() -> a) -> (a, s) -> (a, s)
    putter f (a, s) = (f (), a <| s)

splittingAt :: Int -> Iso' [a] ([a], [a])
splittingAt i = iso (splitAt i) (uncurry (<>))

insertAt :: Int -> Putter' [a] a
insertAt i = splittingAt i . sets putter
  where
    putter f (before, after) = (before, f () : after)

-- >>> Left () & pured .~ "hi"
-- Right "hi"
-- >>> Just 3 & pured .~ "hi"
-- Just "hi"
-- >>> Nothing & pured .~ "hi"
-- Just "hi"

-- >>> (1, Left ()) & _2  ??~ "hi" :: (Int, Either () String)
-- (1,Right "hi")
-- >>> (1, Left ()) & _2  ??~ "hi" :: (Int, Maybe String)
-- (1,Just "hi")
-- >>> (1, Left ()) & _2  ??~ "hi" :: (Int, [String])
-- (1,["hi"])

----------------------------------------------------------

type Putter t a = forall s. Setter s t () a

pured :: Applicative f => Putter (f b) b
pured = sets putter
  where
    putter f _ = pure (f ())

just :: Putter (Maybe b) b
just = pured

right :: Putter (Either e b) b
right = pured

-- Generalize `?~` to Applicative
(??~) :: Applicative f => ASetter s t a (f b) -> b -> s -> t
(??~) l b = l . pured .~ b
