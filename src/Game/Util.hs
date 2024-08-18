module Game.Util (
  snd3,

  liftA3,
  liftA4,
  liftA5
) where

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> f') -> f a -> f b -> f c -> f d -> f e -> f f'
liftA5 f a b c d e = liftA4 f a b c d <*> e

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
