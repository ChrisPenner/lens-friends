module Dynamic where

import Control.Lens
import Data.Typeable
import Data.Dynamic

dynamicLens :: Typeable a => (Lens' s a) -> Lens' s Dynamic
dynamicLens l = lens getter setter
  where
    getter s = toDyn $ view l s
    setter s b =
        case fromDynamic b of
            Nothing -> s
            Just b' -> set l b' s

asDynamic :: Typeable s => Traversal s (Maybe s) Dynamic Dynamic
asDynamic f s = fromDynamic <$> f (toDyn s)

dynamicTraversal :: Typeable a => (Lens' s a) -> LensLike' Maybe s Dynamic
dynamicTraversal l f s = do
    let a = view l s
    altered <- f (toDyn a)
    casted <- fromDynamic altered
    return $ set l casted s

data User = User {_name :: String}
  deriving Show

makeLenses ''User

user = User "Joe"

withDynamic :: Typeable a => Dynamic -> (a -> b) -> Maybe b
withDynamic d f = f <$> fromDynamic d

test :: Maybe User
test = withDynamic (toDyn "hi") (\a -> user & name .~ a)
