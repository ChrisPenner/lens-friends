{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}

module Normalize where
import Text.RawString.QQ (r)
import Control.Lens
import Data.Foldable hiding (find)
import Text.RawString.QQ (r)
import qualified Data.Text as T
import Data.Aeson.Lens
import Data.Aeson
import qualified Data.Map as M
import Data.Barbie
import Data.Functor.Compose
import GHC.Generics (Generic)


original :: Value
original = view @T.Text (singular _JSON) [r|
{
    "users": [{
        "name": "Bob",
        "favoriteFood": "apple"
    }],
    "foodsByName": {
        "apple": {
            "category": "fruit"
        }
    }
}
|]

data FoodF = FoodF
  { foodF :: T.Text
  , categoryF :: T.Text
  }

data FoodMap f = FoodMap
    { nameF :: f T.Text
    , favoriteFoodF :: f FoodF
    }
  deriving
    ( Generic
    , FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC
    )

data User = User
    { _name :: T.Text
    , _favoriteFood :: T.Text
    }

data FoodCat = FoodCat
  { _category :: T.Text
  }

data UserFoods = UserFoods
    { _users :: [User]
    , _foodsByName :: M.Map T.Text FoodCat
    }

makeLenses ''User
makeLenses ''FoodCat
makeLenses ''UserFoods

sourceObj :: UserFoods
sourceObj = UserFoods
    { _users = [User "Bob" "apple"]
    , _foodsByName = M.fromList [("apple", FoodCat "fruit")]
    }

newtype Converter s a = Converter {_value :: s -> Maybe a}
  deriving Functor via Compose ((->) s) Maybe

newtype ListF k (f :: * -> *) = ListF (f [k f])

newtype ListOf b f = ListOf {normalize :: f [b]}
  deriving
    ( Generic
    -- , FunctorB, TraversableB
    )

listFrom :: Fold s a -> Converter s [a]
listFrom fld = Converter $ Just . toListOf fld

find :: Fold s a -> Converter s a
find = Converter . preview

exactly :: a -> Converter s a
exactly a = Converter $ Just . const a

findIn :: t -> Fold t a -> Converter s a
findIn t fld = Converter $ const (t ^? fld)

userFoodsToFoodMap :: Normalized (Converter (UserFoods))
userFoodsToFoodMap =
    Normalized $ listFrom (users . traversed . to
      \user ->
          FoodMap { nameF = findIn user name
                  , favoriteFoodF = find $
                        foodsByName
                      . iix (user ^. favoriteFood)
                      . category
                      . withIndex
                      . to (\(food, cat) -> FoodF food cat)
                  }
        )

convertToCompose :: Converter s a -> Compose ((->) s) Maybe a
convertToCompose (Converter f) = Compose f
runConverter :: TraversableB b => b (Converter s) -> s -> b Maybe
runConverter = bsequence . bmap convertToCompose
    -- FoodMap { nameF = preview
            -- }

-- [{name: "Bob" favoriteFood: {food: "apple", category: "fruit"} ...]

normalized :: Value -> Value
normalized obj = toJSON new
  where
    new = obj ^.. key "users" . values . to normalize
    normalize v =
        let favoriteFood = v ^. key "favoriteFood" . _String
            foodMap = toJSON $ M.fromList [("food" :: String, favoriteFood), ("category", obj ^. key "foodsByName" . key favoriteFood . key "category" . _String)]
         in toJSON $ M.fromList [("name" :: String, v ^? key "name"), ("favoriteFood", Just foodMap)]

-- >>> encode $ normalized original
-- "[{\"favoriteFood\":{\"food\":\"apple\",\"category\":\"fruit\"},\"name\":\"Bob\"}]"
