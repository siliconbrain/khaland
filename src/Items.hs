module Items (
) where

type Weight = Int

class Item a where
    weight :: Weight

