module Store
  ( Store,
    initial, -- Store a b
    value,   -- Store a b -> a -> Perhaps b
    update,  -- Store a b -> a -> b -> Store a b
  ) where

newtype Store a b = Store (a -> Maybe b)

initial :: Store a b
initial = Store (\key -> Nothing)

value :: Store a b -> a -> Maybe b
value (Store f) key = f key  

update :: Eq a => Store a b -> a -> b -> Store a b
update (Store f) key value =
  Store (\k -> if key == k then (Just value) else f k)



