module Mebipenny.Functions where

(<|)                     :: (a -> b) -> a -> b
f <| x                   =  f x

(|>)                     :: a -> (a -> b) -> b
x |> f                   =  f x

infixr 0  <|, |>
