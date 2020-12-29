module DotShow where

class DotShow a where
  dotShow :: a -> String
  dotDeclare :: a -> Maybe String
  dotDeclare = const Nothing
