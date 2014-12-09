module Presenter.Property
( propertyNameAtom
) where

import Import

propertyNameAtom :: Property -> Bool -> Text
propertyNameAtom p True = propertyName p
propertyNameAtom p False = "¬" <> propertyName p
