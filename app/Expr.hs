module Expr (Expr (..)) where

import Relude

data Expr
    = Type
    | Var Int
    | Lam Expr
    | Pi Expr Expr
    | App Expr Expr
    deriving (Show)
