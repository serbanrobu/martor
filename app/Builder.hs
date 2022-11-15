module Builder (the, lam) where

import Expr (Expr (..))
import Relude hiding (Type)

data Builder = Builder [Expr] [[(Text, Expr)]]

the :: Builder
the = Builder [Type, Var 0] [[], [("A", Type)]]

lam :: Text -> Builder -> Maybe Builder
lam varName (Builder (Pi a b : params) (context : contexts)) =
    Just $
        Builder (b : params) (((varName, a) : context) : contexts)
lam _ _ = Nothing

-- var :: Text -> Builder -> Maybe Builder
-- var name (Builder (param : params) (context : contexts))
--   | elem name context =

-- the Pi Nat Type Pi 0 Nat Pi succ Pi _ Nat Nat Nat succ succ 0
