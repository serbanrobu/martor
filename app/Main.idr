import Data.Vect

data HasType : (i : Fin n) -> Vect n Type -> Type -> Type where
    Stop : HasType FZ (t :: ctxt) t
    Pop  : HasType k ctxt t -> HasType (FS k) (u :: ctxt) t
    
data Args : Vect n Type -> Type where
    Nil : Args Nil
    (::) : a -> Args as -> Args (a :: as)

lookup : HasType i ctx t -> Args ctx -> t
lookup Stop (x :: xs) = x
lookup (Pop k) (x :: xs) = lookup k xs

-- data Expr
--     = EVar Nat
--     | ELam Expr
--     | EApp Expr Expr
    
data Builder : Vect n (m ** Vect m Type) -> Vect n Type -> Type -> Type where
    BId : Builder [(0 ** [])] [a] a
    BVar : HasType i ctx a -> Builder ((_ ** ctx) :: cs) (a :: as) ty -> Builder cs as ty
    BLam : Builder ((_ ** ctx) :: cs) ((a -> b) :: as) ty -> Builder ((S n ** a :: ctx) :: cs) (b :: as) ty
    BApp : Builder (c :: cs) (b :: as) ty -> Builder (c :: c :: cs) ((a -> b) :: a :: as) ty
    
-- build' : (cs : _) -> Args as -> Builder cs as ty -> ty 
-- build' [(0 ** [])] [x] BId = x
-- build' env (x :: xs) (BLam b) = ?foo -- \v => build' (v :: ?env) b
-- build' env xs (BVar i b) = build' (lookup i ?env :: xs) b
-- build' env (f :: x :: xs) (BApp b) = build' (f x :: xs) b
    
-- data Builder : Vect n (m ** Vect m Type) -> Vect n Type -> Type -> Type where
--     BId : Builder [(_ ** [])] [a] a
--     BLit : Nat -> Builder (_ :: cs) (Nat :: params) ty -> Builder cs params ty
--     BPlus : Builder (c :: cs) (Nat :: params) ty -> Builder (c :: c :: cs) (Nat :: Nat :: params) ty
--     BTimes : Builder (c :: cs) (Nat :: params) ty -> Builder (c :: c :: cs) (Nat :: Nat :: params) ty
--     BLam : Builder ((n ** ctx) :: cs) ((a -> b) :: params) ty -> Builder ((S n ** a :: ctx) :: cs) (b :: params) ty
--     BVar : HasType i ctx a -> Builder ((n ** ctx) :: cs) (a :: params) ty -> Builder cs params ty
--     BApp : Builder (c :: cs) (b :: params) ty -> Builder (c :: c :: cs) ((a -> b) :: a :: params) ty