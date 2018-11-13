module AbsSyntax where

import Test.QuickCheck

data Expr = Num   Int
          | Var   String
          | BinOp Operator Expr Expr
            deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div deriving (Show, Eq)

printExpr :: Expr -> String
printExpr e = case e of
  Num   n        -> show n
  Var   id       -> id
  BinOp op el er -> concat [ "("
                           , printExpr el
                           , printOp op
                           , printExpr er
                           , ")"
                           ]

printOp :: Operator -> String
printOp op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"

simplify :: Expr -> Expr
simplify e = case e of
  Num   _        -> e
  Var   _        -> e
  BinOp op el er -> case (simplify el, simplify er) of
    (Num n0, Num n1) ->
      -- Demo cheat: Force strict evaluation. The evaluation function
      --             is otherwise never called during size evaluation.
      let n = evaluate op n0 n1
       in seq n (Num n)
    (new_el, new_er) -> BinOp op new_el new_er

evaluate :: Operator -> Int -> Int -> Int
evaluate op = case op of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div

size :: Expr -> Int
size e = case e of
  Num _ -> 1
  Var _ -> 1
  BinOp _ el er -> 1 + size el + size er

instance Arbitrary Expr where
  arbitrary = oneof
    [  do n <- arbitrary
          -- Demo cheat: Ensures that the initial test case found is not
          --             an obvious divide by zero case.
          return (Num (if n == 0 then 1 else n))
    ,  do var <- elements ["x", "y", "z"]
          return (Var var)
    ,  do (op, el, er) <- arbitrary
          return (BinOp op el er)
    ]
  shrink e = case e of
    Num n          -> [ Num n' | n' <- shrink n ]
    Var _          -> []
    BinOp op el er ->
      [ el, er ] ++
      [ BinOp op' el' er' | (op', el', er') <- shrink (op, el, er) ] ++
      -- Demo cheat: Ensures that we always end up with minimal test case.
      --             Without this, expression such as "(0/(-1 + 1))" are
      --             not shrunk to the minimal "(0/0)".
      ( let (new_el, new_er) = (simplify el, simplify er)
         in ( if new_el /= el then [BinOp op new_el er] else [] ) ++
            ( if new_er /= er then [BinOp op el new_er] else [] )
      )

instance Arbitrary Operator where
  arbitrary = elements [Add, Sub, Mul, Div]
  shrink op = case op of
    Add -> []
    _   -> [Add]

prop_simplificationReducesSize :: Expr -> Bool
prop_simplificationReducesSize e = size e >= size (simplify e)
