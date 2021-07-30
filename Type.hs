module Type where
{-
checks three properties:
	1. variable or function was not defined before
	2. initialising expression is type correct
	3. initialising expression returns a type equal to the declared type
-}

import Library
import Parser
import Data.List

data VFType = VarType Type | FunType [Type] Type
  deriving (Eq,Show)

type VarContext = [(Identifier,VFType)]

-- used in Main as an identity function for any correctly-typed AST
astCheck :: AST -> AST
astCheck (Program ds c) = let (e,ctx) = app (checkDecls ds) []
                          in case e of
                            Nothing -> (Program ds c)
                            Just msg -> error msg

-- return the type of a given expression from a given context
typeCheck :: VarContext -> Expr -> Maybe Type
typeCheck _ (LitInteger x) = Just TyInt
typeCheck _ (LitBool b) = Just TyBool
typeCheck ctx (Var v) = case lookup v ctx of
  Just (VarType v) -> Just v
  _ -> Nothing
typeCheck ctx (BinOp op e1 e2) = do
  t1 <- typeCheck ctx e1
  t2 <- typeCheck ctx e2
  binType op t1 t2
typeCheck ctx (UnOp op e) = do
  t <- typeCheck ctx e
  unType op t
typeCheck ctx (Conditional e1 e2 e3) =  do
  t1 <- typeCheck ctx e1
  t2 <- typeCheck ctx e2
  t3 <- typeCheck ctx e3
  if t1 == TyBool && t2 == t3 then Just t2 else Nothing
typeCheck ctx (FunApp f es) = case lookup f ctx of
  Just (FunType ts t) -> case (checkArgs ctx ts es) of
      True -> Just t
      _ -> Nothing -- arguments expressions do not type check in the context
  _ -> Nothing -- the function to be applied is not in the context

-- given a context, type check each 'zipped' expression,type pair
checkArgs :: VarContext -> [Type] -> [Expr] -> Bool
checkArgs _ [] [] = True
checkArgs _ _ [] = False
checkArgs _ [] _ = False
checkArgs ctx (t:ts) (e:es) = case typeCheck ctx e of
-- if the expression is in the context, ensure its type is the same as the given one
  Just t' -> if t == t' then checkArgs ctx ts es else False
  _ -> False -- expression is not in the context

type Error = Maybe String
noError :: Error
noError = Nothing 
errorMsg :: String -> Error
errorMsg = Just

checkVar :: Identifier -> ST VarContext Error
checkVar v = do
  ctx <- stState
  case lookup v ctx of
    Nothing -> return noError
    Just t -> return $ errorMsg ("Variable " ++ v ++ " declared more than once.")

checkVType :: Expr -> Type -> ST VarContext Error
checkVType e t = do
  ctx <- stState
  case (typeCheck ctx e) of
    Nothing -> return $ errorMsg ("Expression does not type check.")
    Just t' -> if t' == t then return noError
                          else return $ errorMsg "Variable type /= Expression type."

-- verify the three properties for the given declaration
checkDecl :: Declaration -> ST VarContext Error
checkDecl (VarDecl v t) = do
  e1 <- checkVar v -- whether it is already in context
  case e1 of
    Nothing -> stRevise ((v,(VarType t)):) >> return noError -- applies function to present state
    _ -> return e1
checkDecl (VarInit v t e) = do
  e1 <- checkVar v -- whether the variable is already in context
  e2 <- checkVType e t -- e is correct and has type t
  case e1 of
    Nothing -> case e2 of
      Nothing -> stRevise ((v,(VarType t)):) >> return noError
      Just msg -> return $ errorMsg ("Variable " ++ v ++ ": " ++ msg)
    _ -> return e1
checkDecl (Fun f ps t e) = do
  e1 <- checkVar f -- whether the function is already in context
  let e2 = checkDuplicates (map fst ps) -- whether parameters have the same name
  originalContext <- stState -- save context before parameter are added
  stRevise (((map toVFType ps)++[funContext])++) -- prepend parameters to context
  e3 <- checkVType e t -- whether e has type t in the context with parameters
  stUpdate (funContext:originalContext) -- restore context without parameters
-- error checking is done afterwards but the presence of an error crashes everything
  case e1 of
    Nothing -> case e2 of
      Nothing -> case e3 of
        Nothing -> return noError
-- the expression's type is different to its declared type
        Just msg -> return $ errorMsg ("Function " ++ f ++ ": " ++ msg)
-- there are repeated identifiers in the arguments
      Just msg -> return $ errorMsg ("Function " ++ f ++ ": " ++ msg)
-- the function identifier has already been used
    Just msg -> return $ errorMsg ("Function " ++ f ++ ": " ++ msg)
  where
    funContext = (f,FunType (map snd ps) t) -- the function context is used twice
    toVFType (i,t) = (i,VarType t) -- converts variables from Type to VFType
    checkDuplicates xs
      | (length (nub xs) == length xs) == True = noError
      | otherwise = errorMsg "contains duplicate parameters"

checkDecls :: [Declaration] -> ST VarContext Error
checkDecls [] = return noError
checkDecls (d:ds) = do
  e <- checkDecl d
  case e of
    Nothing -> checkDecls ds
    _ -> return e

-- generate a context from a list of declarations (only used for testing)
context :: [Declaration] -> VarContext
context ds = let (e,ctx) = app (checkDecls ds) []
             in case e of
               Nothing -> ctx
               Just msg -> error msg

unType :: UnOperator -> Type -> Maybe Type
unType Negation TyInt = Just TyInt
unType NegBool TyBool = Just TyBool

binType :: BinOperator -> Type -> Type -> Maybe Type
binType Addition TyInt TyInt = Just TyInt
binType Subtraction TyInt TyInt = Just TyInt
binType Multiplication TyInt TyInt = Just TyInt
binType Division TyInt TyInt = Just TyInt
binType Conjunction TyBool TyBool = Just TyBool
binType Disjunction TyBool TyBool = Just TyBool
binType LssOp TyInt TyInt = Just TyBool
binType LeqOp TyInt TyInt = Just TyBool
binType GtrOp TyInt TyInt = Just TyBool
binType GeqOp TyInt TyInt = Just TyBool
binType EqOp t1 t2 = if t1==t2 then Just TyBool else Nothing
binType NeqOp t1 t2 = if t1==t2 then Just TyBool else Nothing
binType _ _ _ = Nothing