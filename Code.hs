module Code where
-- generates code from AST

import Library
import Parser
import Machine
type VarEnv = [(Identifier,StkAddress)] -- [(String, SAddAbs Int | SAddSB Int | SAddLB Int )]

-- `lookup` finds local variable addresses before global ones
address :: VarEnv -> Identifier -> StkAddress
address ve v = case lookup v ve of
  Nothing -> error "variable not in environment"
  Just a -> a

-- creates the variable environment, declarations code, code code & functions code
generateCode :: AST -> (VarEnv,[TAMInst])
generateCode (Program ds c) = (ve,declsTAM++codeTAM++[HALT]++funTAM)
  where (ve,declsTAM) = declsCode ds
-- the resulting ST label is passed from code generation to function generation because there may be ternary conditional expressions in function definitions which need fresh labels
        (codeTAM,l) = app (commCode ve c) 0
        (funTAM,l') = app (funsCode ve ds) l

funsCode :: VarEnv -> [Declaration] -> ST Int [TAMInst]
funsCode ve [] = return ([])
funsCode ve (c:cs) = 
  do tf <- funCode ve c
     tfs <- funsCode ve cs
     return (tf ++ tfs)

-- ignores all declarations except function declarations (complementary to declTAM)
funCode :: VarEnv -> Declaration -> ST Int [TAMInst]
funCode ve (Fun i ps _ e) =
  do tam <- expCode (prependParams ps ve 1) e
     return ([Label i] ++ tam ++ [RETURN 1 (length ps)])
funCode _ _ = return []

-- prepends parameter identifier, local offset pairs to the variable environment
prependParams :: [(Identifier,Type)] -> VarEnv -> Int -> VarEnv
prependParams ps ve lb = enumerateVariables (map fst ps) 1 ++ ve

-- assigns an progressively incremented local offset to each parameter identifier
enumerateVariables :: [Identifier] -> Int -> [(Identifier,StkAddress)]
enumerateVariables [] _ = []
enumerateVariables (i:is) x = ((i,(SAddLB (-x))):enumerateVariables is (x+1))

-- extracts VarEnv and declaration instructions from the ST of declsTAM
declsCode :: [Declaration] -> (VarEnv,[TAMInst])
declsCode ds = let (tam,(ve,a)) = app (declsTAM ds) ([],(SAddSB 0))
               in (ve,tam)

declsTAM :: [Declaration] -> ST (VarEnv,StkAddress) [TAMInst]
declsTAM [] = return []
declsTAM (d:ds) = do
  td <- declTAM d
  tds <- declsTAM ds
  return (td++tds)

-- ignores all function declarations (complementary to funCode)
declTAM :: Declaration -> ST (VarEnv,StkAddress) [TAMInst]
declTAM (VarDecl v _) = do
  (ve,a) <- stState
  stUpdate ((v,a):ve,addToStkAddress a 1)
  return [LOADL 0]
declTAM (VarInit v _ e) = do
  (ve,a) <- stState
  stUpdate ((v,a):ve,addToStkAddress a 1)
  let (tam,_) = app (expCode ve e) 0
  return tam
declTAM (Fun _ _ _ _) = return []

-- allows incrementing the Int within a StkAddress
addToStkAddress :: StkAddress -> Int -> StkAddress
addToStkAddress (SAddAbs a) a' = (SAddAbs (a + a'))
addToStkAddress (SAddSB a) a' = (SAddSB (a + a'))
addToStkAddress (SAddLB a) a' = (SAddLB (a + a'))

-- uses a variable enironment to generate instructions from commands in a ST
commsCode :: VarEnv -> [Command] -> ST Int [TAMInst]
commsCode ve [] = return ([])
commsCode ve (c:cs) = 
  do tc <- commCode ve c
     tcs <- commsCode ve cs
     return (tc ++ tcs)

-- handles each command case and generates the corresponding instructions
commCode :: VarEnv -> Command -> ST Int [TAMInst]
commCode ve (Assignment i e) =
  do tam <- expCode ve e
     return (tam ++ [STORE (address ve i)])
commCode ve (IfThenElse e c1 c2) =
  do te <- expCode ve e
     l1 <- fresh
     l2 <- fresh
     tc1 <- commCode ve c1
     tc2 <- commCode ve c2
     return (te ++ [JUMPIFZ l1] ++ tc1 ++ [JUMP l2,Label l1] ++ tc2 ++ [Label l2])
commCode ve (WhileDo e c) =
  do te <- expCode ve e
     l1 <- fresh
     l2 <- fresh
     tc <- commCode ve c
     return ([Label l1] ++ te ++ [JUMPIFZ l2] ++ tc ++ [JUMP l1,Label l2])
commCode ve (GetInt i) = return ([GETINT,STORE (address ve i)])
commCode ve (PrintInt e) =
  do tam <- expCode ve e
     return (tam ++ [PUTINT])
commCode ve (BeginEnd cs) =
  do tc <- commsCode ve cs
     return tc

-- handles each expression case and generates the corresponding instructions
expCode :: VarEnv -> Expr -> ST Int [TAMInst]
expCode _ (LitInteger x) = return [LOADL x]
expCode _ (LitBool b) = if b == True then return [LOADL 1] else return [LOADL 0]
expCode ve (BinOp LeqOp t1 t2) =
  expCode ve (BinOp Disjunction (BinOp LssOp t1 t2) (BinOp EqOp t1 t2))
expCode ve (BinOp GeqOp t1 t2) =
  expCode ve (BinOp Disjunction (BinOp GtrOp t1 t2) (BinOp EqOp t1 t2))
expCode ve (BinOp NeqOp t1 t2) =
  expCode ve (UnOp NegBool (BinOp EqOp t1 t2))
expCode ve (Conditional b t1 t2) =
  do tamCondition <- expCode ve b
     l1 <- fresh
     l2 <- fresh
     tam1 <- expCode ve t1
     tam2 <- expCode ve t2
     return (tamCondition ++ [JUMPIFZ l1] ++ tam1 ++ [JUMP l2,Label l1] ++ tam2 ++ [Label l2])
expCode ve (BinOp op t1 t2) =
  do tam1 <- expCode ve t1
     tam2 <- expCode ve t2
     return (tam1 ++ tam2 ++ [binOpTAM op])
expCode ve (UnOp op t) =
  do tam <- expCode ve t
     return (tam ++ [unOpTAM op])
expCode ve (Var v) = return [LOAD (address ve v)]
expCode _ (FunApp f []) = return [CALL f]
expCode ve (FunApp f (e:es)) =
  do tam <- expCode ve e
     tams <- expCode ve (FunApp f es)
     return (tam ++ tams)

binOpTAM :: BinOperator -> TAMInst
binOpTAM Addition       = ADD
binOpTAM Subtraction    = SUB
binOpTAM Multiplication = MUL
binOpTAM Division       = DIV
binOpTAM Conjunction    = AND
binOpTAM Disjunction    = OR
binOpTAM LssOp          = LSS
binOpTAM GtrOp          = GTR
binOpTAM EqOp           = EQL

unOpTAM :: UnOperator -> TAMInst
unOpTAM Negation = NEG
unOpTAM NegBool  = NOT