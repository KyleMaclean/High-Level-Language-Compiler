module Parser where
{-
this module contains three sections:
	1. PROGRAM PARSER
	2. EXPRESSION PARSER
	3. TAM INSTRUCTION PARSER
-}

import Library
import Control.Applicative

-- 1. PROGRAM PARSER

data StkAddress = SAddAbs Int  -- Addresses in the Stack
                | SAddSB  Int  -- Relative to Stack Base
                | SAddLB  Int  -- Relative to Local Base
  deriving (Eq)

instance Show StkAddress where
  -- show :: StkAddress -> String
  show (SAddAbs x) = "[" ++ show x ++ "]"
  show (SAddSB x) = "[SB " ++ (showAddress x) ++ "]"
  show (SAddLB x) = "[LB " ++ (showAddress x) ++ "]"

showAddress :: Int -> String
showAddress x = if x < 0 then ("- " ++ (show $ negate x)) else ("+ " ++ show x)

data AST = Program [Declaration] Command
  deriving (Eq,Show)

data Type = TyInt | TyBool
  deriving (Eq,Show)

data Declaration = VarDecl Identifier Type
                 | VarInit Identifier Type Expr
                 | Fun Identifier [(Identifier,Type)] Type Expr
  deriving (Eq,Show)

data Command = Assignment Identifier Expr
             | IfThenElse Expr Command Command
             | WhileDo Expr Command
             | GetInt Identifier
             | PrintInt Expr
             | BeginEnd [Command]
  deriving (Eq,Show)

parseProg :: String -> AST
parseProg src = case parse prog src of
  [(t,"")] -> t
-- error if some of the string is unparsed or the list is not singleton (ambiguity)
  _ -> error "parsing error"

prog :: Parser AST
prog = do symbol "let"
          ds <- decs
          symbol "in"
          c <- com
          return (Program ds c)

decs :: Parser [Declaration]
decs = do d <- dec
          (do symbol ";"
              ds <- decs
              return (d:ds)
           <|>
           return [d])

dec :: Parser Declaration
dec = (do symbol "var"
          i <- identifier
          symbol ":"
-- variable declarations have to have a type
          t <- typeParser
          (do symbol ":="
              e <- expr
              return (VarInit i t e)
           <|>
             return (VarDecl i t))
-- function declarations have to include: name, parameters, type, definition
       <|>
       do symbol "fun"
          i <- identifier
          ps <- parens params
          symbol ":"
          t <- typeParser
          symbol "="
          definition <- expr
          return (Fun i ps t definition)
      )

-- parameter declarations are parsed without the "var" prefix and "," instead of ";"
params :: Parser [(Identifier,Type)]
params = do p <- param
            (do symbol ","
                ps <- params
                return (p:ps)
             <|>
             return [p])
         <|> return []

param :: Parser (Identifier,Type)
param = do i <- identifier
           symbol ":"
           t <- typeParser
           return (i,t)

-- recognises types as string literals and returns the corresponding Type type
typeParser :: Parser Type
typeParser = do symbol "Integer"
                return TyInt
             <|>
             do symbol "Boolean"
                return TyBool

com :: Parser Command
com = (
  do i <- identifier
     symbol ":="
     e <- expr
     return (Assignment i e)
  <|>
  do symbol "if"
     e <- expr
     symbol "then"
     t <- com
     symbol "else"
     f <- com
     return (IfThenElse e t f)
  <|>
  do symbol "while"
     e <- expr
     symbol "do"
     c <- com
     return (WhileDo e c)
  <|>
  do symbol "getint"
     symbol "("
     i <- identifier
     symbol ")"
     return (GetInt i)
  <|>
  do symbol "printint"
     symbol "("
     e <- expr
     symbol ")"
     return (PrintInt e)
  <|>
  do symbol "begin"
     cs <- coms
     symbol "end"
     return (BeginEnd cs))

coms :: Parser [Command]
coms = do c <- com
          (do symbol ";"
              cs <- coms
              return (c:cs)
           <|>
           return [c])

-- 2. EXPRESSION PARSER

data BinOperator = Addition | Subtraction | Multiplication | Division
                 | Conjunction | Disjunction
                 | LssOp | LeqOp | GtrOp | GeqOp | EqOp | NeqOp
  deriving (Eq,Show,Enum)

data UnOperator = Negation | NegBool
  deriving (Eq,Show)

-- added LitBool Bool constructor
data Expr = LitInteger Int
          | LitBool Bool
          | Var Identifier
          | BinOp BinOperator Expr Expr
          | UnOp  UnOperator Expr
          | Conditional Expr Expr Expr
-- during function application, we use its name and list of expression arguments
          | FunApp Identifier [Expr]
  deriving (Eq,Show)

type Identifier = String

{-
Grammar:

  exp ::= bexp | bexp ? bexp : bexp

  bexp ::= cexp | cexp || bexp
  cexp ::= bterm | bterm && cexp
  bterm ::= aexp | aexp `op` aexp
             where `op` is one of <,<=,>,>=,==,!=

  aexp ::= mexp | mexp + aexp | mexp - aexp
  mexp ::= aterm | aterm * mexp | aterm / mexp
  aterm ::= fexp | intLit | boolLit | - aterm | ! aterm | identifier | ( exp )

  fexp ::= identifier ( args )
  args ::= args' | epsilon
  args' ::= exp, args' | exp
-}

expr :: Parser Expr
expr = do b <- bexp
          (do symbol "?"
              e0 <- bexp
              symbol ":"
              e1 <- bexp
              return (Conditional b e0 e1)
           <|>
           return b)

bexp :: Parser Expr
bexp = do e0 <- cexp
          (do symbol "||"
              e1 <- bexp
              return (BinOp Disjunction e0 e1)
           <|>
           return e0)

cexp :: Parser Expr
cexp = do e0 <- bterm
          (do symbol "&&"
              e1 <- cexp
              return (BinOp Conjunction e0 e1)
           <|>
           return e0)

-- Longer operators (eg "<=") must come before shorter ones ("<")
relop :: Parser BinOperator
relop = choice [ symbol "<=" >> return LeqOp
               , symbol "<"  >> return LssOp
               , symbol ">=" >> return GeqOp
               , symbol ">"  >> return GtrOp
               , symbol "==" >> return EqOp
               , symbol "!=" >> return NeqOp
               ]

bterm :: Parser Expr
bterm = do e0 <- aexp
           (do op <- relop
               e1 <- aexp
               return (BinOp op e0 e1)
            <|>
            return e0) 


addminus :: Parser BinOperator
addminus = choice [ symbol "+" >> return Addition
                  , symbol "-" >> return Subtraction
                  ]

-- aexp' keeps a functional accumulator for left-associativity
aexp :: Parser Expr
aexp = aexp' id

aexp' :: (Expr -> Expr) -> Parser Expr
aexp' f = do e0 <- mexp
             (do op <- addminus
                 aexp' (BinOp op (f e0))
              <|>
              return (f e0))

multdiv :: Parser BinOperator
multdiv = choice [ symbol "*" >> return Multiplication
                 , symbol "/" >> return Division
                 ]

mexp :: Parser Expr
mexp = mexp' id

mexp' :: (Expr -> Expr) -> Parser Expr
mexp' f = do e0 <- aterm
             (do op <- multdiv
                 mexp' (BinOp op (f e0))
              <|>
              return (f e0))

aterm :: Parser Expr
aterm = (
  fexp
  <|>
  do n <- natural
     return (LitInteger n)
  <|>
  do symbol "true"
     return (LitBool True)
  <|>
  do symbol "false"
     return (LitBool False)
  <|>
  do symbol "-"
     e <- aterm
     return (UnOp Negation e)
  <|>
  do i <- identifier
     return (Var i)
  <|>
  do symbol "!"
     b <- aterm
     return (UnOp NegBool b)
  <|> parens expr)

exprs :: Parser [Expr]
exprs = do e <- expr
           (do symbol ","
               es <- exprs
               return (e:es)
            <|>
            return [e])

-- parser for function application; the arguments are parenthesised expressions
fexp :: Parser Expr
fexp = do i <- identifier
          es <- parens exprs
          return (FunApp i es)

-- 3. TAM INSTRUCTION PARSER

data TAMInst
  = LOADL MTInt   -- push Integer into the stack
  | ADD           -- adds two top values in the stack
  | SUB           -- subtract second element of stack from top
  | MUL           -- multiplies top values in the stack
  | DIV           -- divides the second value by the top (integer division)
  | NEG           -- negates the top of the stack
  | AND           -- Boolean conjunction (non-zero values are True)
  | OR            -- Boolean disjunction
  | NOT           -- Boolean negation
  | LSS           -- order operation <
  | GTR           -- order operation >
  | EQL           -- equality operator
  | HALT          -- stops execution and halts the machine
  | GETINT        -- reads integer from terminal; pushes it to top of stack
  | PUTINT        -- pops top of stack and prints to terminal
  | Label LName   -- marks place in code with LName; no operation on stack
  | JUMP LName    -- execution control jumps unconditionally to location of LName
  | JUMPIFZ LName -- pop stack; if 0, jump to LName; if 1, continue
  | LOAD StkAddress  -- pushes value of stack[StkAddress] to top of stack
  | STORE StkAddress -- pops top of stack and writes value to stack[StkAddress]
  | CALL LName -- calls func beginning at LName, sets up activation record
  | RETURN MTInt MTInt -- end fun, clear local part of stack, return result, etc.
  deriving (Eq,Show)

type MTInt = Int
type Stack = [MTInt]

-- parses all instructions in a .tam file
parseTAM' :: Parser [TAMInst]
parseTAM' = do t <- parseSingleTAM'
               (do ts <- parseTAM'
                   return (t:ts)
                <|>
                return [t])

-- parses one .tam file instruction
parseSingleTAM' :: Parser TAMInst
parseSingleTAM' = (
  do symbol "LOADL"
     n <- natural
     return (LOADL n)
  <|>
  do symbol "ADD"
     return ADD
  <|>
  do symbol "SUB"
     return SUB
  <|>
  do symbol "MUL"
     return MUL
  <|>
  do symbol "DIV"
     return DIV
  <|>
  do symbol "NEG"
     return NEG
  <|>
  do symbol "AND"
     return AND 
  <|>
  do symbol "OR"
     return OR
  <|>
  do symbol "NOT"
     return NOT
  <|>
  do symbol "LSS"
     return LSS
  <|>
  do symbol "GTR"
     return GTR
  <|>
  do symbol "EQL"
     return EQL
  <|>
  do symbol "HALT"
     return HALT
  <|>
  do symbol "PUTINT"
     return PUTINT
  <|>
  do symbol "Label"
     (do symbol "#"
         l <- nat
         return (Label (show l))
      <|>
      do l <- identifier
         return (Label  l)) 
  <|>
  do symbol "JUMP #"
     l <- nat
     return (JUMP (show l)) 
  <|>
  do symbol "JUMPIFZ #"
     l <- nat
     return (JUMPIFZ (show l))
  <|>
  do symbol "LOAD"
     a <- squareParens parseStkAddress
     return (LOAD a)
  <|>
  do symbol "STORE"
     a <- squareParens parseStkAddress
     return (STORE a)
   <|>
   do symbol "CALL"
      l <- identifier
      return (CALL  l)
   <|>
   do symbol "RETURN"
      m <- natural
      n <- natural
      return (RETURN m n))

-- used by parseSingleTAM' to parse stack addresses according to their `show` format
parseStkAddress :: Parser StkAddress
parseStkAddress =
-- handling absolute addresses
  do a <- integer
     return (SAddAbs a)
-- handling global addresses relative to Stack Base
  <|>
  do symbol "SB"
     (do symbol "+"
         a <- integer
         return (SAddSB a)
      <|>
      do symbol "-"
         a <- integer
         return (SAddSB ((-1)*a)))
-- handling local addresses relative to Local base
  <|>
  do symbol "LB "
     (do symbol "+"
         a <- integer
         return (SAddSB a)
      <|>
      do symbol "-"
         a <- integer
         return (SAddLB ((-1)*a)))