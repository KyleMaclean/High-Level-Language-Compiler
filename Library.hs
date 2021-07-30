module Library where
-- general definitions, instantiations and parsing helper functions

import Control.Applicative
import Data.Char

-- STATE MONAD DEFINITIONS:

newtype ST st a = S (st -> (a,st))

app :: ST st a -> st -> (a,st)
app (S trans) s = trans s

-- returns the state as a result (ST with underlying state 'st' and result 'st')
stState :: ST st st
stState = S (\st -> (st,st))

-- disregards the current state and result; replaces current state with 's'
stUpdate :: st -> ST st ()
stUpdate s = S (\_ -> ((),s))

-- take out a copy of the state, apply 'f' to it, then replace the old state with it
stRevise :: (st -> st) -> ST st ()
stRevise f = do
  st <- stState
  stUpdate (f st)

-- generating fresh labels

type LName = String

fresh :: ST Int LName
fresh = do n <- stState
           stUpdate (n+1)
           return (show n)

instance Functor (ST st) where
  -- fmap :: (a -> b) -> ST st a -> ST st b
  fmap g st = S (\s -> let (x,s') = app st s in (g x,s'))

instance Applicative (ST st) where
  -- pure :: a -> ST st a
  pure x = S (\s -> (x,s))

  -- (<*>) :: ST st (a -> b) -> ST st a -> ST st b
  stf <*> stx = S (\s ->
    let (f,s')  = app stf s
        (x,s'') = app stx s' in (f x,s''))

instance Monad (ST st) where
  -- (>>=) :: ST st a -> (a -> ST st b) -> ST st b
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

-- combining State Monad and IO Monad

newtype StateIO st a = StT (st -> IO (a,st))

appT :: StateIO st a -> st -> IO (a,st)
appT (StT st) x = st x

stStateT :: StateIO st st
stStateT = StT (\st -> return (st,st))

stUpdateT :: st -> StateIO st ()
stUpdateT s = StT (\_ -> return ((),s))

lift :: IO a -> StateIO st a
lift m = StT (\s -> do x <- m
                       return (x,s))

instance Functor (StateIO st) where
  -- fmap :: (a -> b) -> StateIO st a -> StateIO st b
  fmap g st = StT (\s -> do (x,s') <- appT st s
                            return (g x,s'))

instance Applicative (StateIO st) where
  -- pure :: a -> StateIO st a
  pure x = StT (\s -> return (x,s))

  -- (<*>) :: StateIO st (a -> b) -> StateIO st a -> StateIO st b
  stf <*> stx = StT (\s -> do (f,s') <- appT stf s
                              (x,s'') <- appT stx s'
                              return (f x,s''))
    
instance Monad (StateIO st) where
  -- (>>=) :: StateIO st a -> (a -> StateIO st b) -> StateIO st b
  st >>= f = StT (\s -> do (x,s') <- appT st s
                           appT (f x) s')

-- FUNCTIONAL PARSING DEFINITIONS:

-- basic definitions

newtype Parser a = P (String -> [(a,String)])

-- added according to Venanzio's usage in L16b

parseAll :: Parser a -> String -> a
parseAll (P p) s = fst $ head (parse (P p) s)

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- sequencing parsers

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g pa = P (\src -> [ (g x, src1) | (x,src1) <- parse pa src ])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\src -> [(x,src)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = P (\src -> [ (f x,src2) | (f,src1) <- parse pf src,
                                        (x,src2) <- parse pa src1 ] )

instance Monad Parser where
  -- return :: a -> Parser a
  -- return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= fpb = P (\src -> [r | (x,src1) <- parse pa src,
                               r <- parse (fpb x) src1 ] )

-- making choices

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\rsc -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P (\src -> case parse p1 src of
                    [] -> parse p2 src
                    rs -> rs)

-- chosing among many alternatives
choice :: Alternative f => [f a] -> f a
choice = foldl (<|>) empty

-- parallel parsing: get results of both parsers for ambiguous grammars
infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = P (\inp -> (parse p1 inp) ++ (parse p2 inp))

-- derived primitives

-- verify that the parsed object satisfies a condition
satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p cond = do x <- p
                    if cond x then return x else empty

sat :: (Char -> Bool) -> Parser Char
sat = satisfy item

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: (Num int, Read int) => Parser int
nat = do xs <- some digit
         return (read xs)

int :: (Num int, Read int) => Parser int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

reservedWords :: [String]
reservedWords = ["let","in","if","then","while","do","printint","var","begin","end","fun","true","false","Integer","Boolean"]

reserved :: String -> Bool
reserved s = elem s reservedWords

-- altered to not take reserved words
identifier :: Parser String
identifier = satisfy (token ident) (not . reserved)

natural :: (Num int, Read int) => Parser int
natural = token nat

integer :: (Num int, Read int) => Parser int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

parens :: Parser a -> Parser a
parens p = do symbol "("
              x <- p
              symbol ")"
              return x

squareParens :: Parser a -> Parser a
squareParens p = do symbol "["
                    x <- p
                    symbol "]"
                    return x

-- Example: parsing a list of integers
nats :: Parser [Integer]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)