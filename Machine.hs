module Machine where
-- runs code on abstract machine

import Library
import Parser
import Data.List (intercalate)
import Data.Char

type Counter = Int

data TAMState = 
  TAMState {
    tsCode :: [TAMInst],
    tsCounter :: Counter,
    tsStack :: Stack,
    tsSB :: Int,
    tsLB :: Int
  } deriving Show

type TAMSt a = StateIO TAMState a

-- tam state operations

stackT :: TAMSt Stack
stackT = do ts <- stStateT
            return (tsStack ts)

stkUpdateT :: Stack -> TAMSt ()
stkUpdateT stk = do ts <- stStateT
                    stUpdateT (ts { tsStack = stk })

popT :: TAMSt MTInt
popT = do stk <- stackT
          stkUpdateT (tail stk)
          return (head stk)

pushT :: MTInt -> TAMSt ()
pushT x = do stk <- stackT
             stkUpdateT (x:stk)

codeT :: TAMSt [TAMInst]
codeT = do ts <- stStateT
           return (tsCode ts)

counterT :: TAMSt Counter
counterT = do ts <- stStateT
              return (tsCounter ts)

counterUpdateT :: Counter -> TAMSt ()
counterUpdateT c = do ts <- stStateT
                      stUpdateT (ts { tsCounter = c })

-- convenience functions to accessing and modifying the stack and local bases
sbT :: TAMSt Int
sbT = do ts <- stStateT
         return (tsSB ts)

sbUpdateT :: Int -> TAMSt ()
sbUpdateT sb = do ts <- stStateT
                  stUpdateT (ts { tsSB = sb })
lbT :: TAMSt Int
lbT = do ts <- stStateT
         return (tsLB ts)
lbUpdateT :: Int -> TAMSt ()
lbUpdateT lb = do ts <- stStateT
                  stUpdateT (ts { tsLB = lb })

continueT :: TAMSt ()
continueT = do counter <- counterT
               code <- codeT
               if (counter < (length code)-1) then counterUpdateT (counter+1)
                                              else error "counter out of bounds"

findLabelT :: LName -> TAMSt Counter
findLabelT l = do tam <- codeT
                  return (lCounter l tam)

-- calls lCounter' to look for the label's index in the entire code (from index 0)
lCounter :: LName -> [TAMInst] -> Counter
lCounter l is = lCounter' l is 0
-- helper function to find the index of a label after a given counter index
lCounter' :: LName -> [TAMInst] -> Counter -> Counter
lCounter' _ [] _ = error "label not found"
-- case to check whether an encountered label is the desired label
lCounter' l ((Label x):is) c = if x == l then c else lCounter' l is (c+1)
lCounter' l (_:is) c = lCounter' l is (c+1)

executeT :: TAMInst -> TAMSt ()
executeT NEG = executeUnOpT (* (-1))
executeT NOT = executeUnOpT (intNOT)
executeT ADD = executeBinOpT (+)
executeT SUB = executeBinOpT (-)
executeT MUL = executeBinOpT (*)
executeT DIV = executeBinOpT (div)
executeT AND = executeBinOpT (intAND)
executeT OR = executeBinOpT (intOR)
executeT LSS = executeBinOpT (intLSS)
executeT GTR = executeBinOpT (intGTR)
executeT EQL = executeBinOpT (intEQL)
executeT (LOADL x) = do
  pushT x
  continueT
executeT HALT = return ()
executeT GETINT = do
  lift (putStrLn "enter a number:")
  s <- lift getLine
  pushT (read s)
  continueT
executeT PUTINT = do
  stk <- stackT
  x <- popT
  lift (putStrLn ("output > " ++ (show x)))
  continueT
executeT (Label l) = continueT
executeT (JUMP l) = do
  c <- findLabelT l
  counterUpdateT c
executeT (JUMPIFZ l) = do
  x <- popT
  c <- findLabelT l
-- if the top of the stack is zero, set PC to the given label location
  if x == 0 then counterUpdateT c
-- if the top of the stack is non-zero, continue to next instruction
            else continueT
executeT (LOAD stkAddress) = do
  stk <- stackT
  sb <- sbT
  lb <- lbT
  let address = offset stkAddress sb lb
  pushT (stk !! ((length stk - 1) - address))
  continueT
executeT (STORE stkAddress) = do
  x <- popT
  stk <- stackT
  lb <- lbT
  sb <- sbT
  let address = offset stkAddress sb lb
  let ia = length stk - address -- insertion address
  stkUpdateT (take (ia - 1) stk ++ [x] ++ drop ia stk)
  continueT
-- make activation record: push LB (dynamic link) & return address (PC+1) to stack
executeT (CALL l) = do
  stk <- stackT
  oldLb <- lbT
  pushT (oldLb)
  lbUpdateT (length stk)
  counter <- counterT
  pushT (counter+1)
  labelPosition <- findLabelT l
  counterUpdateT labelPosition
-- used at the end of a function to clear the local stack but keep the result
-- m = number of results at top of stack to return; n = number of function args
executeT (RETURN m n) = do
  stk <- stackT
  lb <- lbT
  counterUpdateT ((reverse stk) !! (lb+1))
  lbUpdateT ((reverse stk) !! lb)
  stkUpdateT ((take m stk) ++ (drop (m+n+2) stk))

-- helps executeT by abstracting operators away
executeBinOpT :: (MTInt -> MTInt -> MTInt) -> TAMSt ()
executeBinOpT op  = do
  x <- popT
  y <- popT
  pushT (y `op` x)
  continueT
executeUnOpT :: (MTInt -> MTInt) -> TAMSt ()
executeUnOpT op = do
  x <- popT
  pushT (op x)
  continueT

-- helps executeT calculate the correct absolute address from a Local/Stack Base
offset :: StkAddress -> Int -> Int -> Int
offset (SAddAbs a) _ _ = a
offset (SAddSB a) sb _ = a + sb -- global address relative to global base
offset (SAddLB a) _ lb = a + lb -- local address relative to local base

nextInst :: TAMSt TAMInst
nextInst = do code <- codeT
              counter <- counterT
              stk <- stackT
              return (code !! counter)

initTS :: [TAMInst] -> TAMState
initTS is = TAMState { tsCode=is, tsCounter=0, tsStack=[], tsLB=0, tsSB=0 }

execT :: TAMSt Stack
execT = do inst <- nextInst
           executeT inst
           if inst == HALT then stackT
                           else execT

execTAMT :: [TAMInst] -> IO Stack
execTAMT tam = do (stk,_) <- appT execT (initTS tam)
                  return stk

-- separate the tam instructions onto individual lines within a single string
writeTAM :: [TAMInst] -> String
writeTAM = foldl (\s inst -> s ++ show' inst ++ "\n") ""
  where
    show' (Label s) = if all isDigit s then "Label #" ++ s else "Label " ++ s
    show' (CALL s) = "CALL " ++ s
    show' (JUMP s) = "JUMP #" ++ s
    show' (JUMPIFZ s) = "JUMPIFZ #" ++ s
    show' i = show i

-- expression operations

boolInt :: Bool -> MTInt
boolInt False = 0
boolInt True = 1

intBool :: MTInt -> Bool
intBool x = x/=0

infixr 9 .<
(.<) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
g .< f = \ a1 a2 -> g (f a1 a2)

infixr 9 <.
(<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
g <. f = \ a1 a2 -> g (f a1) (f a2)

intAND :: MTInt -> MTInt -> MTInt
intAND = boolInt .< (&&) <. intBool

intOR :: MTInt -> MTInt -> MTInt
intOR = boolInt .< (||) <. intBool

intNOT :: MTInt -> MTInt
intNOT = boolInt . not . intBool

intLSS :: MTInt -> MTInt -> MTInt
intLSS = boolInt .< (<)

intGTR :: MTInt -> MTInt -> MTInt
intGTR = boolInt .< (>)

intEQL :: MTInt -> MTInt -> MTInt
intEQL = boolInt .< (==)

-- convert the tam file contents into typed tam instructions using the parser
parseTAM :: String -> [TAMInst]
parseTAM src = parseAll parseTAM' src

-- testing utility: returns the unparsed part of the string
parseTAMTest :: String -> [([TAMInst],String)]
parseTAMTest src = parse parseTAM' src