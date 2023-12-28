-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- define Stack and State types
type Stack = [Either Integer Bool]
type State = [(String, Either Integer Bool)]

-- createEmptyStack :: Stack
createEmptyStack :: Stack
createEmptyStack = []

-- Function to convert a stack element (either integer bool) to string
eitherToStr :: Either Integer Bool -> String
eitherToStr (Left i) = show i
eitherToStr (Right b) = show b


-- stack2Str :: Stack -> String
stack2Str :: Stack -> String
stack2Str stack = "[" ++ (intercalate ", " (map eitherToStr stack)) ++ "]"
  where intercalate sep = foldr (\a b -> a ++ if null b then b else sep ++ b) ""

-- createEmptyState :: State
createEmptyState :: State
createEmptyState = []


-- stateElemToStr :: Function to convert a state element (key-value pair) to string
stateElemToStr :: (String, Either Integer Bool) -> String
stateElemToStr (key, Left i) = key ++ "=" ++ show i
stateElemToStr (key, Right b) = key ++ "=" ++ show b


-- state2Str :: State -> String
state2Str :: State -> String
state2Str state = intercalate "," (map stateElemToStr state)
  where intercalate sep = foldr (\a b -> a ++ if null b then b else sep ++ b) ""

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- No code to run
run ((inst:rest), stack, state) = 
    case inst of
        Push n -> run (rest, (Left n):stack, state)
        Add -> case stack of
                  (Left a : Left b : xs) -> run (rest, Left (a + b) : xs, state)
                  _ -> error "Run-time error: Add expects two integers"
        Mult -> case stack of
                  (Left a : Left b : xs) -> run (rest, Left (b * a) : xs, state)
                  _ -> error "Run-time error: Mult expects two integers"
        Sub -> case stack of
                  (Left a : Left b : xs) -> run (rest, Left (b - a) : xs, state)
                  _ -> error "Run-time error: Sub expects two integers"
        Tru -> run (rest, (Right True):stack, state)
        Fals -> run (rest, (Right False):stack, state)
        Equ -> case stack of
                  (Left a : Left b : xs) -> run (rest, Right (a == b) : xs, state)
                  (Right a : Right b : xs) -> run (rest, Right (a == b) : xs, state)
                  _ -> error "Run-time error: Equ expects two values of the same type"
        Le -> case stack of
                  (Left a : Left b : xs) -> run (rest, Right (b <= a) : xs, state)
                  _ -> error "Run-time error: Le expects two integers"
        And -> case stack of
                  (Right a : Right b : xs) -> run (rest, Right (a && b) : xs, state)
                  _ -> error "Run-time error: And expects two booleans"
        Neg -> case stack of
                  (Right a : xs) -> run (rest, Right (not a) : xs, state)
                  _ -> error "Run-time error: Neg expects a boolean"
        Fetch varName -> case lookup varName state of
                            Just val -> run (rest, val : stack, state)
                            Nothing -> error ("Variable " ++ varName ++ " not found")
        Store varName -> case stack of
                            (v : xs) -> run (rest, xs, (varName, v) : state)
                            [] -> error "Run-time error: Store expects a value on the stack"
        Noop -> run (rest, stack, state)
        Branch code1 code2 -> case stack of
                                (Right True : xs) -> run (code1 ++ rest, xs, state)
                                (Right False : xs) -> run (code2 ++ rest, xs, state)
                                _ -> error "Run-time error: Branch expects a boolean on the stack"
        Loop code1 code2 -> run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ rest, stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")