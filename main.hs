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

data Aexp = 
    Const Integer          -- Constant
    | Var String           -- Variable
    | Add2 Aexp Aexp        -- Addition
    | Sub2 Aexp Aexp        -- Subtraction
    | Mult2 Aexp Aexp       -- Multiplication

data Bexp = 
    BConst Bool            -- Boolean Constant
    | Eq2 Aexp Aexp         -- Equality
    | Le2 Aexp Aexp         -- Less or Equal
    | And2 Bexp Bexp        -- Logical And
    | Neg2 Bexp             -- Negation

data Stm = 
    Assign String Aexp     -- x := a
    | Seq2 Stm Stm          -- instr1 ; instr2
    | If2 Bexp Stm Stm      -- if b then s1 else s2
    | While2 Bexp Stm       -- while b do s

-- compA :: Aexp -> Code
compA :: Aexp -> Code
compA (Const n)    = [Push n]
compA (Var x)      = [Fetch x]
compA (Add2 a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (Sub2 a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (Mult2 a1 a2) = compA a2 ++ compA a1 ++ [Mult]


-- compB :: Bexp -> Code
compB :: Bexp -> Code
compB (BConst b)      = [if b then Tru else Fals]
compB (Eq2 a1 a2)     = compA a2 ++ compA a1 ++ [Equ]
compB (Le2 a1 a2)     = compA a2 ++ compA a1 ++ [Le]
compB (And2 b1 b2)    = compB b2 ++ compB b1 ++ [And]
compB (Neg2 b)        = compB b ++ [Neg]


-- compile :: Program -> Code
compileStm :: Stm -> Code
compileStm (Assign x a)   = compA a ++ [Store x]
compileStm (Seq2 s1 s2)   = compileStm s1 ++ compileStm s2
compileStm (If2 b s1 s2)  = compB b ++ [Branch (compileStm s1) (compileStm s2)]
compileStm (While2 b s)   = [Loop (compB b ++ [Neg]) (compileStm s)]


compileStm :: Stm -> Code
compileStm (Assign x a) = compA a ++ [Store x]
compileStm (Seq2 s1 s2) = compileStm s1 ++ compileStm s2
compileStm (If2 b s1 s2) = compB b ++ [Branch (compileStm s1) (compileStm s2)]
compileStm (While2 b s) = [Loop (compB b ++ [Neg]) (compileStm s)]

-- lexer that splits the input string into tokens
lexer :: String -> [String]
lexer = words  

parseAexp :: [String] -> (Aexp, [String])
parseAexp (n:rest) 
    | all isDigit n = (Const (read n), rest)  -- Parses an integer constant
    | otherwise     = (Var n, rest)          -- Parses a variable
parseAexp (op:a1:a2:rest) = case op of
    "+" -> (Add2 exp1 exp2, rest2)
    "-" -> (Sub2 exp1 exp2, rest2)
    "*" -> (Mult2 exp1 exp2, rest2)
    where
        (exp1, rest1) = parseAexp [a1]
        (exp2, rest2) = parseAexp (a2:rest)

parseBexp :: [String] -> (Bexp, [String])
parseBexp ("true":rest)   = (BConst True, rest)
parseBexp ("false":rest)  = (BConst False, rest)
parseBexp (op:a1:a2:rest) = case op of
    "==" -> (Eq2 exp1 exp2, rest2)
    "<=" -> (Le2 exp1 exp2, rest2)
    "and" -> (And2 bexp1 bexp2, rest2)
    "not" -> (Neg2 bexp1, rest1)
    where
        (exp1, rest1) = parseAexp [a1]
        (exp2, rest2) = parseAexp (a2:rest)
        (bexp1, _)    = parseBexp [a1]
        (bexp2, _)    = parseBexp (a2:rest)

parseStm :: [String] -> (Stm, [String])
parseStm (";":rest) = parseStm rest
parseStm (var:":=":rest) = 
    let (exp, rest') = parseAexp rest
        rest'' = dropWhile (/=";") rest'
    in (Assign var exp, rest'')
parseStm ("if":rest) = 
    let (bexp, rest1) = parseBexp rest
        (stm1, rest2) = parseStm rest1
        (stm2, rest3) = parseStm (dropWhile (/="else") rest2)
    in (If2 bexp stm1 stm2, drop 1 rest3)
parseStm ("while":rest) = 
    let (bexp, rest1) = parseBexp rest
        (stm, rest2) = parseStm rest1
    in (While2 bexp stm, rest2)


parseStatements :: [String] -> ([Stm], [String])
parseStatements [] = ([], [])
parseStatements tokens = 
    let (stm, restTokens) = parseStm tokens
        (stms, finalTokens) = parseStatements restTokens
    in (stm : stms, finalTokens)


-- parse :: String -> Program
parse :: String -> [Stm]
parse input = 
    let tokens = lexer input
        (stms, _) = parseStatements tokens
    in stms

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