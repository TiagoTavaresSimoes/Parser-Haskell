-- PFL 2023/24 - Haskell practical assignment quickstart
import Data.Char (isDigit, isAlpha, isSpace, isAlphaNum)
import Data.List (intercalate, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

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
stack2Str stack = "[" ++ intercalate "," (map eitherToStr stack) ++ "]"

-- createEmptyState :: State
createEmptyState :: State
createEmptyState = []


-- stateElemToStr :: Function to convert a state element (key-value pair) to string
stateElemToStr :: (String, Either Integer Bool) -> String
stateElemToStr (key, Left i) = key ++ "=" ++ show i
stateElemToStr (key, Right b) = key ++ "=" ++ show b


--- state2Str :: State -> String
state2Str :: State -> String
state2Str state = intercalate "," (map stateElemToStr (sortBy (comparing fst) state))
  where intercalate sep = foldr (\a b -> a ++ if null b then b else sep ++ b) ""
        comparing p = compare `on` p

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- No code to run
run (inst:rest, stack, state) =
  case inst of
    Push n -> run (rest, Left n : stack, state)
    Add -> case stack of
              (Left b : Left a : xs) -> run (rest, Left (a + b) : xs, state)
              _ -> error "Add expects two integers on top of the stack"
    Mult -> case stack of
              (Left b : Left a : xs) -> run (rest, Left (a * b) : xs, state)
              _ -> error "Mult expects two integers on top of the stack"
    Sub -> case stack of
              (Left b : Left a : xs) -> run (rest, Left (a - b) : xs, state)
              _ -> error "Sub expects two integers on top of the stack"
    Tru -> run (rest, Right True : stack, state)
    Fals -> run (rest, Right False : stack, state)
    Equ -> case stack of
              (Left b : Left a : xs) -> run (rest, Right (a == b) : xs, state)
              (Right b : Right a : xs) -> run (rest, Right (a == b) : xs, state)
              _ -> error "Equ expects two values of the same type on top of the stack"
    Le -> case stack of
              (Left b : Left a : xs) -> run (rest, Right (a <= b) : xs, state)
              _ -> error "Le expects two integers on top of the stack"
    And -> case stack of
              (Right b : Right a : xs) -> run (rest, Right (a && b) : xs, state)
              _ -> error "And expects two booleans on top of the stack"
    Neg -> case stack of
              (Right a : xs) -> run (rest, Right (not a) : xs, state)
              _ -> error "Neg expects a boolean on top of the stack"
    Fetch varName -> case lookup varName state of
                        Just val -> run (rest, val : stack, state)
                        Nothing -> error ("Variable not found: " ++ varName)
    Store varName -> case stack of
                    (v : xs) -> let newState = if any ((== varName) . fst) state
                                            then map (\(k, val) -> if k == varName then (k, v) else (k, val)) state
                                            else (varName, v) : state
                                in run (rest, xs, newState)
                    [] -> error "Store expects a value on the stack"
    Noop -> run (rest, stack, state)
    Branch code1 code2 -> case stack of
                              (Right True : xs) -> run (code1 ++ rest, xs, state)
                              (Right False : xs) -> run (code2 ++ rest, xs, state)
                              _ -> error "Branch expects a boolean on top of the stack"
    Loop code1 code2 -> 
      let loopedCode = code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]]
      in run (loopedCode ++ rest, stack, state)

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
    deriving (Show)        -- Adicionando Show aqui

data Bexp = 
    BConst Bool            -- Boolean Constant
    | Eq2 Aexp Aexp         -- Equality
    | Le2 Aexp Aexp         -- Less or Equal
    | And2 Bexp Bexp        -- Logical And
    | Neg2 Bexp             -- Negation
    | BEq Bexp Bexp         -- Comparação de booleanos
    deriving (Show)        -- Adicionando Show aqui

data Stm = 
    Assign String Aexp     -- x := a
    | Seq2 Stm Stm          -- instr1 ; instr2
    | If2 Bexp Stm Stm      -- if b then s1 else s2
    | While2 Bexp Stm       -- while b do s
    deriving (Show)

-- compA :: Aexp -> Code
compA :: Aexp -> Code
compA (Const n)    = [Push n]
compA (Var x)      = [Fetch x]
compA (Add2 a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (Sub2 a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (Mult2 a1 a2) = compA a1 ++ compA a2 ++ [Mult]


-- compB :: Bexp -> Code
compB :: Bexp -> Code
compB (BConst b)      = [if b then Tru else Fals]
compB (Eq2 a1 a2)     = compA a2 ++ compA a1 ++ [Equ]
compB (BEq b1 b2)     = compB b2 ++ compB b1 ++ [Equ]  -- Adicionando comparação de booleanos
compB (Le2 a1 a2)     = compA a2 ++ compA a1 ++ [Le]
compB (And2 b1 b2)    = compB b2 ++ compB b1 ++ [And]
compB (Neg2 b)        = compB b ++ [Neg]


-- compile :: Program -> Code
compileStm :: Stm -> Code
compileStm (Assign x a) = compA a ++ [Store x]
compileStm (Seq2 s1 s2) = compileStm s1 ++ compileStm s2
compileStm (If2 b s1 s2) = compB b ++ [Branch (compileStm s1) (compileStm s2)]
compileStm (While2 b s) = [Loop (compB b ++ [Neg]) (compileStm s)]

-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")

-- compile :: [Stm] -> Code
compile :: [Stm] -> Code
compile [] = []
compile (stm:stms) = compileStm stm ++ compile stms


-- lexer that splits the input string into tokens
lexer :: String -> [String]
lexer str = 
    let tokens = words . map (\c -> if c `elem` [';', '(', ')'] then ' ' else c) $ str
    in if validateTokens tokens then tokens else error "Invalid syntax in input"

validateTokens :: [String] -> Bool
validateTokens = all isValidToken
  where
    isValidToken t = all isDigit t || isValidVarName t || t `elem` allowedTokens
    allowedTokens = ["+", "-", "*", "/", "(", ")", "not", "==", "<=", "=", "and", ";", ":="]

isValidVarName :: String -> Bool
isValidVarName name = isAlpha (head name) && all isAlphaNum name


parseAexp :: [String] -> (Aexp, [String])
parseAexp (var:"-":n:rest) 
    | isAlpha (head var) && all isDigit n = (Sub2 (Var var) (Const (read n)), rest)
parseAexp (n:rest) 
    | all isDigit n = (Const (read n), rest)  -- Trata números
    | otherwise     = (Var n, rest)          -- Trata variáveis
parseAexp (op:a1:a2:rest)
    | op `elem` ["+", "-", "*"] =
        let (exp1, rest1) = parseAexp [a1]
            (exp2, rest2) = parseAexp (a2:rest)
        in case op of
            "+" -> (Add2 exp1 exp2, rest2)
            "-" -> (Sub2 exp1 exp2, rest2)
            "*" -> (Mult2 exp1 exp2, rest2)
    | otherwise = error $ "Unrecognized operator in parseAexp: " ++ op

parseBexp :: [String] -> (Bexp, [String])
parseBexp tokens = trace ("Parsing Bexp: " ++ show tokens) $ case tokens of
    ("(":rest) -> 
        let (bexp, ")":rest') = parseBexp rest
        in trace ("Parsed '(': " ++ show bexp ++ ", rest: " ++ show rest') (bexp, rest')
    ("true":rest) -> trace "Parsed 'true'" $ (BConst True, rest)
    ("True":rest) -> trace "Parsed 'True'" $ (BConst True, rest)
    ("false":rest) -> trace "Parsed 'false'" $ (BConst False, rest)
    ("False":rest) -> trace "Parsed 'False'" $ (BConst False, rest)
    ("not":a1:rest) -> 
        let (bexp1, rest1) = trace "Parsing 'not' expression" $ parseBexp rest
        in trace ("Parsed 'not': " ++ show bexp1 ++ ", rest: " ++ show rest1) (Neg2 bexp1, rest1)
    (op:a1:a2:rest)
        | op == "=" -> 
            let (bexp1, rest1) = trace ("Parsing left expression of '=': " ++ show a1) $ parseBexp (a1:rest)
                (bexp2, rest2) = trace ("Parsing right expression of '=': " ++ show a2) $ parseBexp (a2:rest1)
            in trace ("Comparing expressions: " ++ show bexp1 ++ " and " ++ show bexp2) (BEq bexp1 bexp2, rest2)
        | otherwise -> case op of
            "==" -> 
                let (exp1, rest1) = trace ("Parsing '==' left expression: " ++ show a1) $ parseAexp [a1]
                    (exp2, rest2) = trace ("Parsing '==' right expression: " ++ show a2) $ parseAexp (a2:rest)
                in trace ("Parsed '==': " ++ show (Eq2 exp1 exp2) ++ ", rest: " ++ show rest2) (Eq2 exp1 exp2, rest2)
            "<=" -> 
                let (exp1, rest1) = trace ("Parsing '<=' left expression: " ++ show a1) $ parseAexp [a1]
                    (exp2, rest2) = trace ("Parsing '<=' right expression: " ++ show a2) $ parseAexp (a2:rest)
                in trace ("Parsed '<=': " ++ show (Le2 exp1 exp2) ++ ", rest: " ++ show rest2) (Le2 exp1 exp2, rest2)
            "and" -> 
                let (bexp1, rest1) = trace ("Parsing 'and' left expression: " ++ show a1) $ parseBexp [a1]
                    (bexp2, rest2) = trace ("Parsing 'and' right expression: " ++ show a2) $ parseBexp (a2:rest)
                in trace ("Parsed 'and': " ++ show (And2 bexp1 bexp2) ++ ", rest: " ++ show rest2) (And2 bexp1 bexp2, rest2)
            _ -> error $ "Unrecognized boolean operator: " ++ op
    xs -> error $ "Unrecognized pattern in parseBexp: " ++ show xs



parseStm :: [String] -> (Stm, [String])
parseStm (";":rest) = parseStm rest
parseStm (var:":=":rest) =
    let (exp, rest') = parseAexp rest
    in (Assign var exp, dropWhile (== ";") rest')
parseStm ("if":rest) = 
    let (bexp, restAfterBexp) = parseBexp rest
        restAfterThen = case restAfterBexp of
            ("then":restThen) -> restThen
            _ -> error "Expected 'then' after if condition"
        (stm1, rest2) = parseStm restAfterThen
        rest2' = case rest2 of
            ("else":restElse) -> restElse
            _ -> error "Expected 'else' after then block"
        (stm2, rest3) = parseStm rest2'
    in (If2 bexp stm1 stm2, rest3)
parseStm ("while":rest) = 
    let (bexp, rest1) = parseBexp rest
        (stm, rest2) = parseStm rest1
    in (While2 bexp stm, rest2)
parseStm rest = error $ "Unrecognized pattern in parseStm: " ++ show rest

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
testParser :: String -> String
testParser programCode = show $ parse programCode


main :: IO ()
main = do
  test "x := 5;"
  test "if x <= 5 then x := x + 1 else x := x - 1;"

  where
    test str = putStrLn $ "Teste: " ++ str ++ "\nResultado: " ++ testParser str



-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")