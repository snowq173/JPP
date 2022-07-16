module Interpreter where

import AbsSollang
import PrintSollang

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Map as Map

data Value = VI Integer | VB Bool | VS String | Undefined deriving Eq

instance Show Value where
  show (VI integerValue) = show integerValue
  show (VB booleanValue) = show booleanValue
  show (VS stringValue) = show stringValue
  show Undefined = "undefined"

type Var = Ident

type Loc = Int

type Store = Map.Map Loc Value

newtype Function = Function ([Expr] -> SollangInterpreter Value)

type VariablesEnvironment = Map.Map Ident Loc

type FunctionsEnvironment = Map.Map Ident Function

type Environment = (VariablesEnvironment, FunctionsEnvironment)

type Result = ExceptT String IO

type SollangInterpreter a = StateT Store (ReaderT Environment Result) a

-- Identifier of the main function in program
mainFunctionIdentifier :: Ident
mainFunctionIdentifier = Ident "main"

-- Values printing utility function

printValue :: Value -> SollangInterpreter ()

printValue value = do
  liftIO $ putStrLn $ show value
  return ()

-- Values manipulation utility functions

addToIntegerValue :: Integer -> Value -> Value
addToIntegerValue addedVal (VI value) = VI $ value + addedVal

-- Location management utility functions

getNextFreeLocation :: Store -> Loc
getNextFreeLocation store
    | Map.null store = 1
    | otherwise = maximalLoc + 1
  where maximalLoc = fst $ findMax store

allocate :: SollangInterpreter Loc
allocate = do
  store <- get
  let location = getNextFreeLocation store
  put $ insert location Undefined store
  return location

-- Lookup utility functions

lookupVariable :: Var -> SollangInterpreter Loc
lookupVariable variableName = do
  env <- ask
  return $ (fst env)!variableName

lookupFunction :: Ident -> SollangInterpreter Function
lookupFunction functionName = do
  env <- ask
  return $ (snd env)!functionName

-- Environment modification utility functions

declareVariable :: Ident -> Loc -> Environment -> Environment
declareVariable variableName variableLocation (variablesEnv, functionsEnv) = (variablesEnv', functionsEnv)
  where variablesEnv' = Map.insert variableName variableLocation variablesEnv

putValueAt :: Loc -> Value -> Store -> Store
putValueAt location value = Map.insert location value

declareFunction :: Ident -> Function -> Environment -> Environment
declareFunction functionName declaredFunction (variablesEnv, functionsEnv) = (variablesEnv, functionsEnv')
  where functionsEnv' = Map.insert functionName declaredFunction functionsEnv

-- Variables declarations evaluation

evaluateVariablesDeclarations :: [Decl] -> SollangInterpreter Environment

evaluateVariablesDeclarations [] = ask

evaluateVariablesDeclarations (declaration:declarations) = do
  env <- evaluateVariableDeclaration declaration
  local (const env) $ evaluateVariablesDeclarations declarations

evaluateVariableDeclaration :: Decl -> SollangInterpreter Environment

evaluateVariableDeclaration (Decl _ variableType variableName) = do
  location <- allocate
  let variableValue = case variableType of {
    Int _ -> VI 0;
    Bool _ -> VB False;
    Str _ -> VS "";
  }
  modify $ putValueAt location variableValue
  local (declareVariable variableName location) $ ask

evaluateVariableDeclaration (DeclInit _ _ variableName initExpr) = do
  location <- allocate
  expressionValue <- evaluateExpression initExpr
  modify $ putValueAt location expressionValue
  local (declareVariable variableName location) $ ask

-- Functions definitions evaluation

evaluateFunctionsDefinitions :: [FnDef] -> SollangInterpreter Environment

evaluateFunctionsDefinitions [] = ask

evaluateFunctionsDefinitions (fndef:fndefs) = do
  env <- evaluateFunctionDefinition fndef
  local (const env) $ evaluateFunctionsDefinitions fndefs

evaluateFunctionDefinition :: FnDef -> SollangInterpreter Environment

evaluateFunctionDefinition (FunctionDef _ _ functionName args (FunctionBody _ lvs lfns stmts retIns)) = do
    local (declareFunction functionName function) ask
  where function = (Function fn)
        fn callExpressions = do
         envWithArgs <- evaluateFunctionCallArgs args callExpressions -- evaluate arguments (reference call, value call)
         envWithLocalVariables <- local (const envWithArgs) $ evaluateVariablesDeclarations lvs -- local variables for function
         let envWithSelf = declareFunction functionName function envWithLocalVariables -- recursive calls to the function
         fullEnv <- local (const envWithSelf) $ evaluateFunctionsDefinitions lfns -- nested functions
         local (const fullEnv) $ evaluateStatements stmts
         case retIns of {
           (VoidReturn _) -> return Undefined;
           (Return _ expr) -> local (const fullEnv) $ evaluateExpression expr;
         }

evaluateFunctionCallArgs :: [Arg] -> [Expr] -> SollangInterpreter Environment

evaluateFunctionCallArgs [] [] = ask

evaluateFunctionCallArgs (arg:args) (expr:exprs) = do
  env <- evaluateFunctionCallArg arg expr
  local (const env) $ evaluateFunctionCallArgs args exprs

evaluateFunctionCallArg :: Arg -> Expr -> SollangInterpreter Environment

evaluateFunctionCallArg (FunctionArg _ (VariableArg _ _) argumentName) (EVar _ variableName) = do
  variableLoc <- lookupVariable variableName
  local (declareVariable argumentName variableLoc) $ ask

evaluateFunctionCallArg (FunctionArg _ _ argumentName) expr = do
  loc <- allocate
  expressionValue <- evaluateExpression expr
  modify (putValueAt loc expressionValue)
  local (declareVariable argumentName loc) $ ask

-- Expressions evaluation

evaluateExpression :: Expr -> SollangInterpreter Value

evaluateExpression (EVar _ x) = do
  variableLocation <- lookupVariable x
  store <- get
  return $ store!variableLocation

evaluateExpression (ELitInt _ n) = return $ VI n

evaluateExpression (ELitTrue _) = return $ VB True

evaluateExpression (ELitFalse _) = return $ VB False

evaluateExpression (EString _ s) = return $ VS s

evaluateExpression (EApp _ functionName expressions) = do
  (Function functionToCall) <- lookupFunction functionName
  functionToCall expressions

evaluateExpression (Neg _ expression) = do
  (VI value) <- evaluateExpression expression
  return $ VI $ negate value

evaluateExpression (Not _ expression) = do
  (VB value) <- evaluateExpression expression
  return $ VB $ not value

evaluateExpression (EMul _ firstExpression operation secondExpression) = do
  (VI first) <- evaluateExpression firstExpression
  (VI second) <- evaluateExpression secondExpression
  case operation of {
    Times _ -> return $ VI (first * second);
    Div _ -> case second of {
      0 -> lift $ lift $ throwE $ "Error: division by 0.";
      otherwise -> return $ VI (first `div` second);
    };
    Mod _ -> case second of {
      0 -> lift $ lift $ throwE $ "Error: taking modulo 0.";
      otherwise -> return $ VI (first `mod` second);
    }
  }

evaluateExpression (EAdd _ expr1 addOp expr2) = do
  (VI first) <- evaluateExpression expr1
  (VI second) <- evaluateExpression expr2
  case addOp of {
    (Plus _) -> return $ VI $ first + second;
    (Minus _) -> return $ VI $ first - second;
  }

evaluateExpression (ERel _ expr1 rel expr2) = do
  (VI first) <- evaluateExpression expr1
  (VI second) <- evaluateExpression expr2
  case rel of {
    LTH _ -> return $ VB (first < second);
    LE _ -> return $ VB (first <= second);
    GTH _ -> return $ VB (first > second);
    GE _ -> return $ VB (first >= second);
    EQU _ -> return $ VB (first == second);
  }

evaluateExpression (EAnd _ expr1 expr2) = do
  (VB first) <- evaluateExpression expr1
  case first of {
    False -> return $ VB False;
    otherwise -> evaluateExpression expr2;
  }

evaluateExpression (EOr _ expr1 expr2) = do
  (VB first) <- evaluateExpression expr1
  case first of {
    True -> return $ VB True;
    otherwise -> evaluateExpression expr2;
  }

-- Blocks evaluation

evaluateBlock :: Block -> SollangInterpreter ()
evaluateBlock (Block _ decls stmts) = do
  env <- evaluateVariablesDeclarations decls
  evaluateStatements stmts
  return ()

-- Statements evaluation

evaluateStatement :: Stmt -> SollangInterpreter ()

evaluateStatement (SExp p expr) = do
  evaluateExpression expr
  return ()

evaluateStatement (Ass p variableIdentifier expression) = do
  expressionValue <- evaluateExpression expression
  location <- lookupVariable variableIdentifier
  modify $ putValueAt location expressionValue
  return ()

evaluateStatement (Print p expression) = do
  expressionValue <- evaluateExpression expression
  printValue expressionValue
  return ()

evaluateStatement (Incr p variableIdentifier) = do
  variableLocation <- lookupVariable variableIdentifier
  store <- get
  let variableValue = store!variableLocation
  modify $ putValueAt variableLocation (addToIntegerValue 1 variableValue)
  return ()

evaluateStatement (Decr _ variableIdentifier) = do
  variableLocation <- lookupVariable variableIdentifier
  store <- get
  let variableValue = store!variableLocation
  modify $ putValueAt variableLocation (addToIntegerValue (-1) variableValue)
  return ()

evaluateStatement (Cond _ expression conditionalStatementBlock) = do
  expressionValue <- evaluateExpression expression
  case expressionValue of {
    (VB True) -> evaluateBlock conditionalStatementBlock;
    otherwise -> return ();
  }

evaluateStatement (CondElse _ expression blockIfTrue blockIfFalse) = do
  expressionValue <- evaluateExpression expression
  case expressionValue of {
    (VB True) -> evaluateBlock blockIfTrue;
    otherwise -> evaluateBlock blockIfFalse;
  }

evaluateStatement whileStmt@(While _ expression loopBlock) = do
  expressionValue <- evaluateExpression expression
  case expressionValue of {
    (VB True) -> evaluateBlock loopBlock >> evaluateStatement whileStmt;
    otherwise -> return ();
  }

evaluateStatements :: [Stmt] -> SollangInterpreter ()
evaluateStatements statements = do
  forM_ statements evaluateStatement
  return ()

-- Program evaluation

evaluateProgram :: Program -> SollangInterpreter ()
evaluateProgram (Program _ (GlobalVarsDecl _ decls) functionsDefinitions) = do
  envWithGlobalVariables <- evaluateVariablesDeclarations decls
  envWithFunctions <- local (const envWithGlobalVariables) $ evaluateFunctionsDefinitions functionsDefinitions
  (Function mainFunction) <- local (const envWithFunctions) $ lookupFunction mainFunctionIdentifier
  local (const envWithFunctions) $ mainFunction []
  return ()

runInterpreter :: Program -> Result ()
runInterpreter program = do
  runReaderT (execStateT (evaluateProgram program) Map.empty) (Map.empty, Map.empty)
  return ()
