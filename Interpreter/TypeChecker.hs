module TypeChecker where

import AbsSollang
import PrintSollang

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Map as Map
import Data.List as List

-- Custom types --

data InternalType = TInt | TString | TBool | TVoid deriving Eq

instance Show InternalType where
  show TInt = "int"
  show TString = "string"
  show TBool = "boolean"
  show TVoid = "void"

data FunctionType = FunctionType [ArgPass] InternalType

type VariableEnv = Map.Map Ident InternalType

type FunctionEnv = Map.Map Ident FunctionType

type TypeCheckEnvironment = (VariableEnv, FunctionEnv)

type TypeCheckResult = ExceptT String IO

type SollangTypeChecker a = ReaderT TypeCheckEnvironment TypeCheckResult a

-- Utility functions --

getErrorPositionString :: Maybe (Int, Int) -> String
getErrorPositionString Nothing = ""
getErrorPositionString (Just (x, y)) = "Error at line " ++ (show x) ++ ", column " ++ (show y)

mapToInternalType :: Type -> InternalType
mapToInternalType (Int _) = TInt
mapToInternalType (Bool _) = TBool
mapToInternalType (Str _) = TString
mapToInternalType (Void _) = TVoid

isNameFree :: Ident -> TypeCheckEnvironment -> Bool
isNameFree ident (varEnv, funcEnv) = isVar || isFunc
  where isVar = Map.member ident varEnv
        isFunc = Map.member ident funcEnv

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates as = (length as /= length as')
  where as' = List.nub as

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

getVariablesEnvironment :: SollangTypeChecker VariableEnv
getVariablesEnvironment = do
  env <- ask
  return $ fst env

lookupVariableType :: Ident -> SollangTypeChecker InternalType
lookupVariableType ident@(Ident str) = do
  variablesEnv <- getVariablesEnvironment
  case Map.lookup ident variablesEnv of {
    (Just variableType) -> return variableType;
    otherwise -> lift $ throwE $ "Variable not in scope: " ++ str;
  }

getFunctionsEnvironment :: SollangTypeChecker FunctionEnv
getFunctionsEnvironment = do
  env <- ask
  return $ snd env

lookupFunctionType :: Ident -> SollangTypeChecker FunctionType
lookupFunctionType ident@(Ident str) = do
  functionsEnv <- getFunctionsEnvironment
  case Map.lookup ident functionsEnv of {
    (Just functionType) -> return functionType;
    otherwise -> lift $ throwE $ "Function not in scope: " ++ str;
  }

-- Variables declarations type-correctness checking --

declareVariable :: Ident -> InternalType -> TypeCheckEnvironment -> TypeCheckEnvironment
declareVariable variableName variableType (varEnv, funcEnv) = (varEnv', funcEnv)
  where varEnv' = Map.insert variableName variableType varEnv

extractVariableName :: Decl -> Ident
extractVariableName (Decl _ _ variableName) = variableName
extractVariableName (DeclInit _ _ variableName _) = variableName

extractVariableType :: Decl -> InternalType
extractVariableType (Decl _ variableType _) = mapToInternalType variableType
extractVariableType (DeclInit _ variableType _ _) = mapToInternalType variableType

validateVariableType :: InternalType -> SollangTypeChecker ()
validateVariableType varType = if varType == TVoid 
                                 then lift $ throwE $ "Cannot declare void variable." else return ()

getVariableTypeFromDeclaration :: Decl -> InternalType
getVariableTypeFromDeclaration (Decl _ declaredType _) = mapToInternalType declaredType

checkVariableDeclaration :: Decl -> SollangTypeChecker TypeCheckEnvironment

checkVariableDeclaration (Decl _ variableType variableName) = do
    validateVariableType variableTypeAsInternal
    local (declareVariable variableName variableTypeAsInternal) ask
  where variableTypeAsInternal = mapToInternalType variableType

checkVariableDeclaration (DeclInit _ variableType variableName initExpr) = do
    validateVariableType variableTypeAsInternal
    expressionType <- checkExpression initExpr
    validateType variableTypeAsInternal expressionType
    local (declareVariable variableName variableTypeAsInternal) ask
  where variableTypeAsInternal = mapToInternalType variableType

checkVariablesDeclarations :: [Decl] -> SollangTypeChecker TypeCheckEnvironment
checkVariablesDeclarations [] = ask
checkVariablesDeclarations (decl:decls) = do
  env <- checkVariableDeclaration decl
  local (const env) (checkVariablesDeclarations decls)

validateVariablesNamesUniqueness :: [Decl] -> SollangTypeChecker ()
validateVariablesNamesUniqueness decls = do
    case hasDuplicates variablesNamesList of {
      True -> lift $ throwE $ "Duplicate declaration of variable.";
      False -> return ()
    }
  where variablesNamesList = Prelude.map extractVariableName decls

checkVariablesDeclarationsWrapper :: [Decl] -> SollangTypeChecker TypeCheckEnvironment
checkVariablesDeclarationsWrapper decls = do
  validateVariablesNamesUniqueness decls
  checkVariablesDeclarations decls

validateType :: InternalType -> InternalType -> SollangTypeChecker ()
validateType desiredType validatedType
  | validatedType == desiredType = return ()
  | otherwise = lift $ throwE $ "Bad type. Expected " ++ (show desiredType) ++ ". Got: " ++ (show validatedType)

validateSameTypes :: Expr -> Expr -> InternalType -> SollangTypeChecker ()
validateSameTypes firstExpr secondExpr desiredType = do
  firstExpressionType <- checkExpression firstExpr
  secondExpressionType <- checkExpression secondExpr
  validateType desiredType firstExpressionType
  validateType desiredType secondExpressionType

validateSame :: Expr -> Expr -> SollangTypeChecker ()
validateSame firstExpr secondExpr = do
  firstExpressionType <- checkExpression firstExpr
  secondExpressionType <- checkExpression secondExpr
  case firstExpressionType == secondExpressionType of {
    True -> return ();
    otherwise -> lift $ throwE $ "Type mismatch in equality - comparison between" ++ (show firstExpressionType) ++ " and " ++ (show secondExpressionType);
  }

-- Expressions type-correctness checking

checkExpression :: Expr -> SollangTypeChecker InternalType
checkExpression (EVar _ variableIdentifier) = do
  variableType <- lookupVariableType variableIdentifier
  return variableType

checkExpression (ELitInt _ _) = return TInt

checkExpression (ELitTrue _) = return TBool

checkExpression (ELitFalse _) = return TBool

checkExpression (EString _ _) = return TString

checkExpression (EApp _ functionName args) = do
  (FunctionType argumentsTypes returnType) <- lookupFunctionType functionName
  validateFunctionCallArguments argumentsTypes args
  return returnType

checkExpression (Neg _ expr) = do
  expressionType <- checkExpression expr
  validateType TInt expressionType
  return TInt

checkExpression (Not _ expr) = do
  expressionType <- checkExpression expr
  validateType TBool expressionType
  return TBool

checkExpression (EMul _ expr1 _ expr2) = do
  validateSameTypes expr1 expr2 TInt
  return TInt

checkExpression (EAdd _ expr1 _ expr2) = do
  validateSameTypes expr1 expr2 TInt
  return TInt

checkExpression (ERel _ expr1 (EQU _) expr2) = do
  firstExpressionType <- checkExpression expr1
  secondExpressionType <- checkExpression expr2
  validateSame expr1 expr2
  return TBool

checkExpression (ERel _ expr1 _ expr2) = do
  validateSameTypes expr1 expr2 TInt
  return TBool

checkExpression (EAnd _ expr1 expr2) = do
  validateSameTypes expr1 expr2 TBool
  return TBool

checkExpression (EOr _ expr1 expr2) = do
  validateSameTypes expr1 expr2 TBool
  return TBool

-- Function calls type-correctness checking

validateFunctionCallArguments :: [ArgPass] -> [Expr] -> SollangTypeChecker ()

validateFunctionCallArguments [] [] = return ()

validateFunctionCallArguments (a:as) (e:es) = do
  validateFunctionCallArgument a e
  validateFunctionCallArguments as es

validateFunctionCallArguments _ _ = lift $ throwE $ "Incorrect number of arguments provided to function call."

validateFunctionCallArgument :: ArgPass -> Expr -> SollangTypeChecker ()

validateFunctionCallArgument (ValueArg _ argType) expression = do
  expressionType <- checkExpression expression
  let argTypeAsInternalType = mapToInternalType argType
  case expressionType == argTypeAsInternalType of {
    True -> return ();
    otherwise -> lift $ throwE $ getErrorPositionString (hasPosition expression) ++ " - incorrect function call argument type.";
  }

validateFunctionCallArgument (VariableArg _ argType) (EVar p x) = do
  variableType <- lookupVariableType x
  let argTypeAsInternalType = mapToInternalType argType
  case variableType == argTypeAsInternalType of {
    True -> return ();
    otherwise -> lift $ throwE $ getErrorPositionString p ++ " - incorrect call argument type.";
  }

validateFunctionCallArgument (VariableArg p _) _ = lift $ throwE $ getErrorPositionString p ++ " - reference-call is possible only for variables."

-- Statements type-correctness checking

checkStatement :: Stmt -> SollangTypeChecker ()
checkStatement (SExp p expr) = do
  case expr of {
    app@(EApp _ _ _) -> checkExpression app >> return ();
    otherwise -> lift $ throwE $ getErrorPositionString p ++ " - only function call allowed.";
  }

checkStatement (Ass p variableIdentifier expression) = do
  expressionType <- checkExpression expression
  variableType <- lookupVariableType variableIdentifier
  case variableType == expressionType of {
    True -> return ();
    False -> lift $ throwE $ getErrorPositionString p ++ " - incorrect assignment type.";
  }

checkStatement (Print p expression) = do
  expressionType <- checkExpression expression
  if expressionType == TVoid
    then lift $ throwE $ "Cannot print void-typed expression." else return ()

checkStatement (Incr p variableIdentifier) = do
  variableType <- lookupVariableType variableIdentifier
  validateType TInt variableType

checkStatement (Decr _ variableIdentifier) = do
  variableType <- lookupVariableType variableIdentifier
  validateType TInt variableType

checkStatement (Cond _ expression conditionalStatementBlock) = do
  expressionType <- checkExpression expression
  validateType TBool expressionType
  checkBlock conditionalStatementBlock
  return ()

checkStatement (CondElse _ expression blockIfTrue blockIfFalse) = do
  expressionType <- checkExpression expression
  validateType TBool expressionType
  checkBlock blockIfTrue
  checkBlock blockIfFalse
  return ()

checkStatement (While p expression loopBlock) = do
  expressionType <- checkExpression expression
  if expressionType /= TBool 
    then lift $ throwE ((getErrorPositionString p) ++ " - expected expression of boolean type") else checkBlock loopBlock
  checkBlock loopBlock
  return ()

checkStatements :: [Stmt] -> SollangTypeChecker ()
checkStatements stmts = do
  forM_ stmts checkStatement
  return ()

-- Blocks type-correctness checking --

checkBlock :: Block -> SollangTypeChecker ()
checkBlock (Block _ blockVariablesDeclarations blockStatements) = do
  env <- checkVariablesDeclarations blockVariablesDeclarations
  local (const env) $ (checkStatements blockStatements)
  return ()

-- Function declaration correctness checking

declareFunction :: FnDef -> TypeCheckEnvironment -> TypeCheckEnvironment
declareFunction (FunctionDef _ returnType name arguments functionBody) (varEnv, funcEnv) = (varEnv, funcEnv')
  where extractArgPass :: Arg -> ArgPass
        extractArgPass (FunctionArg _ argPass _) = argPass
        returnTypeAsInternal = mapToInternalType returnType
        argsPass = Prelude.map extractArgPass arguments
        declaredFunction = FunctionType argsPass returnTypeAsInternal
        funcEnv' = Map.insert name declaredFunction funcEnv

checkFunctionDefinitionArguments :: [Arg] -> SollangTypeChecker TypeCheckEnvironment
checkFunctionDefinitionArguments arguments = do 
  validateArgumentsNamesUniqueness arguments
  checkFunctionDefinitionArgumentsTypes arguments

checkFunctionDefinitionArgument :: Arg -> SollangTypeChecker TypeCheckEnvironment
checkFunctionDefinitionArgument (FunctionArg p argPass argName) = do
    let argType = extractArgType argPass
    assertNonVoid argType
    env <- ask
    local (declareVariable argName argType) ask
  where extractArgType :: ArgPass -> InternalType
        extractArgType (ValueArg _ argType) = mapToInternalType argType
        extractArgType (VariableArg _ argType) = mapToInternalType argType
        assertNonVoid :: InternalType -> SollangTypeChecker ()
        assertNonVoid t = if t == TVoid
                            then lift $ throwE $ "Function argument cannot have void type." else return ()

checkFunctionDefinitionArgumentsTypes :: [Arg] -> SollangTypeChecker TypeCheckEnvironment
checkFunctionDefinitionArgumentsTypes [] = ask
checkFunctionDefinitionArgumentsTypes (arg:args) = do
  env <- checkFunctionDefinitionArgument arg
  local (const env) $ checkFunctionDefinitionArguments args

extractFunctionArgumentName :: Arg -> Ident
extractFunctionArgumentName (FunctionArg _ _ name) = name

validateArgumentsNamesUniqueness :: [Arg] -> SollangTypeChecker ()
validateArgumentsNamesUniqueness arguments = do 
    case hasDuplicates argumentsNamesList of {
      True -> lift $ throwE $ "Duplicate names of arguments in function definition";
      False -> return ();
    }
  where argumentsNamesList = Prelude.map extractFunctionArgumentName arguments

checkFunctionsDefinitionsWrapper :: [FnDef] -> SollangTypeChecker TypeCheckEnvironment
checkFunctionsDefinitionsWrapper fndefs = do
    case hasDuplicates functionsNames of {
      True -> lift $ throwE $ "Duplicate names of functions inside declaration block.";
      otherwise -> checkFunctionsDefinitions fndefs
    }
  where extractFunctionName :: FnDef -> Ident
        extractFunctionName (FunctionDef _ _ functionName _ _) = functionName
        functionsNames = Prelude.map extractFunctionName fndefs

checkFunctionsDefinitions :: [FnDef] -> SollangTypeChecker TypeCheckEnvironment
checkFunctionsDefinitions [] = ask
checkFunctionsDefinitions (fndef:fndefs) = do
  env <- checkFunctionDefinition fndef
  local (const env) $ checkFunctionsDefinitions fndefs

checkFunctionDefinition :: FnDef -> SollangTypeChecker TypeCheckEnvironment
checkFunctionDefinition fndef@(FunctionDef _ returnType name arguments functionBody) = do
    env <- checkFunctionDefinitionArguments arguments
    local (const $ declareFunction fndef env) $ checkFunctionBody returnTypeAsInternal functionBody
    local (declareFunction fndef) $ ask
  where returnTypeAsInternal = mapToInternalType returnType

checkFunctionBody :: InternalType -> FBody -> SollangTypeChecker ()
checkFunctionBody returnType (FunctionBody _ decls fndecls stmts returnStmt) = do
  envWithLocalVariables <- checkVariablesDeclarationsWrapper decls
  envWithLocalFunctions <- local (const envWithLocalVariables) $ checkFunctionsDefinitionsWrapper fndecls
  local (const envWithLocalFunctions) $ checkStatements stmts
  local (const envWithLocalFunctions) $ checkFunctionReturnInstruction returnType returnStmt
  return ()

checkFunctionReturnInstruction :: InternalType -> FReturn -> SollangTypeChecker ()
checkFunctionReturnInstruction TVoid (VoidReturn _) = return ()
checkFunctionReturnInstruction TVoid ret = lift $ throwE $ getErrorPositionString (hasPosition ret) ++ " - cannot return value from void-returning function"
checkFunctionReturnInstruction _ vr@(VoidReturn _) = lift $ throwE $ getErrorPositionString (hasPosition vr) ++ " - cannot return void from value-returning function"
checkFunctionReturnInstruction functionReturnType (Return p returnExpression) = do
  returnExpressionType <- checkExpression returnExpression
  if functionReturnType /= returnExpressionType
    then lift $ throwE $ "Incorrect return type from function." else return ()

{- Ensures that the main function is defined and correctly-composed.
   This includes: checking whether main function exists,
                  checking whether its return type is int,
                  checking whether it takes no arguments
-}
ensureMainFunctionIsDefinedAndCorrect :: SollangTypeChecker ()
ensureMainFunctionIsDefinedAndCorrect = do
    -- will result in an exception when main function is not defined
    mainFunctionType <- lookupFunctionType (Ident "main")
    ensureFunctionTakesNoArguments mainFunctionType
    ensureFunctionReturnsIntegerValue mainFunctionType
    return ()
  where
    ensureFunctionTakesNoArguments :: FunctionType -> SollangTypeChecker ()
    ensureFunctionTakesNoArguments (FunctionType argPassTypes _) = do
      if (isEmpty argPassTypes)
        then return () else lift $ throwE $ "The main function cannot take any arguments."
    ensureFunctionReturnsIntegerValue :: FunctionType -> SollangTypeChecker ()
    ensureFunctionReturnsIntegerValue (FunctionType _ functionReturnType) = do
      if functionReturnType == TInt 
        then return () else lift $ throwE $ "The main function should have int return type."

checkProgram :: Program -> SollangTypeChecker ()
checkProgram (Program _ (GlobalVarsDecl _ decls) functionsDefinitions) = do
  env <- checkVariablesDeclarationsWrapper decls
  env' <- local (const env) $ checkFunctionsDefinitionsWrapper functionsDefinitions
  local (const env') ensureMainFunctionIsDefinedAndCorrect
  return ()

checkTypes :: Program -> TypeCheckResult ()
checkTypes program = do
  runReaderT (checkProgram program) (Map.empty, Map.empty)
  return ()
