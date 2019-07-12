module Language
  ( parse
  , compile
  , initialInterpreterState
  , setInterpreterVariables
  , interpret
  , initialImpVMState
  , module Language.Ast
  )
where

import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Vector                   as V

import           Control.Monad                  ( forM_ )
import           Control.Monad.Trans            ( liftIO )
import           Lens.Simple                    ( set )

import           Gfx.Context                    ( GfxContext )

import           Language.Parser                ( parseProgram )
import           Language.Parser.Errors         ( ParserError )
import qualified Language.Compiler             as LC
import           Language.Ast.Transformers      ( transform )
import           Language.Ast                   ( Identifier
                                                , Program(..)
                                                , Value(..)
                                                )
import           Language.ImpVM                 ( cleanVM )
import           Language.ImpVM.Types           ( VMState
                                                , Instruction
                                                )
import           Language.Interpreter           ( emptyState
                                                , getGlobalNames
                                                , interpretLanguage
                                                , setGlobal
                                                )
import           Language.Interpreter.Types     ( InterpreterState
                                                , gfxContext
                                                , runInterpreterM
                                                , externals
                                                )
import           Language.Interpreter.StdLib    ( addStdLib )
import           Logging                        ( logInfo )


parse :: String -> Either ParserError Program
parse = parseProgram

compile
  :: S.Set String -> Program -> Program -> Either String (V.Vector Instruction)
compile globals (Program defaultStmts) (Program progStmts) =
  let stmts = defaultStmts ++ progStmts
  in  LC.compile $ transform globals (Program stmts)

initialInterpreterState
  :: [(FilePath, Program)] -> GfxContext -> IO InterpreterState
initialInterpreterState userCode ctx =
  let langState = set gfxContext ctx emptyState
      setup     = do
        addStdLib
        globals <- getGlobalNames
        mapM (load globals) userCode
  in  snd <$> runInterpreterM setup langState
 where
  load globals (fp, code) = do
    liftIO $ logInfo ("Loading " ++ fp)
    interpretLanguage $ transform globals code

setInterpreterVariables
  :: [(Identifier, Value)]
  -> M.Map String Value
  -> InterpreterState
  -> IO InterpreterState
setInterpreterVariables globals externalVars is =
  let setVars = forM_ globals (uncurry setGlobal)
  in  do
        (_, newState) <- runInterpreterM setVars is
        return $ set externals externalVars newState

interpret
  :: InterpreterState -> Program -> IO (Either String Value, InterpreterState)
interpret initialState program =
  let run = do
        globals <- getGlobalNames
        interpretLanguage (transform globals program)
  in  runInterpreterM run initialState

initialImpVMState :: GfxContext -> VMState GfxContext
initialImpVMState ctx = cleanVM ctx M.empty
