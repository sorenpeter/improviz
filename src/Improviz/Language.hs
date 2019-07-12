{-# LANGUAGE TemplateHaskell #-}

module Improviz.Language
  ( ImprovizLanguage
  , makeLanguageState
  , initialInterpreter
  , impVMState
  , currentAst
  , updateProgram
  , resetProgram
  , saveProgram
  , userCode
  , programHasChanged
  )
where

import qualified Data.Vector                   as V

import           Language                       ( initialInterpreterState
                                                , initialImpVMState
                                                )
import           Language.Ast                   ( Program(..) )
import           Language.Interpreter.Types     ( InterpreterState )
import           Language.ImpVM.Types           ( Instruction
                                                , VMState
                                                )
import           Lens.Simple                    ( (^.)
                                                , set
                                                , view
                                                , makeLenses
                                                )
import           Gfx.Context                    ( GfxContext )


data ImprovizLanguage gfxContext = ImprovizLanguage
  { _programText        :: String
  , _lastProgramText    :: String
  , _currentAst         :: Program
  , _lastWorkingAst     :: Program
  , _currentByteCode :: V.Vector Instruction
  , _lastWorkingByteCode :: V.Vector Instruction
  , _userCode :: Program
  , _initialInterpreter :: InterpreterState
  , _impVMState :: VMState gfxContext
  }

makeLenses ''ImprovizLanguage

makeLanguageState
  :: [(FilePath, Program)] -> GfxContext -> IO (ImprovizLanguage GfxContext)
makeLanguageState userCode ctx = do
  initial <- initialInterpreterState userCode ctx
  return ImprovizLanguage
    { _programText         = ""
    , _lastProgramText     = ""
    , _currentAst          = Program []
    , _currentByteCode     = V.empty
    , _lastWorkingAst      = Program []
    , _lastWorkingByteCode = V.empty
    , _userCode = Program $ userCode >>= (\(_, Program stmts) -> stmts)
    , _initialInterpreter  = initial
    , _impVMState          = initialImpVMState ctx
    }

updateProgram
  :: String
  -> Program
  -> V.Vector Instruction
  -> ImprovizLanguage eg
  -> ImprovizLanguage eg
updateProgram newProgram newAst newByteCode il =
  set programText newProgram $ set currentAst newAst $ set currentByteCode
                                                           newByteCode
                                                           il

resetProgram :: ImprovizLanguage eg -> ImprovizLanguage eg
resetProgram as =
  let oldAst  = view lastWorkingAst as
      oldText = view lastProgramText as
      oldBC   = view lastWorkingByteCode as
  in  set programText oldText $ set currentAst oldAst $ set currentByteCode
                                                            oldBC
                                                            as

saveProgram :: ImprovizLanguage eg -> ImprovizLanguage eg
saveProgram as =
  let ast  = view currentAst as
      text = view programText as
      bc   = view currentByteCode as
  in  set lastWorkingAst ast $ set lastProgramText text $ set
        lastWorkingByteCode
        bc
        as

programHasChanged :: ImprovizLanguage eg -> Bool
programHasChanged as = as ^. currentAst /= as ^. lastWorkingAst
