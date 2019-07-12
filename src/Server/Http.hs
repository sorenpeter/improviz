{-# LANGUAGE OverloadedStrings #-}

module Server.Http
  ( startHttpServer
  )
where

import           Network.Wai.Handler.Warp
import           Web.Scotty

import           Control.Concurrent             ( ThreadId
                                                , forkIO
                                                )
import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , modifyTVar
                                                , readTVarIO
                                                )
import           Control.Monad.Trans            ( liftIO )
import           System.FilePath.Posix          ( (</>) )

import           Data.ByteString.Lazy.Char8     ( pack
                                                , unpack
                                                )
import qualified Data.Map.Strict               as M
import           Logging                        ( logError
                                                , logInfo
                                                )

import qualified Language                      as L
import           Language.Ast                   ( Program
                                                , Value(Number)
                                                )
import           Language.Parser.Errors         ( prettyPrintErrors
                                                , parseErrorsOut
                                                )
import           Language.ImpVM.Types           ( builtins )

import           Server.Protocol

import qualified Configuration                 as C
import           Improviz                       ( ImprovizEnv )
import qualified Improviz                      as I
import qualified Improviz.Language             as IL
import           Improviz.UI                    ( ImprovizUI )
import qualified Improviz.UI                   as IUI

import           Lens.Simple                    ( set
                                                , (^.)
                                                , view
                                                )

editorHtmlFilePath :: FilePath
editorHtmlFilePath = "html/editor.html"

updateProgram :: ImprovizEnv -> String -> IO ImprovizResponse
updateProgram env newProgram = case L.parse newProgram of
  Right newAst -> do
    msg <- updateEnv env newProgram newAst
    case msg of
      Right m -> do
        logInfo m
        return $ ImprovizOKResponse m
      Left m -> do
        logError m
        return $ ImprovizErrorResponse m
  Left err -> do
    logError $ prettyPrintErrors err
    return $ ImprovizCodeErrorResponse $ parseErrorsOut err

updateEnv :: ImprovizEnv -> String -> Program -> IO (Either String String)
updateEnv env newProgram newAst = do
  il <- readTVarIO $ env ^. I.language
  let globals     = M.keysSet $ view (IL.impVMState . builtins) il
  let defaultCode = view IL.userCode il
  case L.compile globals defaultCode newAst of
    Left  err -> return $ Left err
    Right bc  -> do
      atomically $ do
        modifyTVar (env ^. I.language) (IL.updateProgram newProgram newAst bc)
        modifyTVar (env ^. I.ui)       (set IUI.currentText newProgram)
      return $ Right "Updated Successfully"

toggleTextDisplay :: TVar ImprovizUI -> IO ImprovizResponse
toggleTextDisplay ui = do
  atomically $ modifyTVar ui IUI.toggleTextDisplay
  let msg = "Text display toggled"
  logInfo msg
  return $ ImprovizOKResponse msg

updateExternalVar :: ImprovizEnv -> String -> Float -> IO ImprovizResponse
updateExternalVar env name value = do
  atomically $ modifyTVar (env ^. I.externalVars) (M.insert name (Number value))
  return $ ImprovizOKResponse $ name ++ " variable updated"


startHttpServer :: ImprovizEnv -> IO ThreadId
startHttpServer env =
  let port     = (env ^. I.config . C.serverPort)
      settings = setPort port defaultSettings
      options  = Options { verbose = 0, settings = settings }
  in  do
        logInfo $ "Improviz HTTP server listening on port " ++ show port
        forkIO $ scottyOpts options $ do
          get "/" $ text "SERVING"
          get "/editor" $ do
            html <-
              liftIO
              $   readFile
              $   (env ^. I.config . C.assetsDirectory)
              </> editorHtmlFilePath
            raw $ pack html
          post "/read" $ do
            b    <- body
            resp <- liftIO $ updateProgram env (unpack b)
            json resp
          post "/toggle/text" $ do
            resp <- liftIO $ toggleTextDisplay (env ^. I.ui)
            json resp
          post "/vars/edit/:name" $ do
            name <- param "name"
            b    <- body
            case reads (unpack b) of
              [(v, _)] -> do
                resp <- liftIO $ updateExternalVar env name v
                json resp
              _ ->
                json
                  $  ImprovizErrorResponse
                  $  name
                  ++ " variable not updated. Value invalid"
