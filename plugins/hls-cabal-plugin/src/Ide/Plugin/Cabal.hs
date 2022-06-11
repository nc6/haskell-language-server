{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ide.Plugin.Cabal where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text as T
import           Development.IDE            as D
import           GHC.Generics (Generic)
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens as J
import Control.Lens ((^.), to)
import Cabal.Package (parsePackage)
import qualified Data.Text.Encoding as T
import Data.List.NonEmpty
import qualified Data.ByteString as BS
import qualified Distribution.PackageDescription as C
import qualified Distribution.Fields as C
import qualified Cabal.Parse as C
import Control.Arrow (left, right)
import qualified Distribution.Utils.ShortText as C


newtype Log = LogText T.Text deriving Show

instance Pretty Log where
  pretty = \case
    LogText log -> pretty log

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultCabalPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeLens       (codeLens recorder)
                     <> mkPluginHandler STextDocumentHover hover
  }

-- ---------------------------------------------------------------------

codeLens :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState TextDocumentCodeLens
codeLens recorder _ideState plId CodeLensParams{_textDocument=TextDocumentIdentifier uri} = liftIO $ do
    log Debug $ LogText "ExampleCabal.codeLens entered (ideLogger)"
    case uriToFilePath' uri of
      Just (toNormalizedFilePath -> _filePath) -> do
        let
          title = "Add TODO Item via Code Lens"
          range = Range (Position 3 0) (Position 4 0)
        let cmdParams = AddTodoParams uri "do abc"
            cmd = mkLspCommand plId "codelens.todo" title (Just [toJSON cmdParams])
        pure $ Right $ List [ CodeLens range (Just cmd) Nothing ]
      Nothing -> pure $ Right $ List []
  where
    log = logWith recorder
-- ---------------------------------------------------------------------

hover :: PluginMethodHandler IdeState TextDocumentHover
hover ide _ hoverParams = liftIO $ readPackage' uri
  where
    uri = hoverParams ^. J.textDocument . J.uri . to getUri . to T.unpack
    readPackage' :: FilePath -> IO (Either ResponseError (Maybe Hover))
    readPackage' fp = do
      contents <- BS.readFile fp
      pure . left mkResponseError . right mkResponse $ parsePackage fp contents
    mkResponseError :: C.ParseError NonEmpty -> ResponseError
    mkResponseError (C.ParseError {C.peErrors}) = case peErrors of
        (C.PError _pos str) :| _ -> ResponseError ParseError (T.pack str) Nothing
    mkResponse :: C.GenericPackageDescription -> Maybe Hover
    mkResponse (C.packageDescription -> C.PackageDescription { C.maintainer } )
        = Just $ Hover (HoverContentsMS $ List [PlainString . T.pack $ C.fromShortText maintainer]) Nothing

-- ---------------------------------------------------------------------

data AddTodoParams = AddTodoParams
  { file     :: Uri  -- ^ Uri of the file to add the pragma to
  , todoText :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
