{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-|
Module      : Foreign.Nix.Shellout
Description : Interface to the nix package manager’s CLI
Copyright   : Profpatsch, 2016–2018
License     : GPL-3
Stability   : experimental
Portability : nix 1.11.x, nix 2.0

Calls to the nix command line to convert
textual nix expressions to derivations & realized storepaths.
-}
module Foreign.Nix.Shellout
( -- * Calling nix
  -- ** Parse
  parseNixExpr, ParseError(..)
  -- ** Instantiate
, instantiate, instantiateOne, InstantiateError(..)
, eval, evalJSON
  -- ** Realize
, realize, RealizeError(..)
  -- ** Build
, buildPaths, BuildError(..)
  -- ** Helpers
, addToStore
, parseInstRealize
, parseInstRealizeMany
, NixError(..)
, throwErrorWithoutStderr
  -- * Types
, NixExpr

  -- * File paths
, nixFilePath

  -- * Re-exports
, module Foreign.Nix.Shellout.Types
) where

import Control.Error ( throwE, tryLast, ExceptT)
import Data.Text (stripPrefix, lines, isPrefixOf, Text)

import qualified Data.Aeson as Aeson
import qualified Foreign.Nix.Shellout.Helpers as Helpers
import Foreign.Nix.Shellout.Types
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextE
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import Control.Monad ((>=>))
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory
import Data.Function ((&))
import qualified Data.List as List
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Except (throwError)

------------------------------------------------------------------------------
-- Parsing

-- | A sucessfully parsed nix expression.
newtype NixExpr = NixExpr
  { unNixExpr :: Text
  } deriving (Show)

data ParseError
  = SyntaxError Text
    -- ^ the input string was not a syntactically valid nix expression
  | UnknownParseError
    -- ^ catch-all error
  deriving (Show, Eq)

-- | Parse a nix expression and check for syntactic validity.

-- Runs @nix-instantiate@.
parseNixExpr :: MonadIO m => Text -> NixAction ParseError m NixExpr
parseNixExpr e = do
  exec <- Helpers.getExecOr exeNixInstantiate "nix-instantiate"
  mapActionError parseParseError
    $ NixExpr
    <$> evalNixOutputLast exec [ "--parse", "-E", e ]


parseParseError :: Text -> ParseError
parseParseError
  (stripPrefix "error: syntax error, "
               -> Just mes) = SyntaxError mes
parseParseError _           = UnknownParseError

------------------------------------------------------------------------------
-- Instantiating

data InstantiateError
  = NotADerivation
    -- ^ the given expression does not evaluate to a derivaton
  | JSONParseError Text
  | UnknownInstantiateError
    -- ^ catch-all error
  deriving (Show, Eq)

-- | Instantiate parsed expressions to derivations.
--
-- Runs @nix-instantiate@.
instantiate :: (MonadIO m) => [NixExpr] -> NixAction InstantiateError m [StorePath Derivation]
instantiate (fmap unNixExpr -> exprs) = do
  exec <- Helpers.getExecOr exeNixInstantiate "nix-instantiate"
  mapActionError parseInstantiateError
    $ evalNixOutput exec ( "-E" : exprs )
      >>= traverse (fromNixFilePath StorePath)

-- | Instantiate a parsed expression to derivations.
--
-- Runs @nix-instantiate@.
instantiateOne :: (MonadIO m) => NixExpr -> NixAction InstantiateError m [StorePath Derivation]
instantiateOne = instantiate . pure

-- | Evaluate Nix expressions using @--eval --strict -E@, plus arbitrary args at the end.
evalArgs :: MonadIO m => [NixExpr] -> [Text] -> NixAction InstantiateError m [Text]
evalArgs (fmap unNixExpr -> exprs) args = do
  exec <- Helpers.getExecOr exeNixInstantiate "nix-instantiate"

  mapActionError parseInstantiateError
       $ evalNixOutput exec ( "--eval" : "--strict" : "-E" : exprs <> args)

-- | Just tests if the expression can be evaluated.
-- That doesn’t mean it has to instantiate however.
--
-- Runs @nix-instantiate@.
eval :: MonadIO m => [NixExpr] -> NixAction InstantiateError m [Text]
eval exprs = evalArgs exprs []

-- | Run nix-instantiate --eval --json and parse the JSON output.
evalJSON :: (Aeson.FromJSON a, MonadIO m) => [NixExpr] -> NixAction InstantiateError m [a]
evalJSON exprs = do
  out <- evalArgs exprs ["--json"]
  case traverse (Aeson.eitherDecodeStrict' . TextE.encodeUtf8) out of
    Right as -> pure as
    Left err -> throwErrorWithoutStderr $ JSONParseError (Text.pack err)

parseInstantiateError :: Text -> InstantiateError
parseInstantiateError
  (stripPrefix "error: expression does not evaluate to a derivation"
               -> Just _) = NotADerivation
parseInstantiateError _   = UnknownInstantiateError


------------------------------------------------------------------------------
-- Realizing

data RealizeError = UnknownRealizeError deriving (Show, Eq)

-- | Finally derivations are realized into full store outputs.
-- This will typically take a while so it should be executed asynchronously.
--
-- Runs @nix-store@.
realize :: MonadIO m => [StorePath Derivation] -> NixAction RealizeError m [StorePath Realized]
realize (fmap unStorePath -> ds) =
     storeOp ( "-r" : fmap Text.pack ds )

-- | Copy the given files or folders to the nix store and return their paths.
--
-- Runs @nix-store@.
addToStore :: MonadIO m => [FilePath] -> NixAction RealizeError m [StorePath Realized]
addToStore fps = storeOp ( "--add" : fmap Text.pack fps )

storeOp :: (MonadIO m) => [Text] -> NixAction RealizeError m [StorePath Realized]
storeOp op = do
  exec <- Helpers.getExecOr exeNixInstantiate "nix-store"
  mapActionError (const UnknownRealizeError)
    $ evalNixOutput exec op
      >>= traverse (fromNixFilePath StorePath)

------------------------------------------------------------------------------
-- Building

data BuildError = UnknownBuildError
  deriving (Show, Eq)

buildPaths :: MonadIO m => [NixFilePath] -> NixAction BuildError m [StorePath Realized]
buildPaths fps = buildOp ( fmap (Text.pack . unNixFilePath) fps )

buildOp :: (MonadIO m) => [Text] -> NixAction BuildError m [StorePath Realized]
buildOp op = do
  exec <- Helpers.getExecOr exeNixBuild "nix-build"
  mapActionError (const UnknownBuildError)
    $ evalNixOutput exec op
      >>= traverse (fromNixFilePath StorePath)

------------------------------------------------------------------------------
-- nix-env commands

data EnvError = UnknownEnvError
  deriving (Show, Eq)

envOp :: (MonadIO m) => [Text] -> NixAction EnvError m [Text]
envOp op = do
  exec <- Helpers.getExecOr exeNixBuild "nix-build"
  mapActionError (const UnknownEnvError)
    $ evalNixOutput exec op

------------------------------------------------------------------------------
-- Convenience

-- | Combines all error types that could happen.
data NixError
  = ParseError ParseError
  | InstantiateError InstantiateError
  | RealizeError RealizeError
  | BuildError BuildError deriving (Show, Eq)

-- | A convenience function to directly realize a nix expression.
-- Any errors are put into a combined error type.
parseInstRealize :: (MonadIO m) => Text -> NixAction NixError m [StorePath Realized]
parseInstRealize = mapActionError ParseError . parseNixExpr
               >=> mapActionError InstantiateError . instantiateOne
               >=> mapActionError RealizeError . realize

-- | A convenience function to directly realize multiple nix expressions.
-- Any errors are put into a combined error type.
parseInstRealizeMany :: (MonadIO m) => [Text] -> NixAction NixError m [StorePath Realized]
parseInstRealizeMany = mapActionError ParseError . traverse parseNixExpr
                   >=> mapActionError InstantiateError . instantiate
                   >=> mapActionError RealizeError . realize

------------------------------------------------------------------------------
-- Helpers

-- | Take args and return either error message or last output path
evalNixOutputLast :: (MonadIO m)
                  => Helpers.Executable
                  -- ^ name of executable
                  -> [Text]
                  -- ^ arguments
                  -> NixAction Text m Text
                  -- ^ error: (stderr, errormsg), success: path
evalNixOutputLast = Helpers.readProcess $
  \inp ex -> processOutputLines inp ex >>= tryLast "nix didn’t output a store path"

-- | Take args and return either error message or output paths
evalNixOutput :: (MonadIO m)
              => Helpers.Executable
              -- ^ name of executable
              -> [Text]
              -- ^ arguments
              -> NixAction Text m [Text]
              -- ^ error: (stderr, errormsg), success: [path]
evalNixOutput = Helpers.readProcess processOutputLines

-- | Just a helper function needed by 'evalNixOutput' and 'evalNixOutputLast'.
processOutputLines :: MonadIO m => (Text, Text) -> ExitCode -> ExceptT Text m [Text]
processOutputLines (out, err) = \case
  ExitFailure _ -> throwE $
    case
      err
        & Text.lines
        & dropWhile (not . isPrefixOf "error: ")
        & List.intersperse "\n"
        & mconcat of
      "" -> "nix didn’t output any error message"
      s  -> s
  ExitSuccess -> pure $ Data.Text.lines out

-- | Apply filePath p to constructor a if it’s a valid filepath
fromNixFilePath :: Monad m => (String -> a) -> Text -> NixAction Text m a
fromNixFilePath a p =
  if FilePath.isValid (Text.unpack p) then pure $ a (Text.unpack p)
  else throwErrorWithoutStderr $ p <> " is not a filepath!"

-- | Check that a filepath is a real file (note that we don't check it's a valid nix file)
nixFilePath :: MonadIO m => Text -> NixAction Text m NixFilePath
nixFilePath p = do
  exists <- liftIO (Directory.doesFileExist ps)
  if exists then pure $ NixFilePath ps
  else throwErrorWithoutStderr $ p <> " is not a file!"
  where
    ps = Text.unpack p

throwErrorWithoutStderr :: Monad m => e -> NixAction e m a
throwErrorWithoutStderr e =
  throwError $ NixActionError
    { actionError = e
    , actionStderr = mempty
    }
