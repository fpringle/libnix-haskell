{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Foreign.Nix.Shellout.Helpers where

import Foreign.Nix.Shellout.Types ( NixActionError(..), RunOptions (..), LogFn (LogFn), NixAction, Executables )
import qualified System.Process as P
import qualified System.IO as SIO

import GHC.IO.Exception (ExitCode)
import Data.Text (Text)
import Control.Error (ExceptT, runExceptT)
import Control.DeepSeq (rnf)

import Control.Exception (evaluate)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as Text
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Data.Function ((&))

import qualified Conduit as C
import qualified Data.ByteString as BS
import qualified Data.Conduit.Process as C
import qualified Data.Text.Encoding as TE

-- | Something we can run
data Executable =
  ExeFromPathEnv Text
  -- ^ name of the executable, to be looked up in PATH
  | ExeFromFilePath FilePath
  -- ^ a file path to the executable (can be relative or absolute)
  deriving Show

-- | Get an executable from the 'Executables' option (by its getter)
-- or if not set use the given 'Text' as the name of the excutable
-- to be looked up in @PATH@.
getExecOr :: Monad m => (Executables -> Maybe FilePath) -> Text ->  NixAction e m Executable
getExecOr getter exeName =
  let f = \case
        Nothing -> ExeFromPathEnv exeName
        Just fp -> ExeFromFilePath fp
  in asks (f . getter . executables)

-- | Read the output of a process into a NixAction.
-- | Keeps stderr if process returns a failure exit code.
-- | The text is decoded as @UTF-8@.
readProcess :: (MonadIO m)
            => ((Text, Text) -> ExitCode -> ExceptT e m a)
            -- ^ handle (stdout, stderr) depending on the return value
            -> Executable
            -- ^ executable to run
            -> [Text]
            -- ^ arguments
            -> NixAction e m a
readProcess with exec args = do
  let exec' = case exec of
        ExeFromPathEnv name -> name
        ExeFromFilePath fp -> fp & Text.pack
  -- log every call based on the LogFn the user passed
  (LogFn l) <- asks logFn
  lift $ l exec' args

  debug <- asks printStdErr

  (exc, out, err) <- liftIO
    $ readCreateProcessWithExitCodeAndEncoding
        (P.proc (Text.unpack exec') (map Text.unpack args)) debug SIO.utf8 ""
  lift (runExceptT (with (out, err) exc)) >>= \case
    Left e ->
      throwError $ NixActionError
        { actionStderr = err
        , actionError = e }
    Right a -> pure a

-- Copied & modified from System.Process (process-1.6.4.0)

-- | like @readCreateProcessWithExitCodeAndEncoding, but uses
-- | Text instead of [Char] and lets the user specify an encoding
-- | for the handles.
readCreateProcessWithExitCodeAndEncoding
    :: P.CreateProcess
    -> Bool                        -- ^ print stderr?
    -> SIO.TextEncoding            -- ^ encoding for handles
    -> Text                        -- ^ standard input
    -> IO (ExitCode, Text, Text)   -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCodeAndEncoding cp debug encoding input = do
    let cp_opts = cp
          { P.std_in  = P.CreatePipe
          , P.std_out = P.CreatePipe
          , P.std_err = P.CreatePipe }

    -- TODO: encoding

    let stdInC  :: C.ConduitT () BS.ByteString IO () = C.yield input C..| C.encodeUtf8C
        stdOutC :: C.ConduitT BS.ByteString C.Void IO BS.ByteString = BS.toStrict <$> C.sinkLazy
        stdErrC :: C.ConduitT BS.ByteString C.Void IO BS.ByteString = go ""
          where
            go acc = do
              mChunk <- C.await
              case mChunk of
                Nothing -> pure acc
                Just chunk -> do
                  when debug . liftIO $ BS.hPutStr SIO.stderr chunk
                  go (acc <> chunk)

    (ex, TE.decodeUtf8 -> out, TE.decodeUtf8 -> err) <- C.sourceProcessWithStreams cp_opts stdInC stdOutC stdErrC
    evaluate $ rnf out
    evaluate $ rnf err
    pure (ex, out, err)
