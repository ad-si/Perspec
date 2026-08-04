module ConfigLoader (loadConfig) where

import Protolude (
  Bool (True),
  Either (Left, Right),
  IO,
  die,
  pure,
  writeFile,
  ($),
 )

import Data.Text qualified as T
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Directory (
  XdgDirectory (XdgConfig),
  createDirectoryIfMissing,
  getXdgDirectory,
 )
import System.FilePath ((</>))

import Types (Config)


-- | Load the app config, creating a default config file on first run
loadConfig :: IO Config
loadConfig = do
  let appName = "Perspec"

  configDirectory <- getXdgDirectory XdgConfig appName
  createDirectoryIfMissing True configDirectory

  let configPath = configDirectory </> "config.yaml"

  configResult <- decodeFileEither configPath

  case configResult of
    Left error -> do
      if "file not found"
        `T.isInfixOf` T.pack (prettyPrintParseException error)
        then do
          writeFile configPath "licenseKey:\n"
          configResult2 <- decodeFileEither configPath

          case configResult2 of
            Left error2 -> die $ T.pack $ prettyPrintParseException error2
            Right config -> pure config
        else die $ T.pack $ prettyPrintParseException error
    Right config -> pure config
