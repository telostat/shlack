module Main where

import           Data.Maybe          (listToMaybe)
import qualified Data.Text           as T
import           Data.Version        (showVersion)
import           Data.Yaml           (decodeFileEither)
import qualified Data.Yaml           as YAML
import           Network.Shlack      (Config(..), Message(..), Profile(..), shlack)
import qualified Options.Applicative as OA
import           Paths_shlack        (version)
import           System.Directory    (doesFileExist, getHomeDirectory, makeAbsolute)
import           System.Exit         (ExitCode(..), die, exitWith)
import           System.FilePath     ((</>))
import           System.IO           (hPutStrLn, stderr)


-- | Main program entry point.
main :: IO ()
main = exitWith =<< (cliProgram =<< OA.execParser cliProgramParserInfo)


-- | CLI program.
cliProgram :: CliArguments -> IO ExitCode
cliProgram (CliArguments (CommandSend (s, n, ps, es, ds))) = send s n ps es ds


-- | Sends message.
send :: Maybe FilePath -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> IO ExitCode
send mc mp mh mb mf = do
  config <- parseConfigFile =<< getConfigFile
  let profiles = configProfiles config
  case listToMaybe $ maybe profiles (\p -> filter ((p ==) . profileName) profiles) mp of
    Nothing -> die "No suitable profile found."
    Just p  -> either die (const $ pure ExitSuccess) =<< shlack p (Message mh mb mf)
  where
    parseConfigFile cf = either (die . (<>) "Configuration parse error: " . show) pure =<< YAML.decodeFileEither cf

    getConfigFile = case mc of
      Nothing -> do
        home <- getHomeDirectory
        tryConfigFiles [".shlack.json", ".shlack.yaml", home </> ".shlack.json", home </> ".shlack.yaml", "/etc/shlack.json", "/etc/shlack.yaml"]
      Just cf -> either die pure =<< ensureFile cf

    tryConfigFiles []       = die "No configuration file found"
    tryConfigFiles (x : xs) = either (const $ tryConfigFiles xs) pure =<< ensureFile x


-- | Attempts to ensure that the given file path is transformed into an absolute
-- file path and it exists.
ensureFile :: FilePath -> IO (Either String FilePath)
ensureFile fp = do
  sap <- makeAbsolute fp
  exists <- doesFileExist sap
  pure $ if exists
    then Right sap
    else Left $ "File does not exist: " <> fp


-- | CLI arguments parser.
parserProgramOptions :: OA.Parser CliArguments
parserProgramOptions = CliArguments <$> OA.hsubparser
  ( OA.command "send" (OA.info (CommandSend <$> optsSend) (OA.progDesc "Sends Slack message"))
  )


-- | @send@ command arguments parser.
optsSend :: OA.Parser (Maybe FilePath, Maybe T.Text, Maybe T.Text, Maybe T.Text, Maybe T.Text)
optsSend = (,,,,)
  <$> OA.optional (OA.strOption (OA.long "config" <> OA.short 'c' <> OA.metavar "CONFIG" <> OA.help "Shlack configuration file (YAML or JSON)"))
  <*> OA.optional (OA.strOption (OA.long "profile" <> OA.short 'p' <> OA.metavar "PROFILE" <> OA.help "Profile name to use, defaults to first profile in config"))
  <*> OA.optional (OA.strOption (OA.long "header" <> OA.short 'h' <> OA.metavar "HEADER" <> OA.help "Header text"))
  <*> OA.optional (OA.strOption (OA.long "body" <> OA.short 'b' <> OA.metavar "BODY" <> OA.help "Body text"))
  <*> OA.optional (OA.strOption (OA.long "footer" <> OA.short 'f' <> OA.metavar "FOOTER" <> OA.help "Footer text"))


-- | Registry of commands.
newtype Command = CommandSend (Maybe FilePath, Maybe T.Text, Maybe T.Text, Maybe T.Text, Maybe T.Text)
  deriving Show


-- | Parsed command line arguments.
newtype CliArguments = CliArguments { cliArgumentsCommand :: Command } deriving Show


-- | CLI program information.
cliProgramParserInfo :: OA.ParserInfo CliArguments
cliProgramParserInfo = OA.info
  (OA.helper <*> parserVersionOption <*> parserProgramOptions)
  (OA.fullDesc <> OA.progDesc "shlack" <> OA.header "shlack - Send Slack messages from shell, quickly")


-- | Version option.
parserVersionOption :: OA.Parser (a -> a)
parserVersionOption = OA.infoOption (showVersion version) (OA.long "version" <> OA.help "Show version")
