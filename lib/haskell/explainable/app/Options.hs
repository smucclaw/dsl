module Options (
  Options (..),
  optionsParser,
) where

import Options.Applicative
import qualified Data.Text as Text

data Options = Options
  { port :: Int
  , serverName :: Maybe Text.Text
  }

optionsParser :: Parser Options
optionsParser = do
  Options
    <$> ( option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value 8081
                <> help "HTTP port to use"
            )

        )
    <*> optional
      ( strOption
        ( long "serverName"
        <> short 's'
        <> metavar "URL"
        <> help "Name of the server. Exposed in swagger.json field."
        )
      )
