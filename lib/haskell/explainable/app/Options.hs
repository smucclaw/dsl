module Options (
  Options (..),
  optionsParser,
) where

import Options.Applicative

data Options = Options
  { port :: Int
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
