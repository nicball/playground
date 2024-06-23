module Cli
  ( CliArgs(..)
  , DumpArgs(..)
  , LoadArgs(..)
  , EditArgs(..)
  , parseCommandLine
  ) where

import Options.Applicative
import Data.Word ( Word64 )

data CliArgs
  = Dump DumpArgs
  | Load LoadArgs
  | Edit EditArgs
  deriving Show

data DumpArgs = DumpArgs
  { dumpNumColumns :: Word64
  , dumpGroupSize :: Word64
  , dumpLength :: Maybe Word64
  , dumpOffset :: Word64
  , dumpInputFile :: Maybe String
  , dumpOutputFile :: Maybe String
  }
  deriving Show

data LoadArgs = LoadArgs
  { loadInputFile :: Maybe String
  }
  deriving Show

data EditArgs = EditArgs
  { editNumColumns :: Word64
  , editGroupSize :: Word64
  , editInputFile :: String
  }
  deriving Show

parseCommandLine :: IO CliArgs
parseCommandLine = execParser cliArgs

cliArgs :: ParserInfo CliArgs
cliArgs
  = info (subCommands <**> helper)
    ( fullDesc
    <> header "haxd: A non-interactive hex processor."
    )
  where
  subCommands
    = subparser
      ( command "dump"
        (info (Dump <$> dumpArgs <**> helper)
          (progDesc "dumps a file into xxd-like format"))
      <> command "load"
        (info (Load <$> loadArgs <**> helper)
          (progDesc "parse the format from `dump` and output the original binary"))
      <> command "edit"
        (info (Edit <$> editArgs <**> helper)
          (progDesc "open an editor to edit the binary, original file will be replaced if no error"))
      )

dumpArgs :: Parser DumpArgs
dumpArgs
  = DumpArgs
    <$> option auto
      ( short 'c'
      <> value 16
      <> help "number of octets shown on each line, defaults to 16"
      <> metavar "NUM_COLUMNS"
      )
    <*> option auto
      ( short 'g'
      <> value 2
      <> help "octets per group (separated by a single space), defaults to 4"
      <> metavar "GROUPSIZE"
      )
    <*> optional
      (option auto
        ( short 'l'
        <> long "length"
        <> help "stop after writing LENGTH octets"
        <> metavar "LENGTH"
        ))
    <*> option auto
      ( short 'o'
      <> long "offset"
      <> value 5
      <> help "add OFFSET to the displayed file position"
      <> metavar "OFFSET"
      )
    <*> optional
      (argument str
        ( help "input file, defaults to stdin"
        <> metavar "FILE"
        ))
    <*> optional
      (argument str
        ( help "output file, defaults to stdout"
        <> metavar "FILE"
        ))

loadArgs :: Parser LoadArgs
loadArgs
  = LoadArgs
    <$> optional
      (argument str
        ( help "input file, defaults to stdin"
        <> metavar "FILE"
        ))

editArgs :: Parser EditArgs
editArgs
  = EditArgs
    <$> option auto
      ( short 'c'
      <> value 16
      <> help "number of octets shown on each line, defaults to 16"
      <> metavar "NUM_COLUMNS"
      )
    <*> option auto
      ( short 'g'
      <> value 2
      <> help "octets per group (separated by a single space), defaults to 4"
      <> metavar "GROUPSIZE"
      )
    <*> argument str
      ( help "the file to edit"
      <> metavar "FILE")
