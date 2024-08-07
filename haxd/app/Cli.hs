{-# LANGUAGE BlockArguments #-}

module Cli
  ( CliArgs(..)
  , DumpArgs(..)
  , LoadArgs(..)
  , EditArgs(..)
  , parseCommandLine
  ) where

import Options.Applicative
import Data.Int ( Int64 )
import Text.Read ( readMaybe )

data CliArgs
  = Dump DumpArgs
  | Load LoadArgs
  | Edit EditArgs
  deriving Show

data DumpArgs = DumpArgs
  { dumpNumColumns :: Int64
  , dumpGroupSize :: Int64
  , dumpLength :: Maybe Int64
  , dumpOffset :: Int64
  , dumpInputFile :: Maybe String
  , dumpOutputFile :: Maybe String
  }
  deriving Show

data LoadArgs = LoadArgs
  { loadOffset :: Int64
  , loadPatchMode :: Bool
  , loadInputFile :: Maybe String
  , loadOutputFile :: Maybe String
  }
  deriving Show

data EditArgs = EditArgs
  { editNumColumns :: Int64
  , editPatchMode :: Bool
  , editGroupSize :: Int64
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
          (progDesc "Dump a file into xxd-like format."))
      <> command "load"
        (info (Load <$> loadArgs <**> helper)
          (progDesc "Parse the output from `dump` and generate the original binary."))
      <> command "edit"
        (info (Edit <$> editArgs <**> helper)
          (progDesc "Open an editor to edit the binary in xxd-like format. The original file will be replaced if no error occurs."))
      )

dumpArgs :: Parser DumpArgs
dumpArgs
  = DumpArgs
    <$> option positiveInt64
      ( short 'c'
      <> long "num-columns"
      <> value 16
      <> help "Number of bytes shown on each line, defaults to 16."
      <> metavar "NUM_COLUMNS"
      )
    <*> option positiveInt64
      ( short 'g'
      <> long "group-size"
      <> value 2
      <> help "Bytes per group (separated by a single space), defaults to 2."
      <> metavar "GROUPSIZE"
      )
    <*> optional
      (option positiveInt64
        ( short 'l'
        <> long "length"
        <> help "Stop after writing LENGTH bytes."
        <> metavar "LENGTH"
        ))
    <*> option auto
      ( short 'o'
      <> long "offset"
      <> value 0
      <> help "Add OFFSET to the displayed file position."
      <> metavar "OFFSET"
      )
    <*> optional
      (argument str
        ( help "Input file, defaults to stdin."
        <> metavar "INPUT"
        ))
    <*> optional
      (argument str
        ( help "Output file, defaults to stdout."
        <> metavar "OUTPUT"
        ))

loadArgs :: Parser LoadArgs
loadArgs
  = LoadArgs
    <$> option auto
      ( short 'o'
      <> long "offset"
      <> value 0
      <> help "Add OFFSET to file positions in the hex dump."
      <> metavar "OFFSET"
      )
    <*> switch
      ( short 'p'
      <> long "patch-mode"
      <> help
        ( "Enable patch mode."
        <> " Under patch mode, the line numbers at the start of each hex dump line may be out of order, lines may be missing, or overlapping."
        <> " Otherwise only gaps are allowed, which will be filled by null-bytes."
        )
      )
    <*> optional
      (argument str
        ( help
          ( "Input file, defaults to stdin."
          <> " Note that everything after hexadecimal data is ignored."
          <> " This also means that changes to the printable ASCII columns are always ignored."
          )
        <> metavar "INPUT"
        ))
    <*> optional
      (argument str
        ( help "Output file, defaults to stdout."
        <> metavar "OUTPUT"
        ))

editArgs :: Parser EditArgs
editArgs
  = EditArgs
    <$> option positiveInt64
      ( short 'c'
      <> long "num-colomns"
      <> value 16
      <> help "Number of bytes shown on each line, defaults to 16."
      <> metavar "NUM_COLUMNS"
      )
    <*> switch
      ( short 'p'
      <> long "patch-mode"
      <> help "Enable patch mode. See `haxl load --help`."
      )
    <*> option positiveInt64
      ( short 'g'
      <> long "group-size"
      <> value 2
      <> help "Bytes per group (separated by a single space), defaults to 2."
      <> metavar "GROUPSIZE"
      )
    <*> argument str
      ( help "The file to edit."
      <> metavar "FILE")

positiveInt64 :: ReadM Int64
positiveInt64 = maybeReader \s -> do
  (w :: Int64) <- readMaybe s
  if w <= 0
    then Nothing
    else pure . fromIntegral $ w
