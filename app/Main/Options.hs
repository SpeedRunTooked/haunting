module Main.Options
  ( Options(..)
  , Dump(..)
  , InputFile(..)
  , OutputFile(..)
  , getOpts
  ) where

import           Data.Maybe (fromMaybe)
import           Options.Applicative

getOpts :: IO Options
getOpts = execParser optP
 where
  optP = info (helper <*> opts)
    (  fullDesc
    <> progDesc
        "Utilities for reading and manipulating Mario Kart 64 Ghost Data"
    <> header "haunt: Liberating Ghosts" )

data Options = Opt
  { mode   :: Dump
  , input  :: InputFile
  , output :: OutputFile
  }

opts :: Parser Options
opts = Opt <$> pDump <*> pInput <*> pOutput

data Dump = Meta | DumpGhost Int | DumpCSV Int | DumpAll

pDumpGhost :: Parser Dump
pDumpGhost =
  DumpGhost <$> option auto
                ( long "dump-ghost" 
                <> short 'd'
                <> metavar "SLOT-INDEX"
                <> help "dump the ghost corresponding to the specified slot" )

pDumpCSV :: Parser Dump
pDumpCSV =
  DumpCSV <$> option auto
               ( long "dump-csv" 
               <> short 'c'
               <> metavar "SLOT-INDEX"
               <> help "dump the frame-by-frame data for the specified ghost" )

pDumpAll :: Parser Dump
pDumpAll = flag' DumpAll
  ( long "exorcise"
  <> short 'e'
  <> help "write all ghosts out to separate files (filename is based on input file name)" )

pDump :: Parser Dump
pDump = fromMaybe Meta <$> optional (pDumpGhost <|> pDumpAll <|> pDumpCSV)

data InputFile = Stdin | FPath FilePath

fileInput :: Parser InputFile
fileInput = FPath <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "path to pak file" )

stdInput :: Parser InputFile
stdInput = flag' Stdin
  (  long "stdin"
  <> help "Read from stdin" )

pInput :: Parser InputFile
pInput = fromMaybe Stdin <$> optional (fileInput <|> stdInput)

data OutputFile = Stdout | OFPath FilePath

fileOutput :: Parser OutputFile
fileOutput = OFPath <$> strOption
  (  long "out"
  <> short 'o'
  <> metavar "FILENAME"
  <> help "output ghost JSON to a file" )

stdOutput :: Parser OutputFile
stdOutput = flag' Stdout
  (  long "stdout"
  <> help "output ghost JSON to stdout" )

pOutput :: Parser OutputFile
pOutput = fromMaybe Stdout <$> optional (fileOutput <|> stdOutput)
