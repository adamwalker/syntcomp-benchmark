{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Main where

import Data.Yaml
import Data.Aeson.TH hiding (Options)
import qualified Data.ByteString.Char8 as BS
import System.IO
import System.CPUTime
import Control.Monad
import System.Directory
import System.Process
import Data.Maybe
import Data.List
import Control.Concurrent
import Data.List.Utils
import Options.Applicative as O
import Text.PrettyPrint.Boxes hiding ((<>))
import System.Posix.Process
import System.Exit
import Data.IORef
import Data.Time.Clock

filename = reverse . takeWhile (/= '/') . reverse

data Solver = Solver {
    name    :: String,
    command :: String,
    args    :: String
} deriving Show

deriveJSON defaultOptions ''Solver

data Config = Config {
    benchmarks :: FilePath,
    timeout    :: Int,
    solvers    :: [Solver]
} deriving Show

deriveJSON defaultOptions ''Config

data Result 
    = SuccessR Double
    | FailureR
    | TimeoutR
    | ErrorR

r2s :: Result -> String
r2s (SuccessR time) = show time
r2s FailureR        = "result incorrect"
r2s ErrorR          = "error"
r2s TimeoutR        = "timeout"

correct :: FilePath -> String -> ExitCode -> Double -> Bool -> Result
correct _ _ _ _ True = TimeoutR
correct path output ExitSuccess time timedOut
    | (take 10 output == "REALIZABLE" && realizable) || (take 12 output == "UNREALIZABLE" && not realizable) = SuccessR time
    | take 10 output == "REALIZABLE" || take 12 output == "UNREALIZABLE"                                     = FailureR
    | otherwise                                                                                              = ErrorR
    where
    realizable = not $ isInfixOf "unr" (filename path)
correct path output (ExitFailure _) time timedOut = ErrorR

runSolver :: [FilePath] -> Int -> Solver -> IO [Result]
runSolver files timeout Solver{..} = do
    putStrLn $ "running: " ++ name
    forM files $ \file -> do
        putStr $ filename file ++ "..."
        hFlush stdout
        let cmdLine = replace "%i" file args
            process = CreateProcess
                (ShellCommand $ "(ulimit -v 3000000;" ++ command ++ " " ++ cmdLine ++ ")")
                Nothing
                Nothing
                Inherit
                CreatePipe
                Inherit
                False
                False
                False
        ref <- newIORef False
        --start <- getProcessTimes
        start <- getCurrentTime 
        (_, Just stdout, _, handle) <- createProcess process

        pid <- forkIO $ do
            threadDelay $ timeout * 10^6
            terminateProcess handle
            writeIORef ref True

        exitCode <- waitForProcess handle
        killThread pid

        cts <- hGetContents stdout
        cts `seq` hClose stdout

        --end <- getProcessTimes
        end <- getCurrentTime
        --let elapsed = fromIntegral (fromEnum $ childUserTime end - childUserTime start) / 100
        let elapsed = fromRational $ toRational $ diffUTCTime end start 
        timedOut <- readIORef ref
        let res = correct file cts exitCode elapsed timedOut
        putStrLn $ r2s res
        return res

runAll :: Config -> IO String
runAll Config{..} = do
    files <- getDirectoryContents benchmarks
    let files' = map ((benchmarks ++ "/") ++) files
    files <- filterM doesFileExist files'
    times <- forM solvers (runSolver (sort files) timeout)

    let res = pp (map name solvers) (map filename (sort files)) times
    putStrLn ""
    putStrLn res
    return res

data Options = Options {
    configFile :: FilePath,
    outputFile :: Maybe FilePath
}

run :: Options -> IO ()
run Options{..} = do
    yaml <- BS.readFile configFile
    let config = decode yaml :: Maybe Config
    res <- runAll $ fromJust config
    maybe (return ()) (flip writeFile res) outputFile 

pp :: [String] -> [String] -> [[Result]] -> String
pp solvers benchmarks times = render $ hsep 1 left $ bms : zipWith results solvers times
    where
    bms                  = vcat left $ emptyBox 1 1 : map text benchmarks
    results solver times = vcat left $ text solver  : map (text . r2s) times

main = execParser opts >>= run 
    where
    opts   = info (helper <*> parser) (fullDesc <> progDesc "progDesc" <> O.header "header")
    parser = Options <$> argument O.str (metavar "CONFIG") <*> optional (O.strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file"))

