{-# OPTIONS_GHC  -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  AllRightsReserved
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | This script parses an server error log file for requested metadata and tries to build and upload
-- missing metadata files
-- 1. download error log from plesk (don't know uri to automate this)
-- 2. analyze error log  leksah-meta --error_log=error_log (maybe with -t50)
-- 3. leksah-meta --filter=error_log.processed
-- 3. backup user db
-- 4. leksah-meta --build=error_log.processed.filtered
-- 5. recover user db
-- example usage: leksah-meta -eerror_log -t50 -ferror_log.processed -berror_log.processed.filtered
-----------------------------------------------------------------------------

module Main (
    main

) where

import System.Console.GetOpt
       (ArgOrder(..), getOpt, usageInfo, ArgDescr(..), OptDescr(..))
import Control.Monad (liftM, filterM, when)
import Data.Version (showVersion)
import System.Environment (getArgs)
import Paths_leksah_meta(version)
import Data.Maybe (isJust, catMaybes)
import qualified Data.Map as Map
       (toList, empty, update, insert, lookup)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Text.ParserCombinators.Parsec
       (getInput, optional, option, try, (<|>), oneOf, alphaNum, manyTill,
        eof, anyChar, sepBy, (<?>), noneOf, many, char, CharParser)
import Data.List (sortBy, isSuffixOf, isInfixOf)
import qualified Text.ParserCombinators.Parsec.Token as P
       (whiteSpace, makeTokenParser,symbol)
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import GHC.IO (liftIO)
import IDE.Utils.FileUtils
       (getCollectorPath, getConfigFilePathForLoad)
import IDE.Utils.Utils
       (leksahMetadataPathFileExtension,
        leksahMetadataSystemFileExtension, strippedPreferencesFilename)
import IDE.StrippedPrefs (Prefs(..), readStrippedPrefs)
import IDE.Utils.Tool (runTool')
import IDE.Utils.GHCUtils (getInstalledPackageInfos, inGhcIO)
import IDE.Core.CTypes
       (mdMbSourcePath, ModuleDescr(..), PackageDescr(..),
        metadataVersion, packageIdentifierToString, RetrieveStrategy(..))
import System.Directory (doesFileExist, setCurrentDirectory)
import System.Process (system)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Package (PackageIdentifier(..))
import Paths_leksah_meta (getDataDir)
import PackageConfig (PackageConfig)
import IDE.Metainfo.PackageCollector(collectPackage)
import Control.Exception (throw, catch, SomeException)
import Prelude hiding (catch)
import System.FilePath ((</>))
import GHC.IO.Exception (ExitCode(..))
import System.IO (hClose, openBinaryFile)
import GHC.IO.IOMode (IOMode(..))
import Data.Binary.Shared (decodeSer)
import Control.DeepSeq (deepseq)
import qualified Data.ByteString.Lazy as BSL
import IDE.Utils.FileUtils(allFilesWithExtensions)

leksahVersion = "0.9"

data Flag =    ErrorLog String
             | FilterLog String
             | BuildLog String
             | Threshold (Maybe String)
             | TestSource FilePath
             | TestSourceDir FilePath
             | VersionF
             | Help
       deriving (Show,Eq)



options =   [
-- main functions
             Option ['e'] ["errorlog"] (ReqArg ErrorLog "ErrorLog")
                "Process an error log and outputs a file with wanted package"
         ,   Option ['t'] ["treshold"] (OptArg Threshold "Threshold")
                "Filters packages which have less accesses then the given integer"
         ,   Option ['f'] ["filter"] (ReqArg FilterLog "FilterLog")
                "Removes all packages, for which metadata packages already exists"
         ,   Option ['b'] ["build"] (ReqArg BuildLog "BuildLog")
               "Process a package file and uploads packages"
         ,   Option ['s'] ["testsource"] (ReqArg TestSource "TestSource")
               "Tests if a '*.lkshm' has source locations"
         ,   Option ['d'] ["testsourcedir"] (ReqArg TestSourceDir "TestSourceDir")
               "Tests if all '*.lkshm' files in a directory have source locations"
         ,   Option ['v'] ["version"] (NoArg VersionF)
                "Show the version number"
         ,   Option ['h'] ["help"] (NoArg Help)
                "Display command line options"
    ]

processOpts :: [String] -> IO ([Flag], [String])
processOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options

header = "Usage: leksah-meta [OPTION...]"

getThisPackage :: PackageConfig -> PackageIdentifier
#if MIN_VERSION_Cabal(1,8,0)
getThisPackage    =   IPI.sourcePackageId
#else
getThisPackage    =   IPI.package
#endif

main = do
    args            <-  getArgs
    (o,_)           <-  processOpts args
    when (elem VersionF o)
        (putStrLn $ "Leksah the Haskell IDE, admin tools, version " ++ showVersion version)
    when (elem Help o)
        (putStrLn $ "Leksah the Haskell IDE, admin tools, " ++ usageInfo header options)
    dataDir         <- getDataDir
    prefsPath       <- getConfigFilePathForLoad strippedPreferencesFilename Nothing dataDir
    prefs           <- readStrippedPrefs prefsPath

    let thresholdL    =  catMaybes $
                            map (\x -> case x of
                                Threshold (Just s)   -> Just (read s)
                                _             -> Nothing) o
    let threshold     =  case thresholdL of
                            [] -> Nothing
                            (a:_) -> Just a

    let errorLog'     =  catMaybes $
                            map (\x -> case x of
                                ErrorLog s   -> Just s
                                _            -> Nothing) o
    case errorLog' of
        []    -> return ()
        (h:_) -> processErrorLog h prefs threshold

    let filterLog'    =  catMaybes $
                            map (\x -> case x of
                                FilterLog s   -> Just s
                                _            -> Nothing) o
    case filterLog' of
        []    -> return ()
        (h:_) -> processFilterLog h prefs

    let buildLog'     =  catMaybes $
                            map (\x -> case x of
                                BuildLog s   -> Just s
                                _            -> Nothing) o
    case buildLog' of
        []    -> return ()
        (h:_) -> processBuildLog h prefs

    let testSource'     =  catMaybes $
                            map (\x -> case x of
                                TestSource s   -> Just s
                                _            -> Nothing) o
    case testSource' of
        []    -> return ()
        (h:_) -> processTestSource prefs h

    let testSourceDir'     =  catMaybes $
                            map (\x -> case x of
                                TestSourceDir s   -> Just s
                                _            -> Nothing) o
    case testSourceDir' of
        []    -> return ()
        (h:_) -> processTestSourceDir prefs h



processTestSourceDir :: Prefs -> FilePath -> IO ()
processTestSourceDir prefs dirPath = do
    files <- allFilesWithExtensions [".lkshm"] True [] dirPath
    mapM_ (processTestSource prefs) files

processTestSource :: Prefs -> FilePath ->   IO ()
processTestSource prefs  filePath = do
    file            <-  openBinaryFile filePath ReadMode
    bs              <-  BSL.hGetContents file
    let (metadataVersion', (packageInfo :: PackageDescr)) =   decodeSer bs
    if metadataVersion /= metadataVersion'
        then do
            hClose file
            error ("Metadata has a wrong version."
                    ++  " Consider rebuilding metadata with: leksah-server -osb +RTS -N2 -RTS")
        else do
            packageInfo `deepseq` (hClose file)
            let percentage = percentageSourceInPack packageInfo
            putStrLn $ filePath ++ "percentage of module sources = " ++ show (percentage * 100.0) ++ " %"

percentageSourceInPack :: PackageDescr -> Double
percentageSourceInPack pd =
    fromIntegral (sum (map ((\b -> if b then 1 else 0) . hasSourceInMod) (pdModules pd)))
                                / fromIntegral (length (pdModules pd))

hasSourceInMod :: ModuleDescr -> Bool
hasSourceInMod = isJust . mdMbSourcePath

processErrorLog :: FilePath -> Prefs -> Maybe Int -> IO ()
processErrorLog filePath _ threshold = do
    cont <- readFile filePath
    let packageNames = catMaybes $ map parse (lines cont)
    let resMap = foldr (\ str amap -> case Map.lookup str amap of
                                        Nothing -> Map.insert str 1 amap
                                        Just i -> Map.insert str (i + 1) amap) Map.empty packageNames
    let list = Map.toList resMap
    let versionList = filter (\(s1,_) -> isInfixOf ("metadata-" ++ leksahVersion) s1) list
    let shorterList = case threshold of
                        Nothing -> list
                        Just t -> filter (\(_,n1) -> n1 >= t) versionList
    writeFile (filePath ++ ".processed") (show (sortBy (\(_,n1) (_,n2) -> compare n2 n1) shorterList))
    where
    parse :: String -> Maybe String
    parse str = case Parsec.parse lineParser "" str of
                        Left e   -> Nothing
                        Right r  -> Just r

processFilterLog :: FilePath -> Prefs -> IO ()
processFilterLog fp prefs = do
    str <- readFile fp
    list :: [(String,Int)] <- readIO str
    filtered <- filterM (filterPackage prefs) list
    writeFile (fp ++ ".filtered") (show filtered)

filterPackage :: Prefs -> (String,Int) -> IO Bool
filterPackage prefs (packageId,_) = do
    let fullUrl = "http://www.leksah.org:" ++ "/metadata-" ++ leksahVersion ++ "/" ++ packageId ++ leksahMetadataSystemFileExtension
    exit <- catch (system $ "wget " ++ fullUrl ++ " --spider -S")
                (\(e :: SomeException) -> return (ExitFailure 1))
    return (exit /=  ExitSuccess)

processBuildLog :: FilePath -> Prefs -> IO ()
processBuildLog fp prefs = do
    putStrLn "enter password"
    pwd <- getLine
    str <- readFile fp
    list :: [(String,Int)] <- readIO str
    failed <- filterM (processPackage prefs pwd) list
    writeFile (fp ++ ".failed") (show failed)

processPackage :: Prefs -> String -> (String,Int) -> IO Bool
processPackage  prefs pwd (packageId,_) = do
    putStrLn $ "Processing package " ++ packageId
    catch (do
        (out,pid) <- runTool' "cabal" (["install","--user", packageId]) Nothing
        putStrLn (show out)
        packageInfos        <-  inGhcIO [] [] $  \ _ -> getInstalledPackageInfos
        let packageInfoList  = filter (\ pid -> packageId == packageIdentifierToString (getThisPackage pid)) packageInfos
        case packageInfoList of
            []      -> do
                putStrLn $ "Failed to install package. Cabal can't install " ++ packageId
                return True
            hd : [] -> do
                collectPackage False prefs{retrieveStrategy = NeverRetrieve} 1 (hd, 1)
                collectorPath   <- getCollectorPath
                setCurrentDirectory collectorPath
                let filePath  = packageId ++ leksahMetadataSystemFileExtension
                let filePath2 = packageId ++ leksahMetadataPathFileExtension
                exists1 <- doesFileExist filePath
                exists2 <- doesFileExist filePath2
                if exists1 && exists2
                    then do
                    -- wput example.lkshm ftp://leksah:pwd@www.leksah.org:21/httpdocs/metadata-0.8/
                        let wputarg = filePath ++ " " ++ "ftp://leksah:" ++ pwd ++ "@www.leksah.org:21/httpdocs/metadata-" ++ leksahVersion ++ "/"
                        system $ "wput " ++ wputarg
                        putStrLn $ "Success with uploading metadata for package " ++ packageId
                        return False
                    else do
                        putStrLn $ "Failed to install package. Haddock can't build " ++ packageId
                        return True
            _       -> do
                    putStrLn $ "Failed to install package. Cabal can't install " ++ packageId
                    return True)
        (\(e :: SomeException) -> do
                                putStrLn $ "Process package error " ++ packageId ++ " " ++ show e
                                return True)

-- [Sun May 09 19:39:57 2010] [error] [client 98.195.104.51] File does not exist: /srv/www/vhosts/leksah.org/httpdocs/metadata-0.8/regex-tdfa-1.1.2.lkshm
lineParser :: CharParser () String
lineParser = do
    char '['
    many (noneOf "]")
    char ']'
    whiteSpace
    char '['
    many (noneOf "]")
    char ']'
    whiteSpace
    char '['
    many (noneOf "]")
    char ']'
    whiteSpace
    symbol "File does not exist:"
    whiteSpace
    urlParser
    <?> "lineParser"

urlParser :: CharParser () String
urlParser = do
    str <- manyTill anyChar eof
    if isSuffixOf ".lkshm" str
        then return (reverse (drop 6 (takeWhile (/= '/') (reverse str))))
        else fail "no .lkshm"
    <?> "urlParser"

lexer      = P.makeTokenParser haskellStyle
whiteSpace = P.whiteSpace lexer
symbol     = P.symbol lexer

