module Main where

import Codec.JsonTape (Object, decodeFiles, encodeFiles)
import Control.Monad (unless, void, when)
import Data.ByteString.Base64.Type
       (ByteString64(ByteString64), readFile, writeFile, getByteString64)
import Data.Monoid ((<>))
import Options
import Prelude hiding (readFile, writeFile)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Directory.Tree
       (AnchoredDirTree(..), DirTree(..), readDirectoryWith, writeDirectoryWith)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
       ((</>), (<.>), stripExtension, takeDirectory, takeExtension)
import System.IO (hPutStrLn, stderr)

-- | Entry point of the utility
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Right (opts, nonopts) ->
            case opts of
                Options {optMode = Help} -> putStrLn usage
                Options {optMode = Version} -> putStrLn versionInfo
                _ ->
                    if null nonopts
                        then putStrLn usage
                        else mapM_ (processTree opts) nonopts
        Left errs -> hPutStrLn stderr errs >> exitFailure

-- | Choose whether to unpack the tree if the node is a file,
-- or pack it when it is a directory.
processTree :: Options -> FilePath -> IO ()
processTree opts path = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    if isDir
        then packTree opts path
        else if isFile
                 then unpackTree opts path
                 else fail (path <> " is not a file or a directory.")

-- | Pack given directory into a file.
packTree :: Options -> FilePath -> IO ()
packTree opts path = do
    tree <- readDirectoryWith (fmap ByteString64 . readFile) path
    let destination = (path <.> "json-tape")
    exists <- doesFileExist destination
    when
        (exists && not (optForce opts))
        (fail $ "File " <> destination <> " already exists.")
    writeFile (path <.> "json-tape") (encodeFiles (dirTree tree))

-- | Unpack directory tree contained in the given file.
unpackTree :: Options -> FilePath -> IO ()
unpackTree opts path = do
    unless
        (takeExtension path == ".json-tape")
        (fail "File to be unpacked has to have `json-tape` extension.")
    json <- readFile path
    case decodeFiles json of
        Left err -> fail err
        Right tree -> do
            let destination = takeDirectory path
            exists <- doesTreeExist destination tree
            when
                (exists && not (optForce opts))
                (fail $ "Tree " <> path <> " already exists.")
            void $
                writeDirectoryWith
                    (\fp content -> writeFile fp (getByteString64 content))
                    (destination :/ tree)

-- | Check whether the directory tree can be safely unpacked,
-- i.e. no file with the same name exists on the location.
doesTreeExist :: FilePath -> Object -> IO Bool
doesTreeExist anchor tree =
    case tree of
        Dir name _ -> doesDirectoryExist (anchor </> name)
        File name _ -> doesFileExist (anchor </> name)
        _ -> return False
