module Main (main) where

import           WriteAST                                 ( astToFile )
import           Parser                                   ( instructions )

import           Control.Monad                            ( mapM
                                                          , forM_
                                                          , when
                                                          )
import           Data.Attoparsec.Text                     ( parseOnly )
import           Data.List                                ( isSuffixOf )
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as TIO
                                                          ( readFile )
import           System.Directory                         ( doesFileExist
                                                          , doesDirectoryExist
                                                          , listDirectory
                                                          , getCurrentDirectory
                                                          )
import           System.Environment                       ( getArgs
                                                          , getProgName
                                                          )
import           System.Exit                              ( exitSuccess
                                                          , exitFailure 
                                                          )
import           System.FilePath                          ( replaceExtensions
                                                          , splitExtensions
                                                          , takeExtensions
                                                          , combine
                                                          )
import           System.IO                                ( stderr
                                                          , hPutStrLn
                                                          )

isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext = isSuffixOf ('.':ext) . takeExtensions

main :: IO ()
main = do
    args <- getArgs
    when ("--help" `elem` args || "-h" `elem` args) $ usage >> exitSuccess
    cwd    <- getCurrentDirectory
    inputs <- case args of
        [] -> getUMCFiles cwd
        _  -> concat <$> mapM getUMCFiles args
    when (null inputs) $ putStrLn "No matching files!" >> usage >> exitFailure
    forM_ inputs compileFile

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "usage: " ++ progName ++ " [<files>] [<directories>]"
    putStrLn "\num-compiler compiles .umc files into .um files"
    putStrLn "By default, um-compiler works on the current working directory"

compileFile :: FilePath -> IO ()
compileFile inFile = do
    let outFile = replaceExtensions inFile ".um"
    content <- TIO.readFile inFile
    case parseOnly instructions content of
        Left  e   -> putStrLn $ "Error parsing " ++ inFile ++ ": " ++ e
        Right ast -> astToFile ast outFile

getUMCFiles :: FilePath -> IO [FilePath]
getUMCFiles path = do
    fileExists <- doesFileExist path
    dirExists  <- doesDirectoryExist path
    case (fileExists, dirExists) of
        (True, _) -> if ".umc" `isExtensionOf` path
            then return [path]
            else do
                hPutStrLn stderr
                    $  "skipping file "
                    ++ show path
                    ++ ": not a .umc file"
                return []
        (_, True) ->
            filter (isExtensionOf ".umc")
                .   map (combine path)
                <$> listDirectory path
        _ -> return []


