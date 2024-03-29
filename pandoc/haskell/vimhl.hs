{-# LANGUAGE CPP #-}

import Text.Pandoc.JSON
import System.IO (IOMode (WriteMode), openFile, hFlush)
import System.IO.Temp
import System.IO.Error
import System.Environment (lookupEnv)
import System.Directory
import System.Directory.Internal (andM)
import System.FilePath
import System.Process
import System.Exit
import Data.List
import Data.Char (isSpace, toLower)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Control.Arrow
import Control.Monad
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception (bracket)
#if MIN_VERSION_pandoc_types(1,20,0)
import Prelude hiding (readFile)
import Data.Text.IO (readFile)
import qualified Data.Text.IO as P (hPutStr)
import Data.Text (Text, unpack)
#else
import qualified System.IO as P (hPutStr)
#endif
#ifdef DEBUG
import System.IO (hPutStr, hPutStrLn, stderr)
#endif

#if MIN_VERSION_pandoc_types(1,20,0)
tOSTRING :: Text -> String
tOSTRING = unpack
#else
tOSTRING :: String -> String
tOSTRING = id
#endif

vimHl :: Maybe Format -> Block -> IO Block
vimHl (Just fm@(Format fmt)) (CodeBlock (_, cls@(ft : _), namevals) contents)
    | lookup "hl" namevals' == Just "vim" && fmt' `elem` ["html", "latex"] = do
        let vimhlcmd = unwords [cmd fmt', nmb]
                where cmd "html" = "MakeHtmlCodeHighlight"
                      cmd "latex" = "MakeTexCodeHighlight"
                      cmd _ = undefined
                      nmb | "numberLines" `elem` cls' =
                              fromMaybe "-1" $ lookup "startfrom" namevals'
                          | otherwise = ""
            colorscheme = maybe "" (("-c 'let g:PhColorscheme = \"" ++)
                                   . (++ "\"'")
                                   ) $ lookup "colorscheme" namevals'
            cmds = maybe "" (unwords
                            . map cmd
                            . filter (not . null . fst)
                            . map ((strip *** strip . drop 1) . break (== '='))
                            . filter (/= ",")
                            . groupBy ((&&) `on` (/= ','))
                            ) $ lookup "vars" namevals'
                where cmd (x, "") = mkCmd x "1"
                      cmd (x, y) = mkCmd x y
                      mkCmd x y = "--cmd 'let g:" ++ x ++ " = \"" ++ y ++ "\"'"
                      strip = dropWhileEnd isSpace . dropWhile isSpace
        vimrccmd <- do
            home <- getHomeDirectory `catchIOError` const (return "")
            let vimrc = home </> ".vimrc.pandoc"
                exists = doesFileExist &&> (fmap readable . getPermissions)
                (&&>) = liftM2 andM
                (<<$) = liftM2 (<$>)
            (bool "" . ("--noplugin -u '" ++) . (++ "'")) <<$ exists $ vimrc
        block <- withSystemTempFile "_vimhl_src." $ \src hsrc -> do
            P.hPutStr hsrc contents >> hFlush hsrc
            bracket (emptySystemTempFile "_vimhl_dst.") removeFile $
                \dst -> do
                    vimexe <- fromMaybe "vim" <$> lookupEnv "VIMHL_BACKEND"
                    let vimcmd =
                            unwords
                                [vimexe, "-Nen", cmds, vimrccmd, colorscheme
                                ,"-c 'set ft=" ++ ft', "|"
                                ,vimhlcmd ++ "' -c 'w!", dst ++ "' -c 'qa!'"
                                ,src
                                ]
#ifdef DEBUG
                    hPutStr stderr $ vimcmd ++ " ... "
#endif
                    {- vim must think that it was launched from a terminal,
                     - otherwise it won't load its usual environment and the
                     - syntax engine! Using WriteMode for stdin prevents vim
                     - from getting unresponsive on Ctrl-C interrupts while
                     - still doing well its task (vim checks that input is a
                     - terminal using isatty(), however it does not check the
                     - mode of the handle). -}
                    hin <- openFile "/dev/tty" WriteMode
                    hout <- openFile "/dev/null" WriteMode
                    (_, _, _, handle) <- createProcess (shell vimcmd)
                        {std_in = UseHandle hin, std_out = UseHandle hout}
                    r <- waitForProcess handle
#ifdef DEBUG
                    hPutStrLn stderr $ show r
#endif
                    unless (r == ExitSuccess) $ exitWith r
                    readFile dst
        return $ RawBlock fm block
    where fmt' = tOSTRING fmt
          cls' = map tOSTRING cls
          ft' = tOSTRING ft
          namevals' = map (map toLower . tOSTRING *** tOSTRING) namevals
vimHl _ cb = return cb

main :: IO ()
main = toJSONFilter vimHl

