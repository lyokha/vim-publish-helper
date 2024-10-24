{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import Prelude hiding (Applicative (..))
import Control.Applicative (Applicative (..))
import Text.Pandoc.JSON
import System.IO (IOMode (WriteMode), openFile, hFlush)
import System.IO.Temp
import System.IO.Error
import System.IO.Unsafe
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
import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Data.Text.IO as T

vimHl :: Maybe Format -> Block -> IO Block
vimHl (Just fmt) (CodeBlock (_, cls@(ft : _), namevals) contents)
    | Just "vim" <- lookup "hl" namevals'
    , fmt `elem` ["html", "latex", "gfm"] = do
        let vimhlcmd = unwords [cmd fmt, nmb]
                where cmd "latex" = "MakeTexCodeHighlight"
                      cmd _ = "MakeHtmlCodeHighlight"
                      nmb | "numberLines" `elem` cls =
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
        block <- withSystemTempFile "_vimhl_src." $ \src hsrc -> do
            T.hPutStr hsrc contents >> hFlush hsrc
            bracket (emptySystemTempFile "_vimhl_dst.") removeFile $ \dst -> do
                let vimrccmd = maybe ""
                        (("--noplugin -u '" ++) . (++ "'")) vimrcPandoc
                    vimcmd = unwords
                        [vimExe, "-Nen", cmds, vimrccmd, colorscheme
                        ,"-c 'set ft=" ++ T.unpack ft, "|"
                        ,vimhlcmd ++ "' -c 'w!", dst ++ "' -c 'qa!'"
                        ,src
                        ]
                {- Vim must think that it has been launched from a terminal,
                 - otherwise it won't load its usual environment and the syntax
                 - engine! Using WriteMode for stdin prevents Vim from getting
                 - unresponsive on Ctrl-C interrupts while it still keeps doing
                 - well its task (Vim checks that input is a terminal using
                 - isatty(), however it does not check the mode of the handle).
                 - Note that Neovim loads the syntax engine without tty
                 - emulation just fine. -}
                hin <- if vimExeIsNvim
                           then return Nothing
                           else Just <$> openFile "/dev/tty" WriteMode
                hout <- openFile "/dev/null" WriteMode
                (_, _, _, handle) <- createProcess $
                    let cmd = (shell vimcmd) {std_out = UseHandle hout}
                    in maybe cmd (\h -> cmd {std_in = UseHandle h}) hin
                r <- waitForProcess handle
                unless (r == ExitSuccess) $ exitWith r
                T.readFile dst
        return $ uncurry RawBlock $ decorate fmt block
    where namevals' = map (T.map toLower *** T.unpack) namevals
          {- Note that Github markdown sanitizer strips CSS styles in HTML
           - tags. See details at https://github.com/github/markup#readme. -}
          decorate "gfm" block =
              ("html"
              ,T.concat ["<div class=\"highlight notranslate \
                         \position-relative overflow-auto\" dir=\"auto\" \
                         \data-snippet-clipboard-copy-content=\""
                        ,escapeHtml $ stripShellOutputPrompt contents
                        ,"\">"
                        ,block
                        ,"</div>"
                        ]
              )
          decorate fm block = (fm, block)
          escapeHtml = T.concatMap $ \case
              '<' -> "&lt;"
              '>' -> "&gt;"
              '&' -> "&amp;"
              '"' -> "&quot;"
              '\'' -> "&apos;"
              x -> T.singleton x
          stripShellOutputPrompt t
              | ft == "shelloutput" =
                  let prompt = "||| "
                  in T.replace ("\n" `T.append` prompt) "\n" $
                      fromMaybe t $ T.stripPrefix prompt t
              | otherwise = t
vimHl _ cb = return cb

vimExe :: String
vimExe = unsafePerformIO $ fromMaybe "vim" <$> lookupEnv "VIMHL_BACKEND"
{-# NOINLINE vimExe #-}

vimExeIsNvim :: Bool
vimExeIsNvim = unsafePerformIO $
    bracket (emptySystemTempFile "_vimhl_test.") removeFile $ \test -> do
        let vimcmd = unwords
                [vimExe
                ,"-Nens --noplugin -u NONE \
                 \-c 'if has(\"nvim\") | exe \"normal i1\" | endif' -c wq"
                ,test
                ]
        hout <- openFile "/dev/null" WriteMode
        (_, _, _, handle) <- createProcess $
            (shell vimcmd) {std_out = UseHandle hout}
        r <- waitForProcess handle
        unless (r == ExitSuccess) $ exitWith r
        maybe False (('1' ==) . fst) . T.uncons <$> T.readFile test
{-# NOINLINE vimExeIsNvim #-}

vimrcPandoc :: Maybe String
vimrcPandoc = unsafePerformIO $ lookupEnv "VIMRC_PANDOC" >>=
    maybe (do
               home <- getHomeDirectory `catchIOError` const (return "")
               let vimrc = home </> ".vimrc.pandoc"
                   returnIf = liftA2 fmap $ bool Nothing . Just
                   exists = doesFileExist &&> (fmap readable . getPermissions)
                   (&&>) = liftA2 andM
               returnIf exists vimrc
          ) (return . Just)
{-# NOINLINE vimrcPandoc #-}

main :: IO ()
main = toJSONFilter vimHl

