{-# LANGUAGE OverloadedStrings, LambdaCase #-}

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
import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Data.Text.IO as T

vimHl :: Maybe Format -> Block -> IO Block
vimHl (Just fm@(Format fmt)) (CodeBlock (_, cls@(ft : _), namevals) contents)
    | lookup "hl" namevals' == Just "vim" &&
        fmt `elem` ["html", "latex", "gfm"] = do
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
        vimrccmd <- do
            home <- getHomeDirectory `catchIOError` const (return "")
            vimrc <- fromMaybe (home </> ".vimrc.pandoc") <$>
                lookupEnv "VIMRC_PANDOC"
            let exists = doesFileExist &&> (fmap readable . getPermissions)
                (&&>) = liftM2 andM
                (<<$) = liftM2 (<$>)
            (bool "" . ("--noplugin -u '" ++) . (++ "'")) <<$ exists $ vimrc
        block <- withSystemTempFile "_vimhl_src." $ \src hsrc -> do
            T.hPutStr hsrc contents >> hFlush hsrc
            bracket (emptySystemTempFile "_vimhl_dst.") removeFile $
                \dst -> do
                    vimexe <- fromMaybe "vim" <$> lookupEnv "VIMHL_BACKEND"
                    let vimcmd =
                            unwords
                                [vimexe, "-Nen", cmds, vimrccmd, colorscheme
                                ,"-c 'set ft=" ++ T.unpack ft, "|"
                                ,vimhlcmd ++ "' -c 'w!", dst ++ "' -c 'qa!'"
                                ,src
                                ]
                    {- Vim must think that it was launched from a terminal,
                     - otherwise it won't load its usual environment and the
                     - syntax engine! Using WriteMode for stdin prevents Vim
                     - from getting unresponsive on Ctrl-C interrupts while
                     - still doing well its task (Vim checks that input is a
                     - terminal using isatty(), however it does not check the
                     - mode of the handle). Note that Neovim loads the syntax
                     - engine just fine. -}
                    hin <- (Just <$> openFile "/dev/tty" WriteMode)
                        `catchIOError` const (return Nothing)
                    hout <- openFile "/dev/null" WriteMode
                    (_, _, _, handle) <- createProcess $
                        maybe (shell vimcmd) {std_out = UseHandle hout}
                            (\hin' -> (shell vimcmd)
                                {std_in = UseHandle hin'
                                ,std_out = UseHandle hout
                                }
                            ) hin
                    r <- waitForProcess handle
                    unless (r == ExitSuccess) $ exitWith r
                    T.readFile dst
        return $ RawBlock fm' $ wrap fm block
    where namevals' = map (map toLower . T.unpack *** T.unpack) namevals
          fm' | fm == Format "latex" = fm
              | otherwise = Format "html"
          wrap "gfm" block = T.concat
              ["<div class=\"highlight notranslate \
               \position-relative overflow-auto\" dir=\"auto\" \
               \data-snippet-clipboard-copy-content=\""
              ,escapeHtml $ stripShellOutputPrompt contents
              ,"\">"
              ,block
              ,"</div>"
              ]
          wrap _ block = block
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

main :: IO ()
main = toJSONFilter vimHl

