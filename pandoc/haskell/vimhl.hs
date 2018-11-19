#!/usr/bin/env runhaskell

-- vimhl.hs
import Text.Pandoc.JSON
import Text.Regex (mkRegex, splitRegex, matchRegexAll)
import System.IO
import System.IO.Temp
import System.IO.Error
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Control.Arrow (first, (&&&))
import Control.Monad
import Control.Exception (bracket)
import Control.Conditional hiding (unless)

vimHl :: Maybe Format -> Block -> IO Block
vimHl (Just fm@(Format fmt)) (CodeBlock (_, cls@(ft:_), namevals) contents)
    | lookup "hl" namevals' == Just "vim" && fmt `elem` ["html", "latex"] = do
        let vimhlcmd =
                unwords [cmd fmt, nmb]
                where cmd "html"  = "MakeHtmlCodeHighlight"
                      cmd "latex" = "MakeTexCodeHighlight"
                      cmd x       = error $ "Unexpected format '" ++ x ++ "'"
                      nmb | "numberLines" `elem` cls =
                              fromMaybe "-1" $ lookup "startfrom" namevals'
                          | otherwise = ""
            colorscheme =
                maybe "" (("-c 'let g:PhColorscheme = \"" ++) . (++ "\"'")) $
                    lookup "colorscheme" namevals'
            cmds =
                maybe "" (unwords . map cmd . filter (not . null . snd) .
                    map (matchRegexAll (dl"=") &&& id) . splitRegex (dl",")) $
                        lookup "vars" namevals'
                where cmd (Nothing, x) = mkCmd x "1"
                      cmd (Just ("", _, y, _), _) =
                          error $ "Bare value '" ++ y ++ "' found in vars"
                      cmd (Just (x, _, y, _), _) = mkCmd x y
                      mkCmd x y = "--cmd 'let g:" ++ x ++ " = \"" ++ y ++ "\"'"
                      dl = mkRegex . ("\\s*" ++) . (++ "\\s*")
        vimrccmd <- do
            home <- getHomeDirectory `catchIOError` const (return "")
            let vimrc  = home </> ".vimrc.pandoc"
                exists = doesFileExist &&>
                    (getPermissions >=> return . readable)
                (&&>) = liftM2 (<&&>)
                ($>)  = liftM2 (<$>)
            (bool "" . ("--noplugin -u '" ++) . (++ "'")) $> exists $ vimrc
        block <- withSystemTempFile "_vimhl_src." $ \src hsrc -> do
            hPutStr hsrc contents >> hFlush hsrc
            bracket (emptySystemTempFile "_vimhl_dst.") removeFile $
                \dst -> do
                    {- vim must think that it was launched from a terminal,
                     - otherwise it won't load its usual environment and the
                     - syntax engine! Using WriteMode for stdin prevents vim
                     - from getting unresponsive on Ctrl-C interrupts while
                     - still doing well its task (vim checks that input is a
                     - terminal using isatty(), however it does not check the
                     - mode of the handle). -}
                    hin <- openFile "/dev/tty" WriteMode
                    hout <- openFile "/dev/null" WriteMode
                    (_, _, _, handle) <- createProcess
                        (shell $ unwords
                            ["vim -Nen", cmds, vimrccmd, colorscheme
                            ,"-c 'set ft=" ++ ft, "|", vimhlcmd ++ "' -c 'w!"
                            ,dst ++ "' -c 'qa!'", src
                            ]
                        ) {std_in = UseHandle hin, std_out = UseHandle hout}
                    r <- waitForProcess handle
                    unless (r == ExitSuccess) $ exitWith r
                    readFile dst
        return $ RawBlock fm block
    where namevals' = map (first $ map toLower) namevals
vimHl _ cb = return cb

main :: IO ()
main = toJSONFilter vimHl

