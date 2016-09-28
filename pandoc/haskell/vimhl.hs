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
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Control.Arrow (first, (&&&))
import Control.Monad
import Control.Conditional

vimHl :: Maybe Format -> Block -> IO Block
vimHl (Just fm@(Format fmt)) (CodeBlock (_, cls@(ft:_), namevals) contents)
    | lookup "hl" namevals' == Just "vim" && fmt `elem` ["html", "latex"] = do
        let vimhlcmd =
                unwords [cmd fmt, nmb]
                where cmd "html"  = "MakeHtmlCodeHighlight"
                      cmd "latex" = "MakeTexCodeHighlight"
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
                where cmd (Nothing          , x) = mkCmd x "1"
                      cmd (Just (x, _, y, _), _) = mkCmd x  y
                      mkCmd x y = "--cmd 'let g:" ++ x ++ " = \"" ++ y ++ "\"'"
                      dl        = mkRegex . ("\\s*" ++) . (++ "\\s*")
            vimrccmd = do
                home <- getHomeDirectory `catchIOError` const (return "")
                let vimrc  = home `combine` ".vimrc.pandoc"
                    exists = let (&&>) = liftM2 (<&&>)
                             in doesFileExist &&>
                                 (getPermissions >=> return . readable)
                    ($>) = liftM2 (<$>)
                (bool "" . ("--noplugin -u '" ++) . (++ "'")) $> exists $ vimrc
            runVim src dst hsrc hdst = do
                hPutStr hsrc contents
                mapM_ hClose [hsrc, hdst]
                vimrc <- vimrccmd
                {- vim must think that it was launched from a terminal,
                 - otherwise it won't load its usual environment and the
                 - syntax engine! -}
                hin <- openFile "/dev/tty" ReadMode
                (_, Just hout, _, handle) <- createProcess (shell $ unwords
                    ["vim -Nen", cmds, vimrc, colorscheme, "-c 'set ft=" ++ ft,
                     "|", vimhlcmd ++ "' -c 'w!", dst ++ "' -c 'qa!'", src])
                    {std_in = UseHandle hin, std_out = CreatePipe}
                waitForProcess handle
                mapM_ hClose [hin, hout]
        block <- withSystemTempFile "_vimhl_src." $
                    \src hsrc -> withSystemTempFile "_vimhl_dst." $
                        \dst hdst -> runVim src dst hsrc hdst >> readFile dst
        return $ RawBlock fm block
    where namevals' = map (first $ map toLower) namevals
vimHl _ cb = return cb

main :: IO ()
main = toJSONFilter vimHl

