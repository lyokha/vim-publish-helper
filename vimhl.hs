#!/usr/bin/env runhaskell

-- vimhl.hs
import Text.Pandoc.JSON
import Text.Regex (mkRegex, splitRegex)
import System.IO
import System.IO.Temp
import System.Directory
import System.FilePath
import System.Process
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Control.Arrow (first)

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
                maybe "" (unwords . map (cmd . flag) . filter (not . null) .
                            map (splitRegex $ dl"=") . splitRegex (dl",")) $
                                lookup "vars" namevals'
                where cmd (x:y:_) =
                                "--cmd 'let g:" ++ x ++ " = \"" ++ y ++ "\"'"
                      flag [x]    = [x, "1"]
                      flag x      = x
                      dl          = mkRegex . ("\\s*" ++) . (++ "\\s*")
            rc = do
                home <- getHomeDirectory
                let vimrc = home `combine` ".vimrc.pandoc"
                exists <- doesFileExist vimrc
                if exists
                    then do
                        permissions <- getPermissions vimrc
                        return $ if readable permissions
                                     then "--noplugin -u '" ++ vimrc ++ "'"
                                     else ""
                    else return ""
            runVim src dst hsrc hdst = do
                hPutStr hsrc contents
                mapM_ hClose [hsrc, hdst]
                vimrc <- rc
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

