#!/usr/bin/env runhaskell

-- vimhl.hs
import Text.Pandoc.JSON
import System.IO
import System.Directory
import System.FilePath
import System.Process
import Text.Regex
import Data.Char (toLower)

vimHl :: Maybe Format -> Block -> IO Block
vimHl (Just format) cb@(CodeBlock (id, classes@(ft:_), namevals) contents)
  | format == Format "html" || format == Format "latex" =
        case lookup "hl" namevals' of
        Just "vim" -> do
            let tempbuf  = "_vimhl_buffer"
                tempfile = "_vimhl_result"
                vimhlcmd
                    | format == Format "html"  = "MakeHtmlCodeHighlight" ++ nmb
                    | format == Format "latex" = "MakeTexCodeHighlight" ++ nmb
                    where nmb
                            | "numberLines" `elem` classes =
                                case lookup "startfrom" namevals' of
                                Nothing  -> " -1"
                                Just val -> " " ++ val
                            | otherwise = ""
                colorscheme =
                    case lookup "colorscheme" namevals' of
                    Nothing -> ""
                    Just val -> "-c 'let g:PhColorscheme = \"" ++ val ++ "\"' "
                cmds =
                    case lookup "vars" namevals' of
                    Nothing -> ""
                    Just val ->
                        unwords (map cmd (map flag (filter (not . null)
                            (map (splitRegex regex')
                                 (splitRegex regex val))))) ++ " "
                        where cmd (x:y:_) =
                                  "--cmd 'let g:" ++ x ++ " = \"" ++ y ++ "\"'"
                              flag [x]    = [x, "1"]
                              flag x      = x
                              regex       = mkRegex "[[:space:]]*,[[:space:]]*"
                              regex'      = mkRegex "[[:space:]]*=[[:space:]]*"
                vimrcM = do
                    home <- getHomeDirectory
                    let vimrc = home `combine` ".vimrc.pandoc"
                    exists <- doesFileExist vimrc
                    if exists
                        then do
                            permissions <- getPermissions vimrc
                            if readable permissions
                                then return $ "--noplugin -u '" ++ vimrc ++ "' "
                                else return ""
                        else return ""
            vimrc <- vimrcM
            writeFile tempbuf contents
            {- vim must think that it was launched from a terminal, otherwise
             - it won't load its usual environment and the syntax engine! -}
            hin <- openFile "/dev/tty" ReadMode
            (_, Just hout, _, handle) <- createProcess (shell $
                "vim -Nen " ++ cmds ++ vimrc ++ colorscheme ++
                "-c 'set ft=" ++ ft ++ " | " ++ vimhlcmd ++ "' " ++
                "-c 'w! " ++ tempfile ++ "' -c 'qa!' " ++ tempbuf)
                {std_in = UseHandle hin, std_out = CreatePipe}
            waitForProcess handle
            hClose hin
            hClose hout
            block <- readFile tempfile
            removeFile tempbuf
            removeFile tempfile
            return $ RawBlock format block
        _          -> return cb
  | otherwise = return cb
  where namevals' = map (\(x, y) -> (map (\x -> toLower x) x, y)) namevals
vimHl _ cb = return cb

main :: IO ()
main = toJSONFilter vimHl

