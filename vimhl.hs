#!/usr/bin/env runhaskell

-- vimhl.hs
import Text.Pandoc.JSON
import System.IO
import System.Directory
import System.FilePath
import System.Process

vimHl :: Maybe Format -> Block -> IO Block
vimHl (Just format) cb@(CodeBlock (id, classes@(ft:_), namevals) contents)
  | format == Format "html" || format == Format "latex" =
        case lookup "hl" namevals of
        Just "vim" -> do
            let tempbuf  = "_vimhl_buffer"
                tempfile = "_vimhl_result"
                vimhlcmd
                    | format == Format "html"  = "MakeHtmlCodeHighlight"
                    | format == Format "latex" = "MakeTexCodeHighlight" ++ nmb
                    where nmb
                            | "numberLines" `elem` classes =
                                case lookup "startfrom" namevals of
                                Nothing  -> " -1"
                                Just val -> " " ++ val
                            | otherwise = ""
                colorscheme =
                    case lookup "colorscheme" namevals of
                    Nothing -> ""
                    Just val -> "-c 'let g:PhColorscheme = \"" ++ val ++ "\"' "
                vimrcM = do
                    home <- getHomeDirectory
                    exists <- doesFileExist $ vimrc home
                    if exists
                        then do
                            permissions <- getPermissions $ vimrc home
                            if readable permissions
                                then return $ "--noplugin -u '" ++ vimrc home ++
                                              "' "
                                else return ""
                        else return ""
                    where vimrc home = home `combine` ".vimrc.pandoc"
            vimrc <- vimrcM
            writeFile tempbuf contents
            {- vim must think that it was launched from a terminal, otherwise
             - it won't load its usual environment and the syntax engine! -}
            hin <- openFile "/dev/tty" ReadMode
            (_, Just hout, _, handle) <- createProcess (shell $
                "vim -Nen " ++ vimrc ++ colorscheme ++ "-c 'set ft=" ++ ft ++
                " | " ++ vimhlcmd ++ "' " ++ "-c 'w! " ++ tempfile ++
                "' -c 'qa!' " ++ tempbuf)
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
vimHl _ cb = return cb

main :: IO ()
main = toJSONFilter vimHl

