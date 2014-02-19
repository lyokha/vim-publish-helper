#!/usr/bin/env runhaskell

-- vimhl.hs
import Text.Pandoc.JSON
import Text.Regex
import System.IO
import System.IO.Temp
import System.Directory
import System.FilePath
import System.Process
import Data.Char (toLower)

vimHl :: Maybe Format -> Block -> IO Block
vimHl (Just fm@(Format fmt)) cb@(CodeBlock (_, cls@(ft:_), namevals) contents)
  | lookup "hl" namevals' == Just "vim" && fmt `elem` ["html", "latex"] = do
      let vimhlcmd
              | fmt == "html"  = "MakeHtmlCodeHighlight" ++ nmb
              | fmt == "latex" = "MakeTexCodeHighlight" ++ nmb
              where nmb
                      | "numberLines" `elem` cls =
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
                  unwords (map cmd $ map flag $ filter (not . null) $
                      map (splitRegex regex') $ splitRegex regex val) ++ " "
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
          runVim src dst hsrc hdst = do
              hPutStr hsrc contents
              mapM_ hClose [hsrc, hdst]
              vimrc <- vimrcM
              {- vim must think that it was launched from a terminal, otherwise
               - it won't load its usual environment and the syntax engine! -}
              hin <- openFile "/dev/tty" ReadMode
              (_, Just hout, _, handle) <- createProcess (shell $
                  "vim -Nen " ++ cmds ++ vimrc ++ colorscheme ++
                  "-c 'set ft=" ++ ft ++ " | " ++ vimhlcmd ++ "' " ++
                  "-c 'w! " ++ dst ++ "' -c 'qa!' " ++ src)
                  {std_in = UseHandle hin, std_out = CreatePipe}
              waitForProcess handle
              mapM_ hClose [hin, hout]
      block <- withSystemTempFile "_vimhl_src." $
                  \src hsrc -> withSystemTempFile "_vimhl_dst." $
                      \dst hdst -> do
                          runVim src dst hsrc hdst
                          readFile dst
      return $ RawBlock fm block
  | otherwise = return cb
  where namevals' = map (\(x, y) -> (map toLower x, y)) namevals
vimHl _ cb = return cb

main :: IO ()
main = toJSONFilter vimHl

