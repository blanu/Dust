import System.IO
import System.INotify
import System.Directory
import System.FilePath
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main = do
    inotify <- initINotify
    print inotify
    home <- getHomeDirectory 
    let posts = home </> "Dust" </> "hs" </> "sneakermesh"
    wd <- addWatch inotify [MoveIn] posts (publish posts)
    putStrLn "Listening for posts. Hit enter to terminate."
    getLine
    removeWatch wd

publish :: FilePath -> Event -> IO()
publish dir event = do
    let filename = filePath event
    let filepath = dir </> filename
    post <- B.readFile filepath
    let lines = BC.lines post
    let title=filename
    let date = getDate lines
    let postname = date ++ "-" ++ filename ++ ".markdown"
    home <- getHomeDirectory 
    let postpath = home </> "octopress" </> "source" </> "_posts" </> postname
    copyFile filepath postpath
    
    return ()

getTitle :: [B.ByteString] -> B.ByteString
getTitle lines =
    let dates = filter (BC.isPrefixOf (BC.pack "title: ")) lines
    in (head . tail . (BC.split ' ') . head) dates

getDate :: [B.ByteString] -> String
getDate lines =
    let dates = filter (BC.isPrefixOf (BC.pack "date: ")) lines
    in (BC.unpack . head . (drop 1) . (BC.split ' ') . head) dates

