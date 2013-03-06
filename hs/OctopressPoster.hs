import System.IO
import System.INotify
import System.Directory

main = do
    inotify <- initINotify
    print inotify
    posts <- getHomeDirectory </> "octopress" </> "sources" </> "_posts"
    wd <- addWatch
            inotify
            [Open,Close,Access,Modify,Move]
            posts
            print
    print wd
    putStrLn "Listens to your home directory. Hit enter to terminate."
    getLine
    removeWatch inotify wd
