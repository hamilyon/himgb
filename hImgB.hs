{-# LANGUAGE BangPatterns #-}
module Main where

import Network.CGI
import Graphics.GD
import Text.XHtml

import qualified Data.ByteString.Lazy as B
import Data.Char

import System.Posix.Time
import System.Environment
import System.IO

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TMVar

data Post = Post { up_name :: String
                 , file    :: Maybe String
                 , up_time :: Int
                 , txt     :: String
    } deriving (Show, Read)

data Thread = Thread { posts :: [Post]
                     , eyde  :: Int
    } deriving (Show, Read)

data HState = HState { ibtitle :: String
                     , threads :: [Thread]
    } deriving (Show, Read)

-- -------------------------------------------------------------------------

addPost :: Thread -> Post -> Thread
addPost t p = Thread (posts t ++ [p]) (eyde t) 
 
newThread :: HState -> Post -> HState
newThread h po = h { threads = (Thread [po] (up_time po)): threads h }

addPost' :: HState -> Int -> Post -> HState
addPost' hs idx po = 
    let t  = filter (\th -> idx == eyde th) (threads hs)
        nt = addPost (head t) po
        t' = filter (\th -> idx /= eyde th) (threads hs)
     in hs { threads = nt:t' }

addSagedPost :: HState -> Int -> Post -> HState
addSagedPost hs idx po = hs { threads = map (\th -> 
    if idx == eyde th then addPost th po else th) (threads hs) }

-- -------------------------------------------------------------------------

formatImage :: Maybe String -> Html
formatImage Nothing  = noHtml
formatImage (Just l) = lnk (image ! [src (fth l)]) l

fth :: String -> String
fth s = take (length s - 4) s ++ "s" ++ drop (length s - 4) s

formatPost :: Post -> Html
formatPost po = p (toHtml (replicate 70 '-')) +++
                p (bold (toHtml "author:")) +++
                (toHtml (up_name po)) +++
                p (formatImage (file po)) +++
                p (bold (toHtml "txt:")) +++
                (toHtml (txt po)) +++ 
                br +++ 
                br

replyForm eeydee =
  form (hidden "id" eeydee +++
        (case eeydee of
           "1" -> p (bold (toHtml "create new thread:"))
           _   -> p (bold (toHtml "reply to thread:"))) +++
        p (toHtml "name:") +++
        textfield "name" +++
        p (toHtml "Text:") +++
        textfield "txt" +++
        p (toHtml "file") +++
        afile "upfile" +++
        submit "send" "submit") ! [ method "POST"
                                  , enctype "multipart/form-data"]

formatThread :: Thread -> Html
formatThread t = hr +++ 
                 foldr ((+++).formatPost) noHtml (posts t) +++
                 replyForm (show (eyde t)) +++
                 hr

renderimgb :: HState -> Html
renderimgb h = (thetitle (toHtml (ibtitle h))) +++
               h1 (toHtml (ibtitle h)) +++ 
               h2 (toHtml "broken so far: once") +++
               hr +++
               replyForm "1" +++
               (foldr ((+++).formatThread) noHtml $ threads h) +++
               (p $ toHtml $ lnk "source" "http://code.google.com/p/himgb/")

lnk txt hrf = anchor (toHtml txt) ! [href hrf]

-- ------------------------------------------------------------------------

makethumb :: FilePath -> IO ()
makethumb !file = do
    t <- getFileType file
    img <- case t of ".jpg" -> loadJpegFile file
                     ".gif" -> loadGifFile  file
                     ".png" -> loadPngFile  file
    (sizex, sizey) <- imageSize img
    let ratiox = fromIntegral sizex / 100.0
        ratioy = fromIntegral sizey / 100.0
        ratio = max ratiox ratioy
    img' <- resizeImage (round (fromIntegral sizex / ratio))
                        (round (fromIntegral sizey / ratio)) img
    case t of ".jpg" -> saveJpegFile (-1) (fth file) img'
              ".png" -> savePngFile       (fth file) img'
              ".gif" -> saveGifFile       (fth file) img'

mkPost :: String -> Maybe String -> String -> String -> Post
mkPost n f u t = Post { up_name = take 20 (filter isAscii n)
                      , file    = f
                      , up_time = read u
                      , txt     = take 500 (filter isAscii t) }

overwriteConfig :: Handle -> HState -> IO ()
overwriteConfig h cfg = do hSeek h AbsoluteSeek 0
                           hPutStrLn h (show (expirethreads cfg 8))
                           hClose h

validthread :: HState -> String -> Bool
validthread hst idx = 
    not $ null $ filter (\th -> eyde th == read idx) (threads hst)

expirethreads :: HState -> Int -> HState
expirethreads hst n = hst { threads = take n (threads hst) }

howmanyposts :: HState -> String -> Int
howmanyposts hst idx = 
    let thd = head $ filter (\th -> eyde th == read idx) (threads hst)
    in length (posts thd)

getFileType :: String -> IO String
getFileType s = let fff = map toLower (reverse s) in
    return $ case fff of
        ('g':'e':'p':'j':_) -> ".jpg"
        ('g':'p':'j':_)     -> ".jpg"
        ('f':'i':'g':_)     -> ".gif"
        ('g':'n':'p':_)     -> ".png"
        _                   -> "unknown"

-- ------------------------------------------------------------------------

compute_ :: Int -> IO a -> IO (Maybe a)
compute_ limit computation = do
    result <- atomically newEmptyTMVar
    runner <- forkIO $ do c <- computation
                          atomically $ putTMVar result $ Just c
    reader <- forkIO $ do threadDelay limit
                          killThread runner
                          atomically $ putTMVar result $ Nothing
    a <- atomically $ takeTMVar result
    killThread runner
    killThread reader
    return a

compute :: IO () -> IO ()
compute fn = compute_ (5*10^6) fn >> return ()

-- ------------------------------------------------------------------------

main :: IO ()
main = runCGI (handleErrors imgb)

imgb :: CGI CGIResult
imgb = do
    args    <- liftIO getArgs
    efile   <- liftIO $ openFile "stateh" ReadWriteMode
    idx     <- liftM  (maybe               "" id) $ getInput "id"
    text    <- liftM  (maybe        "silence" id) $ getInput "txt"
    bslfile <- liftM  (maybe          B.empty id) $ getInputFPS "upfile"
    fname   <- liftM  (maybe               "" id) $ getInputFilename "upfile"
    name    <- liftM  (maybe "Anonymous Hero" id) $ getInput "name"

    hst   <- (case args of 
      ("purge":_) -> return $ HState "hImgB" []
      _           -> liftM read (liftIO (hGetLine efile)))
    ftype <- liftIO (getFileType fname)
    time  <- liftM show (liftIO epochTime)

    case null idx of
      True  -> do
        liftIO $ do overwriteConfig efile hst
                    hClose efile
        output $ showHtml $ renderimgb hst

      False -> do
        when (ftype == "unknown")
          (fail "images only pls")
        case read idx of
          1 ->
            if B.null bslfile
              then fail "must supply image with new threads"
              else do liftIO $ do
                        let img = "img/"++time++ftype
                        B.writeFile img bslfile
                        compute $ makethumb img
                        let nt = newThread hst (mkPost
                                  name (Just img) time text)
                        overwriteConfig efile nt
                      output $ showHtml $ "Saved!"

          _ -> do liftIO $ do
                    unless (validthread hst idx) (fail
                      "that thread does not exist")
                    when (howmanyposts hst idx >= 20) (fail
                      "that thread has already reached its limit")
                    if B.null bslfile
                      then do when (null text) (fail
                                "reply with either an image or text or both")
                              let nt = addPost' hst (read idx) (mkPost
                                        name  Nothing  time  text)
                              overwriteConfig efile nt
                      else do let img = "img/"++time++ftype
                              B.writeFile img bslfile
                              compute $ makethumb img
                              let nt = addPost' hst (read idx) (mkPost
                                        name  (Just img) time  text)
                              overwriteConfig efile nt
                  output $ showHtml $ "Saved!"

