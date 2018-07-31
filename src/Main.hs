{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

import           CMark                                hiding ( Url )
import           Control.Monad.IO.Class               ( liftIO )
import           Control.Monad.Trans.Class            ( MonadTrans(..) )
import qualified Data.Aeson                           as AE
import qualified Data.Diet.Map.Strict.Unboxed.Lifted  as D
import           Data.IORef
import qualified Data.Map.Strict                      as MS
import qualified Data.Text.IO                         as T
import qualified Data.Text.Lazy                       as TL
import           GeoliteApi.Siphon                    ( getCsvs, replaceCsvs )
import           GeoliteApi.Types                     ( Maps(..) )
import           Network.Wai.Middleware.RequestLogger
import           System.Cron
import           System.Mem                           ( performMajorGC )
import           Web.Scotty

main :: IO ()
main = do
  -- At startup, create our 'Maps',
  -- and store them in an 'IORef'.
  csvRef :: IORef Maps <- getCsvs >>= newIORef
  -- Force a major GC to prevent unnecessary
  -- space usage until 'csvRef' gets overwritten.
  performMajorGC
  -- Add a cron job that periodically downloads
  -- the new CSVs and updates the 'csvRef'.
  _ <- execSchedule $ do
    addJob (replaceCsvs csvRef) "30 2 * * *"
  -- Start the server.
  server csvRef

server :: IORef Maps -> IO ()
server imaps = do
  scotty 3000 $ do
    ----- logger
    middleware logStdoutDev
    ----- help page
    get (regex "/help|readme") $ do
      readMe <- liftIO $ T.readFile "./README.md"
      html $ sakura <> (TL.fromStrict $ commonmarkToHtml [] readMe)
    ----- IPv4 queries
    get "/ipv4/asn/:ip" $ do
      query <- param "ip"
      csvMaps <- lift $ readIORef imaps 
      json $ AE.toJSON $ D.lookup query $ asnipv4diet csvMaps
    get "/ipv4/country/:ip" $ do
      query <- param "ip"
      csvMaps <- lift $ readIORef imaps 
      json $ AE.toJSON $ D.lookup query $ countryipv4diet csvMaps
    get "/ipv4/city/:ip" $ do
      query <- param "ip"
      csvMaps <- lift $ readIORef imaps 
      json $ AE.toJSON $ D.lookup query $ cityBlockipv4diet csvMaps
    ----- IPv6 queries
    get "/ipv6/asn/:ip" $ do
      query <- param "ip"
      csvMaps <- lift $ readIORef imaps 
      json $ AE.toJSON $ D.lookup query $ asnipv6diet csvMaps
    get "/ipv6/country/:ip" $ do
      query <- param "ip"
      csvMaps <- lift $  readIORef imaps 
      json $ AE.toJSON $ D.lookup query $ countryipv6diet csvMaps
    get "/ipv6/city/:ip" $ do
      query <- param "ip"
      csvMaps <- lift $ readIORef imaps 
      json $ AE.toJSON $ D.lookup query $ cityBlockipv6diet csvMaps
    ----- gid queries
    get "/gid/city/:gid" $ do
      query <- param "gid"
      csvMaps <- lift $ readIORef imaps 
      json $ AE.toJSON $ MS.lookup (Just query) $ cityLocationMap csvMaps
    get "/gid/country/:gid" $ do
      query <- param "gid"
      csvMaps <- lift $ readIORef imaps 
      json $ AE.toJSON $ MS.lookup (Just query) $ cityLocationMap csvMaps

sakura :: TL.Text
sakura = "<link rel=\"stylesheet\"\
        \ href=\"https://unpkg.com/sakura.css/css/sakura.css\"\
        \ type=\"text/css\">"
