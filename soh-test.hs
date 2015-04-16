{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell, TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}
import ClassyPrelude.Yesod
import System.IO.Temp
import Data.FileEmbed (embedFile)
import Data.Conduit.Process
import System.Exit
import System.Environment
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import System.FilePath (dropExtension)

data App = App Int [(String, String)] (MVar (Maybe StreamingProcessHandle))

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]
instance Yesod App
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

defCode :: Text
defCode = decodeUtf8 $(embedFile "defcode.hs")

getHomeR, postHomeR :: Handler Html
getHomeR = postHomeR
postHomeR = do
    ((res, widget), enctype) <- runFormPost $ renderDivs $ areq textareaField "Some code" $ Just $ Textarea defCode
    case res of
        FormSuccess (Textarea code) -> withSystemTempFile "someapp.hs" $ \fp h -> do
            (out, err, ec) <- liftIO $ do
                hPut h $ encodeUtf8 code
                hClose h
                (ClosedStream, out, err, sph) <- streamingProcess (proc "ghc" [fp, "-threaded", "-rtsopts"])
                runConcurrently $ (,,)
                    <$> Concurrently (out $$ decodeUtf8C =$ sinkLazy)
                    <*> Concurrently (err $$ decodeUtf8C =$ sinkLazy)
                    <*> Concurrently (waitForStreamingProcess sph)
            if ec == ExitSuccess
                then do
                    App userPort env' var <- getYesod
                    liftIO $ modifyMVar_ var $ \moldSPH -> do
                        forM_ moldSPH $ terminateProcess . streamingProcessHandleRaw
                        let fp' = dropExtension fp
                        (ClosedStream, Inherited, Inherited, sph) <-
                            streamingProcess (proc fp' [])
                                { env = Just env'
                                }
                        return $ Just sph
                    defaultLayout $ do
                        setTitle "Started"
                        [whamlet|
                            <h1>Started
                            <p>Connect to port #{userPort}
                        |]
                else defaultLayout $ do
                    setTitle "Compilation Failed"
                    [whamlet|
                        <h1>Compilation Failed
                        <h2>stdout
                        <pre>#{out}
                        <h2>stderr
                        <pre>#{err}
                    |]
        _ -> defaultLayout $ do
            setTitle "Silly compiler"
            [whamlet|
                <form method=post enctype=#{enctype}>
                    ^{widget}
                    <div>
                        <button>Compile and launch
            |]
            toWidget [lucius| textarea { width: 400px; height: 400px; } |]

main :: IO ()
main = do
    env0 <- getEnvironment
    userPort <-
        case lookup "USER_PORT" env0 of
            Nothing -> error "USER_PORT not set"
            Just p ->
                case readMay p of
                    Nothing -> error $ "USER_PORT not a valid port: " ++ p
                    Just i -> return i
    -- FIXME in future, reverse proxy to the user port
    var <- newMVar Nothing
    let app = App userPort (insertMap "PORT" (show userPort) env0) var
    warpEnv app
