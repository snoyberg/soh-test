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
import Data.UUID (toString)
import Data.UUID.V4
import Data.Yaml (encode)

data NgrokConfig = NgrokConfig
    Text -- authtoken
    Text -- subdomain
    Int -- port
instance ToJSON NgrokConfig where
    toJSON (NgrokConfig auth subd port) = object
        [ "authtoken" .= auth
        , "tunnels" .= object
            [ subd .= object
                [ "proto" .= asText "http"
                , "addr" .= port
                ]
            ]
        ]

data App = App Text (IORef Int) [(String, String)]

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
                    App auth iport env0 <- getYesod
                    name <- liftIO $ do
                        port <- atomicModifyIORef iport $ \p -> let p' = p + 1 in (p', p')
                        let env' = insertMap "PORT" (show port) env0
                            fp' = dropExtension fp
                        (ClosedStream, Inherited, Inherited, _sph) <-
                            streamingProcess (proc fp' [])
                                { env = Just env'
                                }
                        name <- (pack . toString) <$> nextRandom
                        withSystemTempFile "ngrokconfig.yml" $ \fp h -> do
                            hPut h $ encode $ NgrokConfig auth name port
                            hClose h
                            (ClosedStream, Inherited, Inherited, _sph) <-
                                streamingProcess (proc "./ngrok" ["start", "-config", fp, "--all"])
                            threadDelay 100000
                            return name
                    defaultLayout $ do
                        setTitle "Started"
                        let url = concat ["https://", name, ".ngrok.io/"]
                        [whamlet|
                            <h1>Started
                            <a href=#{url}>#{url}
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
    app <- App
        <$> (pack <$> getEnv "NGROK_AUTH")
        <*> newIORef 10000
        <*> pure (deleteMap "NGROK_AUTH" $ deleteMap "PORT" env0)
    warpEnv app
