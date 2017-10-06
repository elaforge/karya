module Ness.Submit where
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Time as Time

import qualified System.Environment
import qualified System.Exit as Exit
import qualified System.Process as Process

import qualified Util.ParseText as ParseText
import qualified Util.Thread as Thread
import Global


loginForm :: String
loginForm = "https://www.ease.ed.ac.uk/"

loginUrl :: String
loginUrl = "https://www.ease.ed.ac.uk/cosign.cgi"

cookies :: FilePath
cookies = "ness-data/cookies"

-- post
-- enctype="application/x-www-form-urlencoded"
--
-- login=qdunkan@gmail.com password=***
-- ref=https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html
-- service=cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk

submitUrl :: String
submitUrl =
    "https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiZCRemoteRunner.php"

main :: IO ()
main = do
    [instrument, score, out] <- System.Environment.getArgs
    submitDownload False instrument score out

submitDownload :: Bool -> FilePath -> FilePath -> FilePath -> IO ()
submitDownload isDemo instrument score out = do
    result <- submit isDemo instrument score
    case result of
        Left err -> errorIO err
        Right (url, runTime) -> do
            putStrLn $ "=== response: " ++ url
            putStrLn $ "=== run time: " ++ show runTime
            ok <- download runTime out url
            unless ok $ putStrLn "=== gave up"
            return ()

-- TODO I get a cosign=xyz cookie, and get "logged in but no access to service"
-- I should get a cosign-eucsCosign-ness-frontend etc.
-- is it ignoring the service= in the login?
login :: String -> String -> IO ()
login user password = do
    -- go to loginForm, to get the cosign cookie
    Process.callProcess "curl"
        [ "--cookie-jar", cookies
        , "--output", "curl.loginForm"
        , loginForm
        ]
    let values =
            [ ("ref", "https://ness-frontend.epcc.ed.ac.uk/\
                \~pgraham/NUI/Web/nuiWebMain.html")
            , ("service", "cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk")
            , ("login", user)
            , ("password", password)
            ]
    Process.callProcess "curl" $
        [ "--insecure"
        , "--verbose"
        , "--cookie", cookies
        , "--cookie-jar", cookies
        , "--output", "curl.login"
        -- follow the location response to pick up the next cookie
        , "--location"
        ] ++ concat [["--data-raw", k <> "=" <> v] | (k, v) <- values]
        ++ [loginUrl]

submit :: Bool -> FilePath -> FilePath -> IO (Either Text (String, Double))
submit isDemo instrument score = do
    putStrLn $ "=== submit " ++ show (instrument, score)
    json <- Process.readProcess "curl"
        [ "--insecure"
        , "--cookie", cookies
        , "--form", "scorefile=@" <> score
        , "--form", "instrfile=@" <> instrument
        , "--form", "booleanDemo=" <> if isDemo then "true" else "false"
        -- , "--verbose"
        , submitUrl
        ]
        ""
    either errorIO return $ first txt $ parseJson $
        ByteString.Lazy.Char8.pack json

download :: Double -> FilePath -> String -> IO Bool
download runTime out url = go =<< Time.getCurrentTime
    where
    go start = do
        ok <- poll out url
        if ok then return True
            else do
                now <- Time.getCurrentTime
                if realToFrac (now `Time.diffUTCTime` start)
                        > runTime + extraSeconds
                    then return False
                    else Thread.delay 2 >> go start

extraSeconds :: Double
extraSeconds = 5

poll :: FilePath -> String -> IO Bool
poll out url = do
    exit <- Process.waitForProcess =<< Process.spawnProcess "curl"
        [ "--insecure"
        , "--cookie", cookies
        , "--output", out
        -- , "--verbose"
        , "--fail"
        , url
        ]
    return $ exit == Exit.ExitSuccess

type Error = String

parseJson :: ByteString.Lazy.ByteString
    -> Either Error (Either Text (String, Double))
parseJson json = do
    json <- justErr ("json: " <> show json) $ Aeson.decode json
    flip Aeson.Types.parseEither json $ \obj -> do
        status <- obj .: "runExitStatus"
        serverError <- obj .: "serverError"
        if status == (1 :: Int) || serverError
            then Left <$> obj .: "serverHtmlOutput"
            else do
                dir <- obj .: "serverResultDir"
                time <- obj .: "runEstRunTime"
                return $ Right
                    ( dir <> "/output-mix.wav"
                    , fromMaybe 0 $ ParseText.float time
                    )

{-
    request:
    POST /cosign.cgi HTTP/1.1
    Host: www.ease.ed.ac.uk
    Content-Type: application/x-www-form-urlencoded

    response:
    HTTP/1.1 302 Found
    Set-Cookie: cosign=CEQaxKckosFgCXzJsOnvW-rP83vQO0Eplx8iN2uTBalJwzJR5Du1UxmfVF7rpvQ-1F3ME6AL9mUMZaq1nwLhlmcFBommvof-ig+KWVR0lCWNaQWQ5vNHUnno152+/1507052428/1; path=/; secure; httponly
    Location: https://ness-frontend.epcc.ed.ac.uk/cosign/valid?cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk=Mxkh0M1yPdDA4reAaK3nWM1+7pGKjZUp1LKITC4wghBDZl3tvXCo6GIMen7UliPzFXKKbcOPR6jRaCHjcV9uaOz1grKpwLVndOIRo3MBcIfPFpG1aGKteC9J6cy9&https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html

    request:
    GET /cosign/valid?cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk=Mxkh0M1yPdDA4reAaK3nWM1+7pGKjZUp1LKITC4wghBDZl3tvXCo6GIMen7UliPzFXKKbcOPR6jRaCHjcV9uaOz1grKpwLVndOIRo3MBcIfPFpG1aGKteC9J6cy9&https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html HTTP/1.1
    Host: ness-frontend.epcc.ed.ac.uk
    Referer: https://www.ease.ed.ac.uk/?cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk&https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html
    Cookie: __utma=1.1368592074.1500506252.1506442495.1506442495.1; __utmc=1; __utmz=1.1506442495.1.1.utmcsr=ease.ed.ac.uk|utmccn=(referral)|utmcmd=referral|utmcct=/services/; cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk=0mUzWEFxRJyOd0lieWX2ounligWObZR0yGpLCctJnqcIOgF-0pDS5tkvKpfaBL+TR7jdSt1X25PPu+eVU8rWoADuGXhna+rN4viffuk3xstZmTUkuR1DsnpyOznz/1506989239; _ga=GA1.3.1368592074.1500506252; _gid=GA1.3.103846257.1506977666

    response:
    HTTP/1.1 301 Moved Permanently
    Date: Tue, 03 Oct 2017 17:40:30 GMT
    Server: Apache/2.2.15 (Scientific Linux) mod_ssl/2.2.15 OpenSSL/1.0.1e-fips PHP/5.3.3
    Set-Cookie: cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk=kOpZNV4Ost1ArmNxbg1woNujTzkldsiDN3EZmoV-Eer+7vZyn+2561p8DF6ef2FwYt0UjuKejIDRbf5h3y4mL3ohHYPwEz7+58yGA5-x67GzkkVGkEmT31wgsyQE/1507052430; path=/; secure
    Location: https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html
    Content-Length: 276
    Keep-Alive: timeout=15, max=100
    Connection: Keep-Alive
    Content-Type: text/html; charset=iso-8859-1
-}

{-
    request: loginForm
    POST /cosign.cgi HTTP/1.1
    Host: www.ease.ed.ac.uk
    Connection: keep-alive
    Content-Length: 207
    Cache-Control: max-age=0
    Origin: https://www.ease.ed.ac.uk
    Upgrade-Insecure-Requests: 1
    Content-Type: application/x-www-form-urlencoded
    User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36
    Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8
    Referer: https://www.ease.ed.ac.uk/?cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk&https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html
    Accept-Encoding: gzip, deflate, br
    Accept-Language: en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4,zh-TW;q=0.2
    Cookie: cosign-eucsCosign-www.ease.ed.ac.uk=TWJNSFfb3wqOvrjf5h+r28kEzld4jR6xAJlmcUvlmIxV4mWWc7CGPgWkeqdLTp1MGkgqz0paD0jG62Yvsik8ZswofmJU9ssELftYDcfI081gJBI5i1PY4eNpS+Za/1506442362; __utma=1.1368592074.1500506252.1506442495.1506442495.1; __utmc=1; __utmz=1.1506442495.1.1.utmcsr=ease.ed.ac.uk|utmccn=(referral)|utmcmd=referral|utmcct=/services/; _ga=GA1.3.1368592074.1500506252; _gid=GA1.3.103846257.1506977666; cosign=CEQaxKckosFgCXzJsOnvW-rP83vQO0Eplx8iN2uTBalJwzJR5Du1UxmfVF7rpvQ-1F3ME6AL9mUMZaq1nwLhlmcFBommvof-ig+KWVR0lCWNaQWQ5vNHUnno152+/1507051835

    response:
    HTTP/1.1 302 Found
    Date: Tue, 03 Oct 2017 17:40:28 GMT
    Server: Apache
    Set-Cookie: cosign=CEQaxKckosFgCXzJsOnvW-rP83vQO0Eplx8iN2uTBalJwzJR5Du1UxmfVF7rpvQ-1F3ME6AL9mUMZaq1nwLhlmcFBommvof-ig+KWVR0lCWNaQWQ5vNHUnno152+/1507052428/1; path=/; secure; httponly
    Location: https://ness-frontend.epcc.ed.ac.uk/cosign/valid?cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk=Mxkh0M1yPdDA4reAaK3nWM1+7pGKjZUp1LKITC4wghBDZl3tvXCo6GIMen7UliPzFXKKbcOPR6jRaCHjcV9uaOz1grKpwLVndOIRo3MBcIfPFpG1aGKteC9J6cy9&https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html
    Content-Length: 476
    Keep-Alive: timeout=5, max=100
    Connection: Keep-Alive
    Content-Type: text/html; charset=iso-8859-1
    Request Headers
    view parsed

    request:
    GET /cosign/valid?cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk=Mxkh0M1yPdDA4reAaK3nWM1+7pGKjZUp1LKITC4wghBDZl3tvXCo6GIMen7UliPzFXKKbcOPR6jRaCHjcV9uaOz1grKpwLVndOIRo3MBcIfPFpG1aGKteC9J6cy9&https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html HTTP/1.1
    Host: ness-frontend.epcc.ed.ac.uk
    Connection: keep-alive
    Cache-Control: max-age=0
    Upgrade-Insecure-Requests: 1
    User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36
    Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8
    Referer: https://www.ease.ed.ac.uk/?cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk&https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html
    Accept-Encoding: gzip, deflate, br
    Accept-Language: en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4,zh-TW;q=0.2
    Cookie: __utma=1.1368592074.1500506252.1506442495.1506442495.1; __utmc=1; __utmz=1.1506442495.1.1.utmcsr=ease.ed.ac.uk|utmccn=(referral)|utmcmd=referral|utmcct=/services/; cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk=0mUzWEFxRJyOd0lieWX2ounligWObZR0yGpLCctJnqcIOgF-0pDS5tkvKpfaBL+TR7jdSt1X25PPu+eVU8rWoADuGXhna+rN4viffuk3xstZmTUkuR1DsnpyOznz/1506989239; _ga=GA1.3.1368592074.1500506252; _gid=GA1.3.103846257.1506977666

    response:
    HTTP/1.1 301 Moved Permanently
    Date: Tue, 03 Oct 2017 17:40:30 GMT
    Server: Apache/2.2.15 (Scientific Linux) mod_ssl/2.2.15 OpenSSL/1.0.1e-fips PHP/5.3.3
    Set-Cookie: cosign-eucsCosign-ness-frontend.epcc.ed.ac.uk=kOpZNV4Ost1ArmNxbg1woNujTzkldsiDN3EZmoV-Eer+7vZyn+2561p8DF6ef2FwYt0UjuKejIDRbf5h3y4mL3ohHYPwEz7+58yGA5-x67GzkkVGkEmT31wgsyQE/1507052430; path=/; secure
    Location: https://ness-frontend.epcc.ed.ac.uk/~pgraham/NUI/Web/nuiWebMain.html
    Content-Length: 276
    Keep-Alive: timeout=15, max=100
    Connection: Keep-Alive
    Content-Type: text/html; charset=iso-8859-1

-}
