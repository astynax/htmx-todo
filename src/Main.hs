module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Network.Wai
import Network.Wai.Cli (defWaiMain)
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Text.Blaze.Html5 (Markup, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty hiding (html)
import Web.Scotty qualified as Scotty
import XStatic
import XStatic.TH

main :: IO ()
main = defWaiMain . withWS =<< scottyApp app
  where
    withWS = websocketsOr defaultConnectionOptions wsApp

app :: ScottyM ()
app = do
  middleware $ xstaticMiddleware
    [ $(embedXStaticFile "data/htmx.min.js.gz") ]
  get "/" $ html index
  get "/enter" $ html gotYa
  get "/leave" $ html here
  where
    html = Scotty.html . renderHtml

index :: Markup
index = H.docTypeHtml do
  H.head do
    H.title "HTMX TODO"
    H.script
      ! A.src "/xstatic/htmx.min.js"
      $ pure ()
  H.body do
    ticker
    here

here :: Markup
here =
  H.span
  ! H.customAttribute "hx-get" "/enter"
  ! H.customAttribute "hx-trigger" "mouseenter once"
  $ "[ Here Mouse, Mouse! ]"

gotYa :: Markup
gotYa =
  H.span
  ! H.customAttribute "hx-get" "/leave"
  ! H.customAttribute "hx-trigger" "mouseleave once"
  $ "[ Got ya! ]"

ticker :: Markup
ticker =
  H.span
  ! H.customAttribute "hx-ws" "connect:/"
  $ H.span ! A.id "ticker" $ "..."

wsApp :: ServerApp
wsApp pendingConn = do
  conn <- acceptRequest pendingConn
  forever do
    sendTextData conn . renderHtml $ H.span ! A.id "ticker" $ "> tick <"
    threadDelay 1000000
    sendTextData conn . renderHtml $ H.span ! A.id "ticker" $ "< tock >"
    threadDelay 1000000
