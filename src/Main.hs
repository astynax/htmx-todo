module Main where

import Network.Wai
import Network.Wai.Cli (defWaiMain)
import Text.Blaze.Html5 (Markup, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty hiding (html)
import Web.Scotty qualified as Scotty
import XStatic
import XStatic.TH

main :: IO ()
main = defWaiMain =<< scottyApp app

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
    here

here :: Markup
here =
  H.div
  ! H.customAttribute "hx-get" "/enter"
  ! H.customAttribute "hx-trigger" "mouseenter once"
  $ "[ Here Mouse, Mouse! ]"

gotYa :: Markup
gotYa =
  H.div
  ! H.customAttribute "hx-get" "/leave"
  ! H.customAttribute "hx-trigger" "mouseleave once"
  $ "[ Got ya! ]"
