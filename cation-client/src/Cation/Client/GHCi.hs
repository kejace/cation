{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Cation.Client.GHCi where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.JSString (JSString, toUpper)
import React.Flux

loadDeps :: IO ()
loadDeps = do
  js_addScript reactCdn
  js_addScript reactDomCdn
  js_createRoot "app"

reactCdn :: JSString
reactCdn = 
  "https://cdnjs.cloudflare.com/ajax/libs/react/15.5.4/react.min.js"

reactDomCdn :: JSString
reactDomCdn = 
  "https://cdnjs.cloudflare.com/ajax/libs/react/15.5.4/react-dom.min.js"

renderApp :: IO ()
renderApp = reactRenderView "app" view'
  where
    view' :: View '[] 
    view' = undefined -- mkControllerView @'[] "app" $ const $ do
              --h1_ "Hello, Test!"
              --view_ nameForm mempty mempty
              --view_ flavorForm mempty mempty

data TextInputArgs = TextInputArgs {
    tiaType :: JSString
  , tiaValue :: JSString
} deriving (Eq, Generic, Typeable)

instance UnoverlapAllEq TextInputArgs

-- instance StoreData TextInputArgs where
--   type StoreAction TextInputArgs = 
-- 
--   transform action state = undefined

nameForm :: View '[TextInputArgs] 
nameForm = mkStatefulView "nameForm" "" $
  \state args ->
      form_ [] $ do
        label_ [] $ do
          "Name: "
          input_ [ "type" $= "text"
                 , "value" $= state
                 , onChange $ \e _ ->
                     ([], Just (toUpper $ target e "value")) ]
        input_ [ "type" $= "submit", "value" $= "Submit" ]

flavorForm :: View '[TextInputArgs]
flavorForm = mkStatefulView "flavorForm" "" $
  \state args ->
    form_ [] $ do
      label_ [] $ do
        "Pick your favorite La Croix flavor: "
        select_ [ "value" $= state
                , onChange $ \e _ -> ([], Just (target e "value")) ] $ do
          option_ [ "value" $= "grapefruit" ] "Grapefruit"
          option_ [ "value" $= "lime" ] "Lime"
          option_ [ "value" $= "coconut" ] "Coconut"
          option_ [ "value" $= "mango" ] "Mango"
      input_ [ "type" $= "submit"
             , "value" $= "Submit" ]

foreign import javascript unsafe
  "function addScript(src) {\
  \  var head = document.getElementsByTagName('head')[0];\
  \  var script = document.createElement('script');\
  \  script.type = 'text/javascript';\
  \  script.src = src;\
  \  head.appendChild(script);\
  \}addScript($1);"
  js_addScript :: JSString -> IO ()

foreign import javascript unsafe
  "function createRoot(name) {\
  \  var e = document.createElement('div');\
  \  e.id = name;\
  \  document.body.appendChild(e);\
  \}createRoot($1);"
  js_createRoot :: JSString -> IO ()
