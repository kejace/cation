{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}

module Cation.Client.Components.Contacts.View where

import           Cation.Client.Components.Contacts.Store
import           Cation.Client.Components.Table
import           Cation.Common.Api.Contacts
import           Data.Default
import           Data.Maybe                              (fromMaybe)
import           React.Flux                              hiding (table_)

contactsApp :: View '[] --()
contactsApp = mkControllerView @'[StoreArg ContactsStore] "contactsApp" $ \state ->
      div_ [ "className" $= "panel panel-default" ] $ do
        div_ [ "className" $= "panel-heading" ] $
          div_ [ "className" $= "container-fluid" ] $
            div_ [ "className" $= "row" ] $ 
              h3_ [ "className" $= "mt-md col flex-first" ] "Contacts"
        div_ [ "className" $= "panel-body" ] $
          case state of
            ContactsInit ->
              div_ [ "className" $= "text-center" ] $
                i_ [ "className" $= "fa fa-cog fa-spin fa-2x fa-fw" ] mempty
            ContactsState{..} ->
              div_ (contactTable_ contacts)

contactTableCfg :: TableCfg Contact handler
contactTableCfg =
  def { tblColumns =
          [ defCol "First Name" (elemText . contactFirstName)
          , defCol "Last Name" (elemText . contactLastName)
          , defCol "Email" (elemText . contactEmail)
          , defCol "Phone" (elemText . fromMaybe "" . contactPhone) ] }

contactTable_ :: [Contact] -> ReactElementM handler ()
contactTable_ = table_ contactTableCfg
