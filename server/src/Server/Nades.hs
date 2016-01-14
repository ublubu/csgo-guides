{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Nades where

import Servant
import Servant.Server

import API.Nades
import Server.App
import Server.Nade
import Server.NadeList

nadesServer :: ServerT NadesAPI App
nadesServer = nadeServer :<|> nadeListServer
