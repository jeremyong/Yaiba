{-# OPTIONS_GHC -fdph-par #-}
{-# LANGUAGE PArr #-}
module Yaiba.SPoly where

import Data.Map
--import Data.Array.Parallel.Prelude
import Yaiba.Sugar
import Yaiba.Polynomial

newtype SPoly ord = SP (Map (Sugar ord) [:Polynomial ord:])

syzygy [] _ = SP empty
--syzygy ms a = 