{-# language DataKinds       #-}

module Quake3.Vertex ( Vertex ) where

-- base
import qualified Foreign.C

-- linear
import Math.Linear(V(..))


type Vertex = V 2 ( V 3 Foreign.C.CFloat )
