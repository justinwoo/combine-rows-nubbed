module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row
import Type.Data.Row (RProxy(..))

combination
  :: forall r1 r2 r' r
   . Row.Union r1 r2 r'
  => Row.Nub r' r
  => RProxy r1 -> RProxy r2 -> RProxy r
combination _ _ = RProxy

type Row1 = ( apple :: String, banana :: String )
type Row2 = ( apple :: String, kiwi :: String )

result :: RProxy
  ( apple :: String
  , banana :: String
  , kiwi :: String
  )
result = combination (RProxy :: _ Row1) (RProxy :: _ Row2)

isSupersetTwo
  :: forall r1 r2 r r1' r2'
   . Row.Union r1 r1' r
  => Row.Union r2 r2' r
  => RProxy r1 -> RProxy r2 -> RProxy r -> Unit
isSupersetTwo _ _ _ = unit

type Row3 =
  ( apple :: String
  , banana :: String
  , kiwi :: String
  )

result2 :: Unit
result2 = isSupersetTwo (RProxy :: _ Row1) (RProxy :: _ Row2) (RProxy :: _ Row3)

main :: Effect Unit
main = do
  log "Hello sailor!"
