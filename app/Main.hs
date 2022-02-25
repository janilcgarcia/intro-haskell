{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Conduit

import Text.XML.Stream.Parse

data Person = Person T.Text deriving (Show, Eq)

produtor :: Monad m => ConduitT i Int m ()
produtor = do
  yield 6
  yield 2
  yield 19
  yield 2

consumidor :: (Show a) => ConduitT a o IO ()
consumidor =
  awaitForever $ \inteiro ->
    liftIO $ print inteiro

mult = do
  x <- await
  y <- await

  let result = (*) <$> x <*> y

  case result of
    Nothing -> pure ()
    Just n -> do 
      yield n
      mult

doc = T.concat
  [ "<people>"
  , "  <person name=\"peter\" />"
  , "  <person name=\"miles\" />"
  , "</people>"
  ]

person = tag' "person" (requireAttr "name") $ \name ->
  pure $ Person name

people = do
  tag' "people" ignoreAttrs $ \_ ->
    manyYield person
  pure ()

main = do
  runConduit $ yield doc .| parseText def .| people .| consumidor
