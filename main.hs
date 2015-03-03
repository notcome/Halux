{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude hiding (putStrLn)
import Data.Text.Lazy.IO (putStrLn)

import Control.Monad.Writer

-- Monoid Instance --
import Text.Blaze.Internal ()
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

writeHtml = renderHtml . execWriter

par = tell . H.p

infixl 3 <*
infixl 5 <<*
infixr 4 *>>
infixr 2 *>

a <* b = a <> H.span b
a <<* b = a <> H.span b

(*>) = (<>)
(*>>) = (<>)

main = do
  let result = writeHtml $ do
      H.section $ execWriter $ do
        
        
      par $ "thread"<*"线程"*>"是……"
      par $ "thread"<*"线程"<<*"或"<>H.span "duang"*>>"线程"*>"是……"
  putStrLn result
