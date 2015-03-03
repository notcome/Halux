{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude hiding (putStrLn)
import Data.Text.Lazy.IO (putStrLn)

import Control.Monad.Writer

-- Monoid Instance --
import Text.Blaze.Internal ()
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

writeHtml = renderHtml . execWriter

par = tell . H.p

link text url = H.a ! A.href url $ text

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
      par $ "五彩斑斓的网页由两个部分组成。其一标记文档内容，使用一种叫做超文本标记语言"<*"测试测试"*>""<*"Hyper Text Markup Language，"<>(link "HTML" "http://www.w3.org/TR/2014/PR-html5-20140916/Overview.html#contents")*>"的技术来书写；其二标记外观"<*"亦称「样式"*>"，使用一种叫做层叠样式表"<*"Cascading Style Sheet，"<>(link "CSS" "http://www.w3.org/Style/CSS/")*>"的技术来描述。"
  putStrLn result
