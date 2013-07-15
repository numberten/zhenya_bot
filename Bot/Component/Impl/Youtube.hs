module Bot.Component.Impl.Youtube (
    youtube
)   where

import Text.Regex.TDFA


-- Grabs multiple results  from msgs based on pat
let pat = "youtube.com/watch\\?v=[a-zA-Z0-9]*"
let msg = "blah blah blah www.youtube.com/watch?v=kr7dXN7wJP0 blah blah blah"

concat ((msg ++ msg) =~ pat :: [[String]])

-- The contents of a youtube page
--   <span id="eow-title" class="watch-title  yt-uix-expander-head" dir="ltr"
--   title="Bright Eyes - We are nowhere and it&#39;s now live"> Bright Eyes -
--   We are nowhere and it&#39;s now live </span>

-- The contents of the description
-- <p id="eow-description" >Bright Eyes -We are nowhere and it&#39;s now</p>

-- Check out tagsoup for parsing web
-- http://hackage.haskell.org/package/tagsoup-0.12.8
-- http://hackage.haskell.org/packages/archive/download-curl/0.1.4/doc/html/Network-Curl-Download.html
-- http://www.cs.york.ac.uk/fp/darcs/tagsoup/tagsoup.htm

-- It looks like you will need do some sort of html decoding...
-- http://hackage.haskell.org/packages/archive/web-encodings/0.3.0.2/doc/html/Web-Encodings.html
