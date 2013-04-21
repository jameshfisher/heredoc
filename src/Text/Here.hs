module Text.Here (here, there) where

import Language.Haskell.TH
  ( litE
  , stringL
  )

import Language.Haskell.TH.Quote
  ( QuasiQuoter
    ( QuasiQuoter
    , quoteExp
    , quotePat
    , quoteType
    , quoteDec
    )
  , quoteFile
  )

err :: String -> String
err ctx =
  "You have used the `here` QuasiQuoter in a " ++ ctx ++ " context; " ++
  "you must only use it as a string literal expression"

-- | Create a string-literal expression from the string being quoted.
here :: QuasiQuoter
here = QuasiQuoter
  { quoteExp  = litE . stringL
  , quotePat  = const $ error $ err "pattern"
  , quoteType = const $ error $ err "type"
  , quoteDec  = const $ error $ err "declaration"
  }

-- | Create a string-literal expression from
-- the contents of the file at the filepath being quoted.
there :: QuasiQuoter
there = quoteFile here