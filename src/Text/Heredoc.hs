{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Text.Heredoc
  ( here
  , there
  , str
  , Bindable
    ( bind
    )
  ) where

import Control.Applicative
  ( (<$>)
  , (<*)
  , (*>)
  , (<|>))
import Text.Parsec
  ( parse
  , try
  , eof
  , newline
  , many
  , many1
  , noneOf
  , char
  )
import Text.Parsec.String (Parser)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Language.Haskell.TH
  ( litE
  , stringL
  , listE
  , appE
  , varE
  , stringE
  , mkName
  , ExpQ
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

import Text.Heredoc.Bindable

data Ctx = Exp | Pat | Type | Dec

qq :: String -> Ctx -> QuasiQuoter
qq qqName correctCtx = QuasiQuoter
  { quoteExp  = const $ error $ errorString Exp
  , quotePat  = const $ error $ errorString Pat
  , quoteType = const $ error $ errorString Type
  , quoteDec  = const $ error $ errorString Dec
  }
  where
    errorString ctx =
      "You have used the `" ++ qqName ++ "` QuasiQuoter " ++
      "in " ++ ctxName ctx ++ " context; " ++
      "you must only use it in " ++ ctxName correctCtx ++ " context"

    ctxName c = case c of
      Exp  -> "an expression"
      Pat  -> "a pattern"
      Type -> "a type"
      Dec  -> "a declaration"

skipNL :: String -> String
skipNL []       = []
skipNL str@(x:xs)
    | x == '\n' = xs
    | otherwise = str

parser :: String -> ExpQ
parser = f . parse binding "binding"
  where
    f (Left err) = fail (show err)
    f (Right xs) = xs

binding :: Parser ExpQ
binding = appE (varE 'concat) . listE <$> many content <* eof

content :: Parser ExpQ
content = try value
    <|> try (stringE . (:[]) <$> escape)
    <|> stringE <$> text

value :: Parser ExpQ
value = do
    char '$'
    char '{'
    str <- many (noneOf "}")
    char '}'
    return [|bind $(varE (mkName str))|]

escape :: Parser Char
escape = char '$' *> char '$'

text :: Parser String
text = many1 (noneOf "$")

toUnix :: String -> String
toUnix cs = case cs of
  '\r':'\n' : cs -> '\n' : toUnix cs
  '\r'      : cs -> '\n' : toUnix cs
  c         : cs -> c    : toUnix cs
  []             -> []

{-| Create a string-literal expression from the string being quoted.

    Newline literals are normalized to UNIX newlines (one '\n' character).

    It is possible to bind the valiable.

    >>> :set -XQuasiQuotes
    >>> let name = "Eli" :: String
    >>> [here|Her name is ${name}.|]
    "Her name is Eli."
    >>> let num = 9 :: Int
    >>> [here|This group contains ${num} members.|]
    "This group contains 9 members."
    >>> [here|$$12|]
    "$12"
-}
here :: QuasiQuoter
here = here' skipNL

here' :: (String -> String) -> QuasiQuoter
here' f = (qq "here" Exp) { quoteExp  = parser . f . toUnix }

{-| Create a string-literal expression from
    the contents of the file at the filepath being quoted.

    Newline literals are normalized to UNIX newlines (one '\n' character).
-}
there :: QuasiQuoter
there = quoteFile (here' id)

{-| Create a multi-line string literal whose left edge is demarcated by the
    "pipe" character ('|'). For example,

    >famousQuote = [str|Any dictator would admire the
    >                  |uniformity and obedience of the U.S. media.
    >                  |
    >                  |    -- Noam Chomsky
    >                  |]

    is functionally equivalent to

    >famousQuote = "Any dictator would admire the\n" ++
    >              "uniformity and obedience of the U.S. media.\n" ++
    >              "\n" ++
    >              "    -- Noam Chomsky\n"

    If desired, you can have a ragged left-edge, so

    >myHtml = [str|<html>
    >                 |<body>
    >                     |<h1>My home page</h1>
    >                 |</body>
    >             |</html>
    >             |]

    is functionally equivalent to

    >myHtml = "<html>\n" ++
    >         "<body>\n" ++
    >         "<h1>My home page</h1>\n" ++
    >          "</body>\n" ++
    >         "</html>\n"
-}
str :: QuasiQuoter
str = (qq "str" Exp)
      { quoteExp = parser . intercalate "\n" . unPipe . lines . toUnix }
  where
    unPipe ls = case ls of
      []     -> []
      l : ls -> l : case splitLast ls of
        Nothing              -> []
        Just (middles, last) ->
          map removePipe middles ++ [fromMaybe "" (tryRemovePipe last)]
            where
            removePipe cs = case tryRemovePipe cs of
              Nothing -> error "no pipe character found in line '" ++ cs ++ "'"
              Just cs -> cs

            tryRemovePipe cs = case dropWhile (/='|') cs of
              []   -> Nothing
              c:cs -> Just cs

    splitLast :: [a] -> Maybe ([a], a)
    splitLast xs = case reverse xs of
      []  -> Nothing
      l:i -> Just (reverse i, l)
