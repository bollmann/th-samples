{- | A VERY simple embedded substitution language. All it provides are
variables for the embedded language. A variable is of the form ${var}
and refers to an already defined identifier in the Haskell host
language. All other text in the embedded language is treated
literally.

While *SO* simple, this embedded substitution language, is already
quite powerful. For instance, we can write a simple HTML templating
language on top of it:

title     = "Hello World!"
introText = "This is my Webpage."

htmlPage = [subst|
  <html>
    <head><title>${title}</title></head>
    <body>
      <h1>${title}</h1>
      <p>${introText}</p>
    </body>
  </html> |]

Notice first, that inside the quasi quotes [subst| ... |] we use the
normal concrete HTML syntax. Moreover, we may refer to the Haskell
identifiers `title` and `introText` with the variables ${title} and
${introText}, respectively.

When run, the template htmlPage will replace the occurences ${title}
and ${introText} by the values that the Haskell identifiers title, and
introText hold, respectively. Thus, $(htmlPage) yields the
instantiated HTML template page:

  <html>
    <head><title>Hello World!</title></head>
    <body>
      <h1>Hello World!</h1>
      <p>This is my Webpage.</p>
    </body>
  </html>

-}
{-# LANGUAGE TemplateHaskell #-}
module SubstLang where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec as P

data SubstLang
  = Var   String
  | Lit   String
  | Parts [SubstLang]
  deriving( Eq, Show )

subst :: QuasiQuoter
subst = QuasiQuoter {
    quoteExp  = buildTH . parseSubstLang
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  } where notHandled thing =
            error $ "html: only expression parsing is supported, "
                 ++ "but you requested to parse " ++ thing ++ "."

buildTH :: SubstLang -> Q Exp
buildTH (Var   s)  = varE (mkName s)
buildTH (Lit   s)  = stringE s
buildTH (Parts ps) = foldr cat [| "" |] (map buildTH ps)
 where cat tl acc = [| $tl ++ $acc |]

parseSubstLang :: String -> SubstLang
parseSubstLang str = case P.parse parser "" str of
  Left err -> error $ "parseSubstLang:" ++ show err
  Right tl -> tl
  where
    parser  = Parts <$> P.many (P.try var <|> lit) <* P.eof
    var     = Var <$> (P.char '$' *>
                       P.char '{' *> P.many1 (P.noneOf "}") <* P.char '}')
    lit     = Lit <$> P.many1 (P.noneOf "$")
