{- Type-safe regular expressions using Template Haskell.

This example shows how to use Template Haskell and Quasi Quotes to
implement type-safe regular expressions (*). The user may specify
regular expressions conveniently as a string and yet receives static
guarantees that the regular expression is syntactically correct.

For instance, consider a regular expression to check the
wellformedness of an email address:

chars           = [regex|[a-z]|[A-Z]|[0-9]|]
someChars       = [regex|${chars}${chars}*|]
validDotComMail = [regex|${someChars}@${someChars}.com|]

Using quasi quotes, this regular expression can be specified in its
concrete syntax. Moreover, larger regular expressions can easily be
built from smaller expressions with the interpolation ${..} operator,
which splices sub regexes into larger ones.

Thus, illformed regular expressions are detected and excluded at
compile-time of the Haskell module.

(*) The example is based on homework #5 of Penn's CIS 552 course.
-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module RegExps where

import Control.Monad.Identity
import Data.Data
import Data.List
import Data.Set (Set)
import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Data.Set as Set
import Text.Parsec as P hiding (Empty)

data RegExp
  = Char (Set Char)          -- literal characters
  | Alt RegExp RegExp        -- r1 | r2   (alternation)
  | Seq RegExp RegExp        -- r1 r2     (concatenation)
  | Star RegExp              -- r*        (Kleene star)
  | Empty                    -- ε, accepts empty string
  | Void                     -- ∅, always fails
  | Var String               -- a variable holding another regexp
  deriving Show

instance Lift a => Lift (Set a) where
  lift set = appE (varE 'Set.fromList) (lift (Set.toList set))

instance Lift RegExp where
  -- lift :: RegExp -> Q Exp
  lift (Char cs)     = apply 'Char  [lift cs]
  lift (Alt r1 r2)   = apply 'Alt   (map lift [r1, r2])
  lift (Seq r1 r2)   = apply 'Seq   (map lift [r1, r2])
  lift (Star r1)     = apply 'Star  (map lift [r1])
  lift Empty         = apply 'Empty []
  lift Void          = apply 'Void  []
  lift (Var r)       = foldl1 appE $ map (varE . mkName) (words r)

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)

-- | Checks if the 'RegExp' `r` matches the given string `s`.
match :: RegExp -> String -> Bool
match r s = nullable (foldl deriv r s)

-- | `nullable r` returns `True` when `r` matches the empty string
nullable :: RegExp -> Bool
nullable (Char _)    = False
nullable (Alt r1 r2) = nullable r1 || nullable r2
nullable (Seq r1 r2) = nullable r1 && nullable r2
nullable (Star _)    = True
nullable Empty       = True
nullable Void        = False
nullable (Var _)     = False -- has been replaced by the pointed regexp already
                             -- due to the regex quasi quoter.

-- | Takes a regular expression `r` and a character `c`, and computes
-- a new regular expression that accepts word `w` if `cw` is accepted
-- by `r`.
deriv :: RegExp -> Char -> RegExp
deriv (Char cs) c
  | c `Set.member` cs = Empty
  | otherwise         = Void
deriv (Alt r1 r2) c   = Alt (deriv r1 c) (deriv r2 c)
deriv (Seq r1 r2) c
  | nullable r1       = Alt (Seq (deriv r1 c) r2) (deriv r2 c)
  | otherwise         = Seq (deriv r1 c) r2
deriv (Star r) c      = deriv (Alt Empty (Seq r (Star r))) c
deriv Empty _         = Void
deriv Void _          = Void
deriv (Var _) _       = Void

-- | Compiles a string representation of a regular expression into its
-- AST form using TH.
compile :: String -> Q (TExp RegExp)
compile s =
  case parse regexParser "" s of
    Left  err    -> fail (show err)
    Right regexp -> [e|| regexp ||]

-- | Parses the given regular expression into the 'RegExp' datatype.
regexParser :: Parsec String () RegExp
regexParser = alts <* eof where
  atom       = try var <|> char
  var        = Var <$> (string "${" *> many1 (noneOf "}") <* P.char '}')
  char       = charclass <|> singlechar
  singlechar = (Char . Set.singleton) <$> noneOf specials
  charclass  = fmap (Char . Set.fromList) $
                 P.char '[' *> content <* P.char ']'
  content    = try (concat <$> many1 range)
                   <|> many1 (noneOf specials)
  range      = enumFromTo
                 <$> (noneOf specials <* P.char '-')
                 <*> noneOf specials
  alts       = try (Alt <$> seqs <*> (P.char '|' *> alts)) <|> seqs
  seqs       = try (Seq <$> star <*> seqs) <|> star
  star       = try (Star <$> (atom <* P.char '*'))
                 <|> try (Star <$> (P.char '(' *> alts <* string ")*"))
                 <|> atom
  specials   = "[]()*|"

-- | A quasi quoter for embedding the regular expression language into
-- Haskell.
regex :: QuasiQuoter
regex = QuasiQuoter {
    quoteExp = unTypeQ . compile
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  } where notHandled things =
            error $ things ++ " are not handled by the subst quasiquoter."

-- alphaNums       = [regex|([a-z]|[A-Z]|[0-9])*|]
-- validDotComMail = "${alphaNums}*@${alphaNums}.com"
