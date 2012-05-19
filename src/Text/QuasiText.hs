{-# LANGUAGE ExistentialQuantification, TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
module Text.QuasiText (embed, Chunk (..), getChunks) where
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Attoparsec.Text hiding (take)
import Data.Text (Text, pack, unpack)
import Language.Haskell.Meta (parseExp)

data Chunk = 
      T Text
    | E Text
    | V Text
  deriving (Show, Eq)

class Stringy a where
    toString :: a -> String

instance Stringy Text where
    toString = unpack

instance Stringy [Char] where
    toString x = x

instance Show a => Stringy a where 
    toString = show

-- | A simple 'QuasiQuoter' to interpolate 'Text' into other pieces of 'Text'. 
-- Escape variables using $var, expressions in $(expression)
-- Expressions that are not 'Text' must be directly convertable to it, ie be 'String', or have an instance of 'Show'.
embed :: QuasiQuoter
embed = QuasiQuoter
    { quoteExp = \s -> 
        let chunks = 
                flip map (getChunks (pack s)) $ \c ->
                    case c of
                        T t -> let str = unpack t in [| str |]
                        E t -> let str      = unpack t
                                   Right e  = parseExp str
                                in appE [| toString |] (return e)
                        V t -> appE [| toString |] $ global $ mkName $ unpack t
            unpacked = foldr (\l r -> appE (appE [| (++) |] l) r) [| [] |] chunks
        in appE [| pack |] unpacked
    , quotePat  = error "cannot use this as a pattern"
    , quoteDec  = error "cannot use this as a declaration"
    , quoteType = error "cannot use this as a type"
    }

-- | Create 'Chunk's without any TH.
getChunks :: Text -> [Chunk]
getChunks i = let Right m = parseOnly parser i in m
  where
    parser = go []
    go s = do
        txt <- takeTill (== '$')
        evt <- choice [expression, var, fmap T takeText]
        end <- atEnd
        if end
            then return $ filter (not . empty) $ reverse (evt:T txt:s)
            else go (evt:T txt:s)

    empty (T "") = True
    empty (E "") = True
    empty (V "") = True
    empty _      = False

    var = do
        char '$'
        val <- takeTill (== ' ')
        return (V val)

    expression = do
        string "$("
        expr <- takeTill (== ')')
        char ')'
        return (E expr)
