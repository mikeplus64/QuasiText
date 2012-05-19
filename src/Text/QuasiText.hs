{-# LANGUAGE ExistentialQuantification, TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
module Text.QuasiText (embed, Chunk (..), getChunks) where
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)

import Data.Attoparsec.Text
import Data.Text as T (Text, pack, unpack, append, empty, head, strip)

instance Lift Text where
    lift t = litE (stringL (unpack t))

data Chunk = 
      T Text
    | E Text
    | V Text
  deriving (Show, Eq)

alter :: (Text -> Text) -> Chunk -> Chunk
alter f (T t) = T $ f t
alter f (E t) = E $ f t
alter f (V t) = V $ f t

class Textish a where
    toText :: a -> Text

instance Textish Text where
    toText = id

instance Textish [Char] where
    toText = pack

instance Show a => Textish a where 
    toText = pack . show

-- | A simple 'QuasiQuoter' to interpolate 'Text' into other pieces of 'Text'. 
-- Expressions can be embedded using $(...) or $..., $... will only work for one-word expressions (best suited for just
-- variable substitution), but $(...) will work for anything..
embed :: QuasiQuoter
embed = QuasiQuoter
    { quoteExp = \s -> 
        let chunks = flip map (getChunks (pack s)) $ \c ->
                    case c of
                        -- literal text
                        T t -> [| t |]

                        -- haskell expression
                        E t -> let Right e = parseExp (unpack t) in appE [| toText |] (return e) 

                        -- one-word expression
                        V t | T.head t `elem` ['a'..'z'] -> appE [| toText |] (global (mkName (unpack t)))
                            | otherwise -> let Right e = parseExp (unpack t) in appE [| toText |] (return e)

        in foldr (\l r -> appE (appE [| append |] l) r) [| empty |] chunks

    , quotePat  = error "cannot use this as a pattern"
    , quoteDec  = error "cannot use this as a declaration"
    , quoteType = error "cannot use this as a type"
    }

-- | Create 'Chunk's without any TH.
getChunks :: Text -> [Chunk]
getChunks i = let Right m = parseOnly parser i in map (alter strip) m
  where
    parser = go []

    go s = do
        txt <- takeTill (== '$')
        evt <- choice [expression, var, fmap T takeText]
        end <- atEnd
        if end
            then return $ filter (not . blank) $ reverse (evt:T txt:s)
            else go (evt:T txt:s)

    blank (T "") = True
    blank (E "") = True
    blank (V "") = True
    blank _      = False

    var = do
        char '$'
        val <- takeTill (notInClass "a-zA-Z0-9_")
        return (V val)

    expression = do
        string "$("
        expr <- takeTill (== ')')
        char ')'
        return (E expr)

