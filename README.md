# Interpolated string QuasiQuoter for Text values.

For example:

~~~{.haskell}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import qualified Data.Text.IO as T
import Text.QuasiText

main :: IO ()
main = do
    user  <- T.getLine
    email <- T.getLine
    airSpeedVelocityOfAnUnladenSwallow <- fmap read getLine :: IO Int
    T.putStrLn [embed|
The air speed velocity of an unladen swallow is $airSpeedVelocityOfAnUnladenSwallow.
Your username is $user.
Your email is $email.
1 + 1 = $(1 + 1). |]
~~~

Monadic version:

~~~{.haskell}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import qualified Data.Text.IO as T
import Text.QuasiText

main :: IO ()
main = do
    let user = T.getLine
    let email = T.getLine
    let airSpeedVelocityOfAnUnladenSwallow = fmap read getLine :: IO Int
    s <- [embedM|
The air speed velocity of an unladen swallow is $airSpeedVelocityOfAnUnladenSwallow. 
Your username is $user. 
Your email is $email. 
1 + 1 = $(return (1 + 1)). |]
    T.putStrLn s
~~~

