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
