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
