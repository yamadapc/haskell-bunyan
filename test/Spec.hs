{-# LANGUAGE OverloadedStrings #-}
import           System.Log.Bunyan
import           Test.Hspec

main :: IO ()
main = do
    logger <- newLogger def { loggerName = "app"
                            }
    hspec $ do
        describe "debug" $ do
            it "should log things" $ do
                ldebug logger [] "here"
