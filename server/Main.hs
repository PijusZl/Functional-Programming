{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text.IO (putStrLn)

main :: IO ()
main = liftIO $ Prelude.putStrLn "Hello"