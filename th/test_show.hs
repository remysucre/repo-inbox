{-# LANGUAGE TemplateHaskell #-}

import CustomShow

data MyData = MyData
    { foo :: String
    , bar :: Int
    }

listFields ''MyData

main = print $ MyData { foo = "bar", bar = 5 }
