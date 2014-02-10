module Main where

import          Yesod
import          Yesod.Form.Jquery
import          Yesod.Default.Util

import          Foundation
import          Dispatch

main :: IO ()
main = warpEnv FormApp