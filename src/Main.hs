module Main where

import          Yesod
import          Yesod.Form.Jquery

import          Foundation
import          Dispatch

main :: IO ()
main = warpEnv FormApp