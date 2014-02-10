module Dispatch where

import Yesod

import Foundation

import Handler.Home
import Handler.Person

mkYesodDispatch "FormApp" resourcesFormApp