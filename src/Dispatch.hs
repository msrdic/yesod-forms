module Dispatch where

import Yesod

import Foundation

import Handler.Home
import Handler.Person
import Handler.Inline
import Handler.Horiz

mkYesodDispatch "FormApp" resourcesFormApp