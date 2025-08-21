module FPO.Data.AppToast where

import Prelude

import FPO.Data.AppError (AppError)
import Halogen.HTML as HH

type ToastId = Int
data AppToast
  = Success String
  | Error AppError
  | Warning String
  | Info String

type AppToastWithId = { id :: ToastId, toast :: AppToast }

derive instance Eq AppToast
instance Show AppToast where
  show = case _ of
    Success msg -> msg
    Error err -> show err
    Warning msg -> msg
    Info msg -> msg

classForToast :: AppToast -> HH.ClassName
classForToast = case _ of
  Success _ -> HH.ClassName "fpo-toast-success"
  Error _ -> HH.ClassName "fpo-toast-error"
  Warning _ -> HH.ClassName "fpo-toast-warning"
  Info _ -> HH.ClassName "fpo-toast-info"
