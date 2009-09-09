module Cmd.ControlTrack where
import Control.Monad
import qualified Data.Maybe as Maybe

import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil


cmd_raw_edit :: Cmd.Cmd
cmd_raw_edit = cmd_val_edit

cmd_val_edit :: Cmd.Cmd
cmd_val_edit msg = do
    key <- EditUtil.alpha_key msg
    modify_event $ -- deleting past "" on the val deletes the event
        \(method, val) -> case EditUtil.modify_text_key key val of
            Nothing -> (Nothing, Nothing)
            Just new_val -> (Just method, Just new_val)
    return Cmd.Done

cmd_method_edit :: Cmd.Cmd
cmd_method_edit msg = do
    key <- EditUtil.alpha_key msg
    modify_event $
        \(method, val) -> (EditUtil.modify_text_key key method, Just val)
    return Cmd.Done

-- * implementation

modify_event :: (Monad m) => ((String, String) -> (Maybe String, Maybe String))
    -> Cmd.CmdT m ()
modify_event f = EditUtil.modify_event True (unparse . f . parse)

parse :: String -> (String, String)
parse s
    | null post = ("", pre)
    | otherwise = (pre, tail post)
    where
    (pre, post) = break (==',') s

unparse :: (Maybe String, Maybe String) -> Maybe String
unparse (method, val)
    | null pre && null post = Nothing
    | null pre = Just post
    | otherwise = Just (pre ++ ',' : post)
    where
    pre = Maybe.fromMaybe "" method
    post = Maybe.fromMaybe "" val
