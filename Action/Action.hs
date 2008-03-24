module Action.Action where

data Action = Action deriving (Show)

-- | If @act@ is worth undoing, put it in the undo list and save it to the
-- file.
record :: Action -> IO ()
record act = return ()
