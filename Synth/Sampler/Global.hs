module Global (
    module Global, Text, (<>), fromMaybe, mapMaybe
    , module Control.Monad
) where
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.Conduit.Audio as Audio
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)

-- | Time in absolute seconds since the start of the score.
type Time = Double

errorIO :: Trans.MonadIO m => Text -> m a
errorIO = Trans.liftIO . Exception.throwIO . Exception.ErrorCall . Text.unpack

type Audio = Audio.AudioSource (Resource.ResourceT IO) Float

txt :: String -> Text
txt = Text.pack

untxt :: Text -> String
untxt = Text.unpack

showt :: Show a => a -> Text
showt = txt . show

-- | This is like the hackage bifunctor package, but with no extra
-- dependencies, and no clowns.
class Bifunctor p where
    (***) :: (a -> b) -> (c -> d) -> p a c -> p b d
    first :: (a -> b) -> p a c -> p b c
    second :: (c -> d) -> p a c -> p a d
infixr 3 ***

instance Bifunctor (,) where
    f *** g = \(x, y) -> (f x, g y)
    first f (a, c) = (f a, c)
    second f (c, a) = (c, f a)

instance Bifunctor Either where
    f *** g = either (Left . f) (Right . g)
    first f = either (Left . f) Right
    second f = either Left (Right . f)
