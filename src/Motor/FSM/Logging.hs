{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple MonadIO-based logging for 'MonadFSM' programs.
module Motor.FSM.Logging where

import           Control.Monad.IO.Class
import           Data.OpenRecords
import           Data.Proxy
import           Data.Reflection

import           Motor.FSM

log ::
     (MonadIO (m i i), KnownSymbol n, Reifies n String)
  => Name n
  -> String
  -> m (i :: Row *) (i :: Row *) ()
log (_ :: Name n) s = liftIO (putStrLn (prefix ++ ": " ++ s))
  where prefix = reflect (Proxy :: Proxy n)
