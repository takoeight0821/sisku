module Sisku.App where

import Control.Lens (to, view)
import Relude
import Sisku.Config

newtype SiskuApp a = SiskuApp {unSiskuApp :: ReaderT Config IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runSiskuApp :: Config -> SiskuApp a -> IO a
runSiskuApp config (SiskuApp m) = runReaderT m config

class Monad m => MonadSiskuApp m where
  getConfig :: m Config
  getLspSettingMap :: m (Map Text LspSetting)

instance MonadSiskuApp SiskuApp where
  getConfig = SiskuApp ask
  getLspSettingMap = SiskuApp (view $ lspSettingMap . to unwrapLspSettingMap)

instance MonadSiskuApp m => MonadSiskuApp (ReaderT r m) where
  getConfig = lift getConfig
  getLspSettingMap = lift getLspSettingMap

instance MonadSiskuApp m => MonadSiskuApp (ExceptT e m) where
  getConfig = lift getConfig
  getLspSettingMap = lift getLspSettingMap

instance MonadSiskuApp m => MonadSiskuApp (StateT s m) where
  getConfig = lift getConfig
  getLspSettingMap = lift getLspSettingMap