{-#  LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
module HelloWorld3 where
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    firstName String
    lastName String
    age Int Maybe
    created UTCTime default=CURRENT_TIME
    language String default='Haskell'
    UniqueName firstName lastName
    deriving Show
|]

{-main = (withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration migrateAll
    insert $ Person "Michael" "Snoyman" (Just 26) Nothing
    michael <- getBy $ UniqueName "Michael" "Snoyman"
    liftIO $ print $ michael) :: (Control.Monad.Trans.Control.MonadBaseControl IO IO) => IO ()-}

