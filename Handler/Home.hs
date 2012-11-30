{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes #-}
module Handler.Home where
import Database.Persist.GenericSql (runSqlConn, runMigration, SqlPersist)
import Database.Persist.Store
import Data.Text (pack, unpack)
import Import

{-import Database.Persist.Sqlite-}
    {-( ConnectionPool, SqlPersist, runSqlPool, runMigration-}
    {-, createSqlitePool-}
    {-)-}

prip :: (a,b) -> c -> (a,b,c)
prip (a,b) c = (a,b,c)
fs :: forall t t1 t2. (t, t1, t2) -> t
fs (a,_,_) = a
sn :: forall t t1 t2. (t, t1, t2) -> t1
sn (_,b,_) = b
thd :: forall t t1 t2. (t, t1, t2) -> t2
thd (_,_,c) = c

getHomeR :: Handler RepHtml
getHomeR = do
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text

    mauth <- maybeAuth
    let userId = case mauth of
            Nothing -> (Key $ PersistText "1" :: ClientIdentityId)
            Just (Entity clientIdentityId clientIdentity) -> clientIdentityId

    {-taskEntId <- runDB $ insert $ Task userId "what" False-}
    taskEntities <- runDB $ selectList [TaskUser ==. userId] []
    let tasksFromDb = map entityVal taskEntities

    let tasks = map toTaskFormData tasksFromDb
    let forms = doneForm `fmap` taskEntities
    widgetAndEnctypes <- mapM generateFormPost forms
    let all = zipWith prip widgetAndEnctypes tasks
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")


postHomeR :: Handler RepHtml
postHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "stub")
    {-[>(formWidget, formEnctype) <- generateFormPost (doneForm 1)<]-}
    {-let forms = doneForm `fmap` tasks-}
    {-widgetAndEnctypes <- mapM runFormPost forms-}
    {-let results = map (fst. fst) widgetAndEnctypes-}
    {-let result = results !! 0-}
    {-let handlerName = "postHomeR" :: Text-}
        {-submission = case result of-}
            {-FormSuccess res -> Just res-}
            {-_ -> Nothing-}
    {-defaultLayout $ do-}
        {-setTitle "Welcome To Yesod!"-}
        {-$(widgetFile "homepage")-}

client :: ClientIdentityId
client = undefined

data TaskFormData = TaskFormData {
  tId :: String,
  text :: Text,
  done :: Bool
}

{-tasks :: [TaskFormData]-}
{-tasks = [TaskFormData "1" "Написать на yesod todo app" False,-}
         {-TaskFormData "2" "Почитать hacker news" True]-}

toTaskFormData :: Task -> TaskFormData
toTaskFormData (Task user text_ done_) = TaskFormData (show $ unKey user) (pack $ text_) done_

doneForm :: Entity Task -> Form TaskFormData
doneForm taskEntity = renderDivs $ TaskFormData
    <$> areq hiddenField "" (Just (show $ unKey $ entityKey $ taskEntity))
    <*> areq textField (FieldSettings "" Nothing Nothing Nothing [("a","b")]) (Just (pack $ taskText task))
    <*> areq checkBoxField "" (Just (taskDone task))
    where task = entityVal taskEntity






































