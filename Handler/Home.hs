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
            Nothing -> (fakeClientIdentity )
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

fakeClientIdentity :: ClientIdentityId
fakeClientIdentity = Key $ PersistText "1"

postHomeR :: Handler RepHtml
postHomeR = do
    resultWidgetEnctype  <- runFormPost (defaultDoneForm Nothing Nothing Nothing)
    let ((formResult, _), _) = resultWidgetEnctype
    case formResult of
        FormSuccess taskFormData -> do
            liftIO $ print $ tId taskFormData
            runDB $ insert $ fromTaskFormData taskFormData
            setMessageI $ MsgTaskCreated
            redirect $ HomeR
            {-$ text taskFormData-}
        _ -> defaultLayout $ do
            setTitleI MsgTaskNotValid
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "stub")
    {-let forms = doneForm `fmap` tasks-}
    {-let results = map (fst. fst) widgetAndEnctypes-}
    {-let result = results !! 0-}
    {-let handlerName = "postHomeR" :: Text-}
        {-submission = case result of-}
            {-FormSuccess res -> Just res-}
            {-_ -> Nothing-}
    {-defaultLayout $ do-}
        {-setTitle "Welcome To Yesod!"-}
        {-$(widgetFile "homepage")-}

fromTaskFormData :: TaskFormData -> Task
fromTaskFormData (TaskFormData u t d) = Task fakeClientIdentity (unpack t) d

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
doneForm taskEntity =  defaultDoneForm
        (Just (unpack $ toPathPiece $ unKey $ entityKey $ taskEntity))
        (Just (pack $ taskText task))
        (Just (taskDone task))
    where task = entityVal taskEntity

defaultDoneForm :: Maybe String -> Maybe Text -> Maybe Bool -> Form TaskFormData
defaultDoneForm id text done = renderDivs $ TaskFormData
    <$> areq hiddenField "" id
    <*> areq textField (FieldSettings "" Nothing Nothing Nothing [("a","b")]) text
    <*> areq checkBoxField "" done






































