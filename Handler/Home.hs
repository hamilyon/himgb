{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where
import Database.Persist.GenericSql (runSqlConn, runMigration, SqlPersist)

import Import

prip :: (a,b) -> c -> (a,b,c)
prip (a,b) c = (a,b,c)

fs (a,b,c) = a
sn (a,b,c) = b
thd (a,b,c) = c

getHomeR :: Handler RepHtml
getHomeR = do
    let forms = doneForm `fmap` tasks
    widgetAndEnctypes <- mapM generateFormPost forms
    let all = zipWith prip widgetAndEnctypes tasks
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
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
  text :: String,
  done :: Bool
}

tasks :: [TaskFormData]
tasks = [TaskFormData "1" "Написать на yesod todo app" False,
         TaskFormData "2" "Почитать hacker news" True]

doneForm :: TaskFormData -> Form TaskFormData
doneForm task = renderDivs $ TaskFormData
    <$> areq hiddenField "" (Just (tId task))
    <*> areq hiddenField "" (Just (text task))
    <*> areq checkBoxField "" (Just (done task))






































