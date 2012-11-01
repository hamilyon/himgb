{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Handler.Models

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

tasks :: [Task]
tasks = [Task 1 "Написать на yesod todo app" False,
         Task 2 "Почитать hacker news" True]

doneForm :: Task -> Form Task
doneForm task = renderDivs $ Task
    <$> areq hiddenField "" (Just (tId task))
    <*> areq hiddenField "" (Just (text task))
    <*> areq checkBoxField "" (Just (done task))






































