{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Handler.Models

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost (doneForm (tasks !! 0))
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    {-(formWidget, formEnctype) <- generateFormPost (doneForm 1)-}
    ((result, formWidget), formEnctype) <- runFormPost (doneForm (tasks !! 0))
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

tasks :: [Task]
tasks = [Task 1 "Написать на yesod todo app" False,
         Task 2 "Почитать hacker news" True]

doneForm :: Task -> Form Task
doneForm task = renderDivs $ Task
    <$> areq hiddenField "" (Just (tId task))
    <*> areq hiddenField "" (Just (text task))
    <*> areq checkBoxField "" (Just (done task))






































