{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost (doneForm 1)
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
        where tasks = [] :: [] String

postHomeR :: Handler RepHtml
postHomeR = do
    {-(formWidget, formEnctype) <- generateFormPost (doneForm 1)-}
    ((result, formWidget), formEnctype) <- runFormPost (doneForm 1)
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
        where tasks = [] :: [] String

text = id
taskTaskId x = 1

doneForm :: Integer -> Form (String, Bool)
doneForm taskId = renderDivs $ (,)
    <$> areq hiddenField "" (Just (show taskId))
    <*> areq checkBoxField "Done?" (Just True)

sampleForm :: Form (String, Bool)
sampleForm = renderDivs $ (,)
    <$> areq hiddenField "" (Just "1")
    <*> areq checkBoxField "Done?" (Just True)








































