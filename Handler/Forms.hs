module Handler.Forms where

import Import
import Handler.Models

doneForm :: Task -> Form Task
doneForm task = renderDivs $ Task
    <$> areq hiddenField "" (Just (tId task))
    <*> areq hiddenField "" (Just (text task))
    <*> areq checkBoxField "" (Just (done task))

