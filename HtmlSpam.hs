module HtmlSpam
where 

import Spam

data HiperText = 
    HiperText {
        childs :: [(HiperText, String)],
        effectiveAttr :: Maybe HiperTextAttr
    } deriving Show

data HiperTextAttr = 
    HiperTextAttr {
        style :: HiperTextStyle,
        href :: Maybe HiperTextHref
    } deriving Show

data HiperTextStyle = HiperTextStyle (String, String) -- key-value
    deriving Show
data HiperTextHref = HiperTextHref (String)
    deriving Show
data Stem = Stem {
    contextTags :: Bool
}

data Action = Cut (HiperText) 
    deriving Show
