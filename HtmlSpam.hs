module HtmlSpam
where 

import Spam

data HiperText = 
    HiperText {
        childs :: [(HiperText, String)],
        effectiveAttr :: HiperTextAttr
    }

data HiperTextAttr = 
    HiperTextAttr {
        style :: HiperTextStyle,
        href :: Maybe HiperTextHref
    }

data HiperTextStyle = HiperTextStyle (String, String) -- key-value
data HiperTextHref = HiperTextHref (String)

data Stem = Stem {
    contextTags :: Boolean
}
