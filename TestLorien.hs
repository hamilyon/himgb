module TestLorien
where

import Lorien
import ReadString
import Test.QuickCheck

import Test.HUnit

propParsesRight :: Bool
propParsesRight = 
    -- do ReadString.readData "test.lorien"
    lorienToPlain ("MIME-Version: 1.0\n" ++
        "Content-Type: multipart/alternative;\n" ++
        "\tboundary=\"----=_NextPart_000_0007_01C983E1.D68DFE80\"\\n" ++
        "X-Priority: 3\n" ++
        "X-MSMail-Priority: Normal\n" ++
        "X-Mailer: Microsoft Outlook Express 6.00.2900.2180\n" ++
        "X-MimeOLE: Produced By Microsoft MimeOLE V6.00.2900.2180\n" ++
        "\n" ++
        "This is a multi-part message in MIME format.\n" ++
        "\n" ++
        "------=_NextPart_000_0007_01C983E1.D68DFE80\n" ++
        "Content-Type: text/plain;\n" ++
        "\tcharset=\"windows-1250\"\n" ++
        "Content-Transfer-Encoding: quoted-printable\n" ++
        "\n" ++
        "\n" ++
        "Enter Here... http://researchedwatches.cn\n" ++
        "\n" ++
        "\n" ++
        "Click to purchase high quality leather goods !\n" ++
        "\n" ++
        "\n" ++
        "- We watch your wallet to ensure that you can afford the watch you like!\n")
    == ("\nEnter Here... http://researchedwatches.cn\n" ++
        "Click to purchase high quality leather goods !\n" ++
        "- We watch your wallet to ensure that you can afford the watch you like!\n")

prop_idempotent_to_plain x = lorienToPlain (lorienToPlain x) == lorienToPlain x

main = 
    quickCheck propParsesRight
    >>
    (runTestTT $ TestList [testEmpty, testParsesRight,
     testSplitsSingleRight, testDetectsIMIME, testStripsMime])

testEmpty = TestCase $ assertEqual 
  "an empty string from an empty string" "" (lorienToPlain "")

testParsesRight = TestCase $ assertEqual
        "Strips excessive headers"

        ("\nEnter Here... http://researchedwatches.cn\n" ++
        "Click to purchase high quality leather goods !\n" ++
        "- We watch your wallet to ensure that you can afford the watch you like!\n")

        (lorienToPlain ("MIME-Version: 1.0\n" ++
        "Content-Type: multipart/alternative;\n" ++
        "\tboundary=\"----=_NextPart_000_0007_01C983E1.D68DFE80\"\\n" ++
        "X-Priority: 3\n" ++
        "X-MSMail-Priority: Normal\n" ++
        "X-Mailer: Microsoft Outlook Express 6.00.2900.2180\n" ++
        "X-MimeOLE: Produced By Microsoft MimeOLE V6.00.2900.2180\n" ++
        "\n" ++
        "This is a multi-part message in MIME format.\n" ++
        "\n" ++
        "------=_NextPart_000_0007_01C983E1.D68DFE80\n" ++
        "Content-Type: text/plain;\n" ++
        "\tcharset=\"windows-1250\"\n" ++
        "Content-Transfer-Encoding: quoted-printable\n" ++
        "\n" ++
        "\n" ++
        "Enter Here... http://researchedwatches.cn\n" ++
        "\n" ++
        "\n" ++
        "Click to purchase high quality leather goods !\n" ++
        "\n" ++
        "\n" ++
        "- We watch your wallet to ensure that you can afford the watch you like!\n"))

testSplitsSingleRight = TestCase $ assertEqual 
        "Splits multipart right on \\n\\n"
        
        ["MIME-Version: 1.0\n" ++
        "Content-Type: multipart/alternative;\n" ++
        "\tboundary=\"----=_NextPart_000_0007_01C983E1.D68DFE80\"\\n" ++
        "X-Priority: 3\n" ++
        "X-MSMail-Priority: Normal\n" ++
        "X-Mailer: Microsoft Outlook Express 6.00.2900.2180\n" ++
        "X-MimeOLE: Produced By Microsoft MimeOLE V6.00.2900.2180",
        "This is a multi-part message in MIME format.\n"]
        
        (splitMultiPart
        ("MIME-Version: 1.0\n" ++
        "Content-Type: multipart/alternative;\n" ++
        "\tboundary=\"----=_NextPart_000_0007_01C983E1.D68DFE80\"\\n" ++
        "X-Priority: 3\n" ++
        "X-MSMail-Priority: Normal\n" ++
        "X-Mailer: Microsoft Outlook Express 6.00.2900.2180\n" ++
        "X-MimeOLE: Produced By Microsoft MimeOLE V6.00.2900.2180\n" ++
        "\n" ++
        "This is a multi-part message in MIME format.\n"))

testDetectsIMIME = TestCase $ assertEqual 
        "mime header getsDetected Ok"
        True
        $
        isHeaders "MIME-Version: 1.0\n"

testStripsMime = TestCase $ assertEqual
        "Mime get dropped correctly"
        []
        $
        dropHeaders
        ["MIME-Version: 1.0\n" ++
        "Content-Type: multipart/alternative;\n"]

