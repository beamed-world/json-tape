{-# LANGUAGE OverloadedStrings #-}

import Codec.JsonTape
import Data.ByteString.Base64.Type
import Prelude hiding (readFile, writeFile)
import Test.HUnit.Base
import Test.HUnit.Text
import System.Directory.Tree (DirTree(..))
import System.Exit

main :: IO Counts
main = do
    res <- runTestTT $ TestList [testEncode, testDecodeOk, testDecodeFail]
    exitWith $
        case (failures res + errors res) of
            0 -> ExitSuccess
            n -> ExitFailure n

testEncode :: Test
testEncode = TestCase $ assertEqual "encoded tree" (encodeFiles sampleTree) sampleJson

testDecodeOk :: Test
testDecodeOk =
    TestCase $
    assertEqual "decoded tree" (decodeFiles sampleJson) (Right sampleTree)

testDecodeFail :: Test
testDecodeFail =
    TestCase $
    assertEqual
        "decoded tree"
        (decodeFiles sampleJsonBadKind)
        (Left "Error in $: unknown kind: badkind")

sampleTree :: Object
sampleTree =
    Dir
    { name = "antigravity-0.1"
    , contents =
        [ File
          { name = "PKG-INFO"
          , file =
              ByteString64
              { getByteString64 =
                  "Metadata-Version: 1.0\r\nName: antigravity\r\nVersion: 0.1\r\nSummary: A really simple module that allow everyone to do \"import antigravity\"\r\nHome-page: http://fabien.schwob.org/antigravity/\r\nAuthor: Fabien Schwob\r\nAuthor-email: antigravity@x-phuture.com\r\nLicense: UNKNOWN\r\nDescription: UNKNOWN\r\nPlatform: UNKNOWN\r\n"
              }
          }
        , File
          { name = "setup.py"
          , file =
              ByteString64
              { getByteString64 =
                  "#!/usr/bin/env python\r\n\r\nfrom distutils.core import setup\r\n\r\nsetup(\r\n    name='antigravity',\r\n    version='0.1',\r\n    description='A really simple module that allow everyone to do \"import antigravity\"',\r\n    author='Fabien Schwob',\r\n    author_email='antigravity@x-phuture.com',\r\n    url='http://fabien.schwob.org/antigravity/',\r\n    packages=['antigravity'],\r\n)"
              }
          }
        , Dir
          { name = "antigravity"
          , contents =
              [ File
                { name = "__init__.py"
                , file =
                    ByteString64
                    { getByteString64 =
                        "#!/usr/bin/env python\r\n\r\nSTRIP_URL = \"http://xkcd.com/353/\"\r\n\r\ndef start():\r\n    return STRIP_URL"
                    }
                }]
          }]
    }

sampleJson :: ByteString
sampleJson =
    "{\"kind\":\"directory\",\"content\":[{\"kind\":\"file\",\"content\":\"TWV0YWRhdGEtVmVyc2lvbjogMS4wDQpOYW1lOiBhbnRpZ3Jhdml0eQ0KVmVyc2lvbjogMC4xDQpTdW1tYXJ5OiBBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSINCkhvbWUtcGFnZTogaHR0cDovL2ZhYmllbi5zY2h3b2Iub3JnL2FudGlncmF2aXR5Lw0KQXV0aG9yOiBGYWJpZW4gU2Nod29iDQpBdXRob3ItZW1haWw6IGFudGlncmF2aXR5QHgtcGh1dHVyZS5jb20NCkxpY2Vuc2U6IFVOS05PV04NCkRlc2NyaXB0aW9uOiBVTktOT1dODQpQbGF0Zm9ybTogVU5LTk9XTg0K\",\"name\":\"PKG-INFO\"},{\"kind\":\"file\",\"content\":\"IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNCmZyb20gZGlzdHV0aWxzLmNvcmUgaW1wb3J0IHNldHVwDQoNCnNldHVwKA0KICAgIG5hbWU9J2FudGlncmF2aXR5JywNCiAgICB2ZXJzaW9uPScwLjEnLA0KICAgIGRlc2NyaXB0aW9uPSdBIHJlYWxseSBzaW1wbGUgbW9kdWxlIHRoYXQgYWxsb3cgZXZlcnlvbmUgdG8gZG8gImltcG9ydCBhbnRpZ3Jhdml0eSInLA0KICAgIGF1dGhvcj0nRmFiaWVuIFNjaHdvYicsDQogICAgYXV0aG9yX2VtYWlsPSdhbnRpZ3Jhdml0eUB4LXBodXR1cmUuY29tJywNCiAgICB1cmw9J2h0dHA6Ly9mYWJpZW4uc2Nod29iLm9yZy9hbnRpZ3Jhdml0eS8nLA0KICAgIHBhY2thZ2VzPVsnYW50aWdyYXZpdHknXSwNCik=\",\"name\":\"setup.py\"},{\"kind\":\"directory\",\"content\":[{\"kind\":\"file\",\"content\":\"IyEvdXNyL2Jpbi9lbnYgcHl0aG9uDQoNClNUUklQX1VSTCA9ICJodHRwOi8veGtjZC5jb20vMzUzLyINCg0KZGVmIHN0YXJ0KCk6DQogICAgcmV0dXJuIFNUUklQX1VSTA==\",\"name\":\"__init__.py\"}],\"name\":\"antigravity\"}],\"name\":\"antigravity-0.1\"}"

sampleJsonBadKind :: ByteString
sampleJsonBadKind =
    "{\"kind\":\"badkind\",\"content\":\"bar\",\"name\":\"foo\"}"
