module SemLog.Test.NameTest where

import Hedgehog (TestT, (===))

import SemLog (name)

test_name :: TestT IO ()
test_name = "sem-log" === name
