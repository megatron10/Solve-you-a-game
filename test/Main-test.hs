import TTTtest
import Test.HUnit

main = do
    runTestTT testBaseCase
    runTestTT testInitPosn
    return ()
