import TTTtest as TTT
import FPtest as FP
import CHOMPtest as CT
import Test.HUnit

main = do
    runTestTT TTT.testBaseCase
    runTestTT TTT.testInitPosn
    runTestTT FP.testBaseCase
    runTestTT FP.testInitPosn
    runTestTT CT.testDoMove
    return ()
