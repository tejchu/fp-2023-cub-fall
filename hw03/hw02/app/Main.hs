import Test.Hspec
import BankersDequeTest
import SkewBinomialHeapTest
import BootstrappedHeapTest

main :: IO ()
main = hspec $ do 
    BankersDequeTest.spec
    SkewBinomialHeapTest.spec
    BootstrappedHeapTest.spec 
