import Test.Hspec
import Sleep
import Data.Map as M

main = hspec $ do
    describe "guard" $ do
        it " creates 60 minutes with 0 sleep" $ do
            guard  `shouldBe` replicate 60 Awake

    describe "sleepAt" $ do
        it "creates minutes of sleep from a given minute" $ do
            sleepAt 34 guard `shouldBe` 
                replicate 34 Awake ++ replicate 26 Asleep
        
    describe "wake" $ do
        it "creates minutes of wake from a given minute" $ do
            wakeAt 42 (sleepAt 34 guard) `shouldBe` 
                replicate 34 Awake ++ 
                replicate 8 Asleep ++ 
                replicate 18 Awake 

    describe "total time asleep" $ do
        it "sum the total numbers of minutes asleep" $ do
            let t = [wakeAt 42 (sleepAt 34 guard) 
                    ,sleepAt 30 guard]
            totalTimeAsleep t `shouldBe` 38

    describe "sleep minutes" $ do
        let t = [wakeAt 42 (sleepAt 34 guard) 
                ,sleepAt 30 guard
                ,wakeAt 7 (sleepAt 5 guard)]
        it "sum the sleep time per minute" $ do
            sleepPerMinute t `shouldBe`
                replicate 5 0 ++ 
                replicate 2 1 ++ 
                replicate 23 0 ++ 
                replicate 4 1 ++ 
                replicate 8 2 ++ 
                replicate 18 1
        it "max sleep minute" $ do
            maxSleepMinute t  `shouldBe` 34

    describe "collect events" $ do
        it "collect events about sleep patterns" $ do
            let evts = [(1518,11,01,00,00, Guard 10)
                       ,(1518,11,01,00,05, Sleep)
                       ,(1518,11,01,00,25, Wake)
                       ,(1518,11,01,00,30, Sleep)
                       ,(1518,11,01,00,55, Wake)
                       ,(1518,11,01,23,58, Guard 99)
                       ,(1518,11,02,00,40, Sleep)
                       ,(1518,11,02,00,50, Wake)
                       ,(1518,11,03,00,05, Guard 10)
                       ,(1518,11,03,00,24, Sleep)
                       ,(1518,11,03,00,29, Wake)
                       ,(1518,11,04,00,02, Guard 99)
                       ,(1518,11,04,00,36, Sleep)
                       ,(1518,11,04,00,46, Wake)
                       ,(1518,11,05,00,03, Guard 99)
                       ,(1518,11,05,00,45, Sleep)
                       ,(1518,11,05,00,55, Wake)]
            M.toList (M.map totalTimeAsleep (collect evts))
                  `shouldBe` [(10,50),(99,30)]
            M.toList (M.map maxSleepMinute (collect evts))
                  `shouldBe` [(10,24),(99,45)]
                

                   
