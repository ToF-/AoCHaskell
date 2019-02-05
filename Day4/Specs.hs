import Test.Hspec
import Sleep

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

                   
