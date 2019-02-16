import Test.Hspec
import Steps
import Data.Map as M

small = [(C,A) ,(C,F) ,(A,B) ,(A,D) ,(B,E) ,(D,E) ,(F,E)]

--       -->A--->B--
--      /    \      \
--     C      -->D----->E
--      \           /
--       ---->F-----

tiny = [(Q,Y) ,(Q,C) ,(Q,I) ,(I,Y) ,(I,S) ,(C,Y) ,(S,Y)]

--      -->I--->S--
--     /    \      \
--    Q------------>Y
--     \           /
--      ---->C-----


large=
    [(V,H) ,(U,R) ,(E,D) ,(B,R) ,(W,X) ,(A,P) ,(T,L) ,(F,C) ,(P,Y) ,(N,G) ,(R,S) ,(D,C) ,(O,K) ,(L,J) ,(J,H) ,(M,I) ,(G,K) ,(Z,Q)
    ,(X,Q) ,(H,I) ,(K,Y) ,(Q,S) ,(I,Y) ,(S,Y) ,(C,Y) ,(T,S) ,(P,S) ,(I,S) ,(V,O) ,(O,Q) ,(T,R) ,(E,J) ,(F,S) ,(O,H) ,(Z,S) ,(D,Z)
    ,(F,K) ,(W,P) ,(G,I) ,(B,T) ,(G,Y) ,(X,S) ,(B,K) ,(V,A) ,(U,N) ,(T,P) ,(V,D) ,(G,X) ,(B,D) ,(R,J) ,(M,Z) ,(U,Z) ,(U,G) ,(A,C)
    ,(H,Q) ,(X,K) ,(B,S) ,(Q,C) ,(Q,Y) ,(R,I) ,(V,Q) ,(A,D) ,(D,S) ,(K,S) ,(G,C) ,(D,O) ,(R,H) ,(K,Q) ,(W,R) ,(H,Y) ,(P,J) ,(N,Z)
    ,(J,K) ,(W,M) ,(A,Z) ,(V,W) ,(J,X) ,(U,F) ,(P,L) ,(W,G) ,(T,F) ,(R,C) ,(R,O) ,(Z,C) ,(E,S) ,(L,I) ,(U,O) ,(W,K) ,(K,I) ,(O,M)
    ,(V,M) ,(V,Z) ,(A,I) ,(F,J) ,(F,O) ,(M,C) ,(Q,I) ,(H,S) ,(U,A) ,(J,S) ,(P,Z)] 

--    B --- ...
--    E---- ...
--    U---- ...
--    V---- ...


main = hspec $ do
    describe "a succ list" $ do
        it "tells what are the successors of a step" $ do
            let sl = succList small
            C `M.lookup` sl `shouldBe` Just [A,F]
            E `M.lookup` sl `shouldBe` Nothing
            True `shouldBe` True 

        describe "a pred list" $ do
            it "tells what are the predecessors of a step from a succ list" $ do
                let pl = predList (succList small)
                E `M.lookup` pl `shouldBe` Just [B,D,F]
                C `M.lookup` pl `shouldBe` Nothing

        describe "start steps" $ do
            it "tells what are the starting steps of the list" $ do
                startSteps (succList small) `shouldBe` [C]

        describe "end step" $ do
            it "tells what is the ending step of the list" $ do
                endStep (succList small)  `shouldBe` E

        describe "execute" $ do
            it "tells which steps to execute given a successor list" $ do
                (execute (succList small))  `shouldBe` [C,A,B,D,F,E]
                concatMap show (execute (succList large))  `shouldBe` "BETUFNVADWGPLRJOHMXKZQCISY"

    describe "a job" $ do
        it "can be a working step for a given time" $ do
            step (Job A 42) `shouldBe` Just A

        it "can be Idle for a given time, then it's not a step" $ do
            step (Idle 17) `shouldBe` Nothing

        it "has a duration" $ do
            duration (Job A 42) `shouldBe` 42
            duration (Idle 17)  `shouldBe` 17

    describe "a worker" $ do
        it "has jobs (steps or idle) thus a time" $ do
            time []  `shouldBe` 0
            time [Job A 42,Idle 17] `shouldBe` 59

        it "has steps done" $ do
            stepsDone [] `shouldBe` []
            stepsDone [Job F 6,Job C 3] `shouldBe`  [F,C]

        it "has steps done at a given time" $ do
            stepsDoneAt 0 [] `shouldBe` []
            stepsDoneAt 1 [Job F 6,Job C 3] `shouldBe`  []
            stepsDoneAt 3 [Job F 6,Job C 3] `shouldBe`  [C]
            stepsDoneAt 7 [Job F 6,Job C 3] `shouldBe`  [C]
            stepsDoneAt 9 [Job F 6,Job C 3] `shouldBe`  [F,C]

        it "can be asked to wait until a given time" $ do
            wait 42 [] `shouldBe` [Idle 42]
            wait 9 [Job C 3] `shouldBe` [Idle 6, Job C 3]
            wait 4  [Idle 17] `shouldBe` [Idle 17]
