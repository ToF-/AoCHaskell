import Test.Hspec
import Steps
import Data.Map as M
import Data.List as L

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
                S `M.lookup` (predList (succList large)) `shouldBe` Just [B,D,E,F,H,I,J,K,P,Q,R,T,X,Z]

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

    describe "a predecessor count list" $ do
        it "tells the number of predecessors for each step of a succ list" $ do
            let pc = predCount (succList small)
            M.toList pc `shouldBe`[(A,1),(B,1),(C,0),(D,1),(E,3),(F,1)]
            let pc = predCount (succList large)
            M.toList pc `shouldBe` 
                [(A,2),(B,0),(C,8),(D,4),(E,0),(F,2),(G,3),(H,4),(I,8),(J,5),(K,7),(L,2),(M,3)
                ,(N,1),(O,5),(P,3),(Q,6),(R,4),(S,14),(T,1),(U,0),(V,0),(W,1),(X,3),(Y,8),(Z,7)]

    describe "available steps" $ do
        it "tells which steps has their pred count at 0" $ do
            availableSteps (predCount (succList small)) `shouldBe` [C]
            availableSteps (predCount (succList large)) `shouldBe` [B,E,U,V]

    describe "decrease Pred count" $ do
        it "decreases the predecessor count of a step" $ do
            let pc = decreasePredCount (predCount (succList small)) F
            M.toList pc `shouldBe`[(A,1),(B,1),(C,0),(D,1),(E,3),(F,0)]

    describe "workers"  $ do
        it "can be free of assignment or assigned a job for a step ending at a given time" $ do
            Job 3 C `shouldBe` Job 3 C
        it "can be ordered by job time, free workers are at the end of this sort" $ do
            L.sort [Free,Job 10 C, Job 3 F] `shouldBe` [Job 3 F, Job 10 C, Free]
        it "can finish their job and be free again" $ do
            finishNextJob [Free,Free]  `shouldBe` (Free,[Free,Free])
            finishNextJob [Free,Job 3 C]  `shouldBe` (Job 3 C,[Free,Free])
            finishNextJob [Job 10 F,Job 3 C]  `shouldBe` (Job 3 C,[Job 10 F,Free])
            finishNextJob [Job 3 F,Job 3 C]  `shouldBe` (Job 3 C,[Job 3 F,Free])

    describe "a schedule" $ do
        describe "can be created for a given number of workers, a base duration, and a list of edges" $ do
            let sch = schedule 2 0 small
            let big = schedule 5 60 large
            it "is not done as long as the last step is not done" $ do
                done sch `shouldBe` False
            it "has a time"  $ do
                time sch `shouldBe` 0
            it "can find next steps to be working on" $ do
                nextSteps sch `shouldBe` []
                let sch' = next sch
                nextSteps sch' `shouldBe` [C]
                nextSteps (next big) `shouldBe` [B,E,U,V] 
            describe "can assign the next step" $ do
                it "to a worker" $ do
                    workers sch `shouldBe` [Free,Free]
                    let sch' = assign (next sch)
                    workers sch' `shouldBe` [Job 3 C,Free] 
                    nextSteps sch' `shouldBe` []
                it "if there are next steps" $ do
                    workers (assign sch) `shouldBe` [Free,Free]
                it "if there are free workers" $ do
                    let sch' = assign (next ( sch { workers = [Job 10 F, Job 3 C] }))
                    workers sch'  `shouldBe` [Job 10 F,Job 3 C]
            
