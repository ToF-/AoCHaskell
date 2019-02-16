import Test.Hspec
import Steps
import Data.Map as M
import Data.List as L
import Data.Ord
import Data.Maybe

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
    describe "pred list" $ do
        it "creates a list of all predecessors of all steps" $ do
            (sort (M.toList (predList small))) `shouldBe` [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[C])]
            (sort (M.toList (predList tiny))) `shouldBe` [(C,[Q]),(I,[Q]),(S,[I]),(Y,[C,I,Q,S])]

    describe "succ list" $ do
        it "creates a list of all successors of all steps" $ do
            (sort (M.toList (succList small))) `shouldBe` [(A,[B,D]),(B,[E]),(C,[A,F]),(D,[E]),(F,[E])]
            (sort (M.toList (succList tiny))) `shouldBe` [(C,[Y]),(I,[S,Y]),(Q,[C,I,Y]),(S,[Y])]

    describe "start step" $ do
        it "tells wich steps are starting steps from succ list" $ do
            startSteps (succList small)  `shouldBe` [C]
            startSteps (succList tiny)  `shouldBe` [Q]
            startSteps (succList large)  `shouldBe` [B,E,U,V]

    describe "steps" $ do
        it "tells which steps to execute in which order" $ do
            concatMap show (steps small)  `shouldBe` "CABDFE"
            concatMap show (steps tiny)   `shouldBe` "QCISY"
            concatMap show (steps large)   `shouldBe` "BETUFNVADWGPLRJOHMXKZQCISY"

    describe "schedule" $ do
        let scSmall = schedule 3 0 small
            scTiny  = schedule 2 1000 tiny
            scLarge = schedule 5 60 large
            scSmallA = assignStep scSmall A 
            scSmallF = assignStep scSmallA F
            scSmallB = assignStep scSmallF B
            scSmallD = assignStep scSmallB D
            scTinyC = assignStep scTiny C 
        it "has a number of todo list" $ do
            jobs scSmall `shouldBe` [[],[],[]]

        describe "critical path time" $ do
            it "tells the minimum time to completion from a given step" $ do
                (sort (M.toList (criticalPathTime scSmall)))
                    `shouldBe` [(A,10),(B,7),(C,14),(D,9),(E,5),(F,11)]
                (sort (M.toList (criticalPathTime scTiny))) 
                    `shouldBe` [(C,2028),(I,3053),(Q,4070),(S,2044),(Y,1025)]
      
        describe "assign step" $ do
            it "places a step with its duration in the todo list with the smallest total work" $ do
                jobs scSmallA `shouldBe` [[Job A 1],[],[]]
                jobs scTinyC  `shouldBe` [[Job C 1003],[]]
                jobs scSmallF `shouldBe` [[Job A 1],[Job F 6],[]]
                jobs scSmallB `shouldBe` [[Job A 1],[Job F 6],[Job B 2]]
                jobs scSmallD `shouldBe` [[Job A 1,Job D 4],[Job F 6],[Job B 2]]

        describe "idle until" $ do
            describe "fills the todos with idle time until time of completion of a given step" $ do
                    it "only if the step is being done"  $ do
                        jobs (idleUntil A scSmall)  `shouldBe` [[],[],[]]
                        jobs (idleUntil A scSmallD) `shouldBe` [[Job A 1,Job D 4],[Job F 6],[Job B 2]]
                        jobs (idleUntil J scSmallF) `shouldBe` [[Job A 1],[Job F 6],[]]
                    it "filling every other todo" $ do
                        jobs (idleUntil A scSmallA)  `shouldBe` [[Job A 1],[Idle 1],[Idle 1]]
                        jobs (idleUntil F scSmallD)  `shouldBe` [[Job A 1,Job D 4,Idle 1],[Job F 6],[Job B 2,Idle 4]]
                    it "filling only the todo wih time smaller than time to completion of the step" $ do
                        jobs (idleUntil D scSmallD)  `shouldBe` [[Job A 1,Job D 4],[Job F 6],[Job B 2,Idle 3]]

        describe "start" $ do
            it "sets the first next steps of a schedule to the starting steps" $ do 
                nextSteps (start scSmall) `shouldBe` [C]
                nextSteps (start scTiny)  `shouldBe` [Q]
            it "ordered by descending critical path time" $ do
                catMaybes (L.map (`M.lookup` criticalPathTime scLarge) [E,B,V,U]) `shouldBe` [527,468,332,331]
                nextSteps (start scLarge)  `shouldBe` [E,B,V,U]
            
        describe "last step" $ do
            it "tells the last step in the schedule" $ do
                lastStep scSmall `shouldBe` E
                lastStep scTiny  `shouldBe` Y
                lastStep scLarge  `shouldBe` Y

        describe "done" $ do
            it "tells if the last step has been done in the schedule" $ do
                done scSmall `shouldBe` False
