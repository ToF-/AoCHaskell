import Test.Hspec
import Steps
import Data.Map as M
import Data.List as L

import Data.Ord
small = [(C,A)
        ,(C,F)
        ,(A,B)
        ,(A,D)
        ,(B,E)
        ,(D,E)
        ,(F,E)]

--       -->A--->B--
--      /    \      \
--     C      -->D----->E
--      \           /
--       ---->F-----

tiny = [(Q,Y)
       ,(Q,C)
       ,(Q,I)
       ,(I,Y)
       ,(I,S)
       ,(C,Y)
       ,(S,Y)]

--      -->I--->S--
--     /    \      \
--    Q------------>Y
--     \           /
--      ---->C-----


large=
    [(V,H)
    ,(U,R)
    ,(E,D)
    ,(B,R)
    ,(W,X)
    ,(A,P)
    ,(T,L)
    ,(F,C)
    ,(P,Y)
    ,(N,G)
    ,(R,S)
    ,(D,C)
    ,(O,K)
    ,(L,J)
    ,(J,H)
    ,(M,I)
    ,(G,K)
    ,(Z,Q)
    ,(X,Q)
    ,(H,I)
    ,(K,Y)
    ,(Q,S)
    ,(I,Y)
    ,(S,Y)
    ,(C,Y)
    ,(T,S)
    ,(P,S)
    ,(I,S)
    ,(V,O)
    ,(O,Q)
    ,(T,R)
    ,(E,J)
    ,(F,S)
    ,(O,H)
    ,(Z,S)
    ,(D,Z)
    ,(F,K)
    ,(W,P)
    ,(G,I)
    ,(B,T)
    ,(G,Y)
    ,(X,S)
    ,(B,K)
    ,(V,A)
    ,(U,N)
    ,(T,P)
    ,(V,D)
    ,(G,X)
    ,(B,D)
    ,(R,J)
    ,(M,Z)
    ,(U,Z)
    ,(U,G)
    ,(A,C)
    ,(H,Q)
    ,(X,K)
    ,(B,S)
    ,(Q,C)
    ,(Q,Y)
    ,(R,I)
    ,(V,Q)
    ,(A,D)
    ,(D,S)
    ,(K,S)
    ,(G,C)
    ,(D,O)
    ,(R,H)
    ,(K,Q)
    ,(W,R)
    ,(H,Y)
    ,(P,J)
    ,(N,Z)
    ,(J,K)
    ,(W,M)
    ,(A,Z)
    ,(V,W)
    ,(J,X)
    ,(U,F)
    ,(P,L)
    ,(W,G)
    ,(T,F)
    ,(R,C)
    ,(R,O)
    ,(Z,C)
    ,(E,S)
    ,(L,I)
    ,(U,O)
    ,(W,K)
    ,(K,I)
    ,(O,M)
    ,(V,M)
    ,(V,Z)
    ,(A,I)
    ,(F,J)
    ,(F,O)
    ,(M,C)
    ,(Q,I)
    ,(H,S)
    ,(U,A)
    ,(J,S)
    ,(P,Z)]

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
        let sc = schedule 3 0 small
            sc'= schedule 2 1000 tiny
            sc''= schedule 5 60 large
            sc1 = assignJob sc A 
            sc11 = assignJob sc1 F
            sc12 = assignJob sc11 B
            sc13 = assignJob sc12 D
            sc2 = assignJob sc' C 
        it "has a number of todo list" $ do
            jobs sc `shouldBe` [[],[],[]]

        describe "critical paths" $ do
            it "tells the critical time from any step with a given basis" $ do
                (sort (M.toList (criticalPaths sc)))
                    `shouldBe` [(A,10),(B,7),(C,14),(D,9),(E,5),(F,11)]
                (sort (M.toList (criticalPaths sc'))) 
                    `shouldBe` [(C,2028),(I,3053),(Q,4070),(S,2044),(Y,1025)]
      
        describe "available time" $ do
            it "tells what is the smaller available time in the schedule" $ do
                availableTime sc `shouldBe` 0

        describe "assign job" $ do
            it "assigns a step for the step time in the todo with the smaller available time" $ do
                jobs sc1  `shouldBe` [[Job A 1],[],[]]
                jobs sc2 `shouldBe` [[Job C 1003],[]]
                jobs sc11 `shouldBe` [[Job A 1],[Job F 6],[]]
                jobs sc12 `shouldBe` [[Job A 1],[Job F 6],[Job B 2]]
                jobs sc13 `shouldBe` [[Job A 1,Job D 4],[Job F 6],[Job B 2]]
                availableTime sc13 `shouldBe` 2

        describe "fill until step" $ do
            it "fills the least loaded todo until time of execution of a step is done" $ do
                jobs (fillUntil A sc)  `shouldBe` [[],[],[]]
                jobs (fillUntil A sc1)  `shouldBe` [[Job A 1],[Idle 1],[Idle 1]]
                jobs (fillUntil F sc13)  `shouldBe` [[Job A 1,Job D 4,Idle 1],[Job F 6],[Job B 2,Idle 4]]

        describe "start" $ do
            it "assigns the first steps on a schedule ordered by critical time" $ do
                jobs (start sc) `shouldBe` [[Job C 3],[],[]]
                jobs (start sc') `shouldBe` [[Job Q 1017],[]]
                M.lookup E (criticalPaths sc'') `shouldBe` Just 527
                M.lookup B (criticalPaths sc'') `shouldBe` Just 468
                M.lookup V (criticalPaths sc'') `shouldBe` Just 332
                M.lookup U (criticalPaths sc'') `shouldBe` Just 331
                
                jobs (start sc'') `shouldBe` [[Job B 62],[Job E 65],[Job U 81],[Job V 82],[]]
            
