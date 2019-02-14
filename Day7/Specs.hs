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

    describe "critical paths" $ do
        it "tells the critical time from any step with a given basis" $ do
            (sort (M.toList (criticalPaths 0 (predList small))))  
                `shouldBe` [(A,10),(B,7),(C,14),(D,9),(E,5),(F,11)]
            (sort (M.toList (criticalPaths 1000 (predList tiny))))  
                `shouldBe` [(C,2028),(I,3053),(Q,4070),(S,2044),(Y,1025)]
            
    describe "assign" $ do
        it "given a worker schedule, assign a step to the least loaded worker" $ do
            let s  = [[],[Idle 1],[Idle 2]] -- Idle counts a work load
                s' = assign A 3 s
                s''= assign B 2 s'
                s'''= assign C 2 s''
            s' `shouldBe` [[Job A 3],[Idle 1],[Idle 2]]
            s'' `shouldBe` [[Job A 3],[Idle 1,Job B 2],[Idle 2]]
            s''' `shouldBe` [[Job A 3],[Idle 1,Job B 2],[Idle 2,Job C 2]]
            
    describe "steps done" $ do
        it "given a worker schedule, tell the steps that are done at the time when the least loaded worker is done" $ do
            let sc = [[Job C 3],[Idle 1,Job B 2],[Idle 2]]
            let sc'= [[Job C 3],[Idle 1,Job B 2],[Idle 2,Job C 3]]
            L.map timeWhenDone sc `shouldBe` [3,3,2]
            L.map timeWhenDone sc' `shouldBe` [3,3,5]
            stepsDone sc `shouldBe` stepsDoneAt 2 sc
            stepsDone sc' `shouldBe` stepsDoneAt 3 sc'
            stepsDone sc `shouldBe` []
            stepsDone sc' `shouldBe` [C,B]
            let sc = [[Job C 3],[Idle 3,Job F 6],[Idle 3]]
            stepsDone sc `shouldBe` [C]
            stepsDone [[Job C 3],[Idle 3],[Idle 3]] `shouldBe` [C]


    describe "next steps" $ do
        let succ = succList small
            pred = predList small
            cp = criticalPaths 0 (pred)
        it ("given a work schedule, succ and pred list, and a critical time table," ++ 
           "tells the next step to be doing, in descending order of critical time") $ do
            let sc = [[Job C 3],[Idle 3],[Idle 3]]
            nextSteps sc succ pred cp `shouldBe` [F,A]

            let sc = [[Job C 3],[Idle 3,Job F 6],[Idle 3]]
            doing sc  `shouldBe` [F]
            stepsDone sc `shouldBe` [C]
            nextSteps sc succ pred cp `shouldBe` [A]
            nextSteps [[Job C 3],[Idle 3],[Idle 3]] succ pred cp `shouldBe` [F,A]

    describe "assign next" $ do
        let succ = succList small
            pred = predList small
            cp = criticalPaths 0 (pred)
        it "assigns the next steps to the first worker done" $ do
            let sc = [[Job C 3],[Idle 3],[Idle 3]]
            assignNext sc succ pred cp  `shouldBe` [[Job C 3,Job F 6],[Idle 3,Job A 1],[Idle 3]]
            let sc =  [[Job C 3,Job F 6],[Idle 3,Job A 1],[Idle 3]]
            stepsDone sc `shouldBe` [C]
            doing sc  `shouldBe` [F,A]
            nextSteps sc succ pred cp `shouldBe` [A]
            assignNext sc succ pred cp  `shouldBe` [[Job C 3,Job F 6],[Idle 3,Job A 1],[Idle 3]]



        it "starts with first steps in case schedule is empty" $ do 
            assignNext [[],[],[]] succ pred cp  `shouldBe` [[Job C 3],[],[]]

    describe "idle" $ do
        it "fills the schedule with idle up the first worker done" $ do
            idle [[],[Job C 3],[]]  `shouldBe`  [[Idle 3],[Job C 3],[Idle 3]]
    describe "schedule" $ do
        it "schedule the steps for a list of edges" $ do
            schedule 3 small `shouldBe` []

