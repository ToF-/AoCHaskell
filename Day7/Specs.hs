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

        describe "minimal start times" $ do
            it "tells at what time at least each step should take place given a base duration" $ do
                M.toList (minimalTimes 0 (succList small))  `shouldBe` [(A,3),(B,4),(C,0),(D,4),(E,9),(F,3)]

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
            timeWorked []  `shouldBe` 0
            timeWorked [Job A 42,Idle 17] `shouldBe` 59

        it "has steps done at the total worked time" $ do
            stepsDone [] `shouldBe` []
            stepsDone [Job F 6,Job C 3] `shouldBe`  [(F,9),(C,9)]

        it "can be asked to wait until a given time" $ do
            wait 42 [] `shouldBe` [Idle 42]
            wait 9 [Job C 3] `shouldBe` [Idle 6, Job C 3]
            wait 4  [Idle 17] `shouldBe` [Idle 17]

    describe "a critical path time list" $ do
        it "tells the critical path time from any step given a successor list and a base duration" $ do
            let cpl = criticalPathTimeList 0 (succList small) 
            M.toList cpl  `shouldBe` [(A,1+4+5),(B,2+5),(C,3+6+5),(D,4+5),(E,5),(F,6+5)]
            let cpl = criticalPathTimeList 60 (succList large) 
            M.toList cpl  `shouldBe` 
                [(A,744),(B,825),(C,148),(D,608),(E,673),(F,610),(G,532),(H,378),(I,233)
                ,(J,535),(K,381),(L,607),(M,469),(N,606),(O,544),(P,683),(Q,310),(R,622)
                ,(S,164),(T,763),(U,825),(V,848),(W,766),(X,465),(Y,85),(Z,396)]

    describe "a schedule" $ do
        let schSmall = schedule 2 0 small 
            schLarge = schedule 5 60 large
            schSmall' = assignStep (C,0) schSmall
            schSmall''= assignStep (F,3) schSmall'

        it "is created with a number of workers, a base duration, and a list of edges" $ do
            baseDuration schSmall `shouldBe` 0
            workers schSmall `shouldBe` [[],[]]
            successors schSmall  `shouldBe` succList small
            predecessors schSmall `shouldBe` predList (successors schSmall)
            criticalPath schSmall `shouldBe` criticalPathTimeList 0 (successors schSmall)

        it "can have steps in progress" $ do
            stepsInProgress schSmall  `shouldBe` []

        it "can assign a step to the first worker that is ready" $ do
            workers schSmall'' `shouldBe` [[Job C 3],[Job F 6,Idle 3]]
            workers schSmall' `shouldBe` [[Job C 3],[]]

        it "can tell all its steps that are done" $ do
            allStepsDone schSmall''  `shouldBe` [(C,3),(F,9)]

        describe "nextSteps" $ do
            describe "tells the next Step that can be executed given the current schedule" $ do
                it "at the start, tells the first steps" $ do
                    nextSteps schSmall `shouldBe` [(C,0)]
                    nextSteps schLarge  `shouldBe` [(V,0),(B,0),(U,0),(E,0)]
                it "tells the step in order of descending critical path time" $ do
                    L.map fst (nextSteps schLarge) `shouldBe` [V,B,U,E]
                it "tells the step for which all predecessors are done" $ do
                    let schSmall' = schSmall { workers = [[Job C 3],[]] }
                    nextSteps schSmall' `shouldBe` [(F,3),(A,3)] 
                    let schSmall'' = schSmall { workers = [[Job C 3],[Job F 6,Idle 3]] }
                    L.concatMap stepsDone (workers schSmall'')  `shouldBe` [(C,3),(F,9)]
                    nextSteps schSmall'' `shouldBe` [(A,3)]
                it "tells the time that the step was done" $ do
                    let schB = schSmall { workers = [[Job D 4,Job A 1,Job C 3],[Job F 6,Idle 3]] }
                    allStepsDone schB `shouldBe` [(D,8),(A,8),(C,8),(F,9)]
                    nextSteps schB `shouldBe` [(B,8)]
                it "tells the maximum time that the next step has to be done" $ do
                    let schE = schSmall { workers = [[Job B 2,Job D 4,Job A 1,Job C 3],[Job F 6,Idle 3]] }
                    allStepsDone schE `shouldBe` [(B,10),(D,10),(A,10),(C,10),(F,9)]
                    nextSteps schE `shouldBe` [(E,10)]
                it "doesn't eliminate previous successors that were already in the next steps" $ do
                    nextSteps schLarge  `shouldBe` [(V,0),(B,0),(U,0),(E,0)]
                    nextSteps (assignNext schLarge)  `shouldBe` [(B,0),(U,0),(W,82),(E,0)]
        describe "assignNext" $ do
            describe "assign the next step to be done" $ do
                it "on the worker with the minimum time" $ do
                    workers (assignNext schSmall)  `shouldBe` [[Job C 3],[]]
                it "making wait the worker if need be" $ do
                    workers (assignNext (assignNext schSmall)) `shouldBe` [[Job C 3],[Job F 6,Idle 3]]

                    let schE = schSmall { workers = [[Job B 2,Job D 4,Job A 1,Job C 3],[Job F 6,Idle 3]] }
                    nextSteps schE `shouldBe` [(E,10)]
                    workers (assignNext schE) `shouldBe` 
                        [[Job B 2,Job D 4,Job A 1,Job C 3],[Job E 5,Idle 1,Job F 6,Idle 3]]
        describe "run" $ do
            it "assign each step until all is done" $ do
                workers (run schSmall) `shouldBe` 
                 [[Job B 2,Job D 4,Job A 1,Job C 3],[Job E 5,Idle 1,Job F 6,Idle 3]]

        describe "maxTime" $ do
            it "tells the maximum worked time for the given list of edges a number of workers and a base duration" $ do
                maxTime small 2 0 `shouldBe` 15
                maxTime large 5 60 `shouldBe` 1422 -- wrong
        
