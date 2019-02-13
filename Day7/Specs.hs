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
        it "given a list of workers, assign a step to the least loaded worker" $ do
            let ws  = [[],[Idle 1],[Idle 2]]
                ws' = assign A 3 ws
                ws''= assign B 2 ws'
                ws'''= assign C 2 ws''
            ws' `shouldBe` [[Work A 3],[Idle 1],[Idle 2]]
            ws'' `shouldBe` [[Work A 3],[Idle 1,Work B 2],[Idle 2]]
            ws''' `shouldBe` [[Work A 3],[Idle 1,Work B 2],[Idle 2,Work C 2]]
            

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
