import Test.Hspec
import Steps
import Data.Map as M
import Data.List as L

small = [(C,A)
        ,(C,F)
        ,(A,B)
        ,(A,D)
        ,(B,E)
        ,(D,E)
        ,(F,E)]

tiny = [(Q,S)
       ,(I,Y)
       ,(I,S)
       ,(Q,Y)
       ,(C,Y)
       ,(S,Y)]

main = hspec $ do
    let g = graph small
    let gg= graph large
    let t = graph tiny
    describe "graph" $ do
        it "creates a graph of all steps" $ do
            let l = L.sort $ M.toList $ g
                m = L.sort $ M.toList $ t
            l `shouldBe` [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[C])]
            m `shouldBe` [(S,[I,Q]),(Y,[C,I,Q,S])]

    describe "priority" $ do
        it "creates a priority list for the steps" $ do
            let l = L.sort $ M.toList $ priority g
            let lt = L.sort $ M.toList $ priority t
            l `shouldBe` [(A,2),(B,1),(C,3),(D,1),(E,0),(F,1)]
            lt `shouldBe` [(C,1),(I,1),(Q,2),(S,1),(Y,0)]


    describe "steps" $ do
        it "tells which steps to do in which order" $ do
            let l = concatMap show $ steps g
            l `shouldBe` "CABDFE"
            let lt = concatMap show $ steps t
            lt `shouldBe` "QCISY"

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

