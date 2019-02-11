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

main = hspec $ do
    let g = graph small
    let gg= graph large
    describe "graph" $ do
        it "creates a graph of all steps" $ do
            let l = L.sort $ M.toList $ g
                ll= L.sort $ M.toList $ gg
            l `shouldBe` [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[C])]

    describe "priority" $ do
        it "creates a priority list for the steps" $ do
            let l = L.sort $ M.toList $ priority g
            let ll = L.sort $ M.toList $ priority gg
            l `shouldBe` [(A,2),(B,1),(C,3),(D,1),(E,0),(F,1)]
            ll `shouldBe` [(A,9),(B,10),(C,1),(D,7),(E,8),(F,7),(G,6),(H,4),(I,2),(J,6),(K,4),(L,7),(M,5),(N,7),(O,6),(P,8),(Q,3),(R,7),(S,1),(T,9),(U,10),(V,10),(W,9),(X,5),(Y,0),(Z,4)]

    describe "steps" $ do
        it "tells which steps to do in which order" $ do
            let l = concatMap show $ steps g
            l `shouldBe` "CABDFE"
            let ll = concatMap show $ steps gg
            ll `shouldBe` ""

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

