import Test.Hspec
import Stars
import Small

main = hspec $ do
    describe "stars" $ do
        it "occupy an area" $ do
            area small  `shouldBe` ((-6,-4),(15,11))

        it "move every second" $ do
            let small' = move 1 small
                first = small  !! 0
                first'= small' !! 0
                (x0,y0) = position first
                (vx,vy) = velocity first
                (x1,y1) = position first'
            (x1,y1) `shouldBe` (x0+vx,y0+vy)

    describe "area" $ do
        it "has a width and a height" $ do
            let a = area small 
                s = surface a
            width s  `shouldBe` 22
            height s `shouldBe` 16

    describe "viewable" $ do  
        it "is true if an area that is not too wide" $ do
            viewable ((-10000,-4564),(30000,34354)) `shouldBe` False
            viewable ((-10,-25),(30,3)) `shouldBe` True
            viewable (area small) `shouldBe` True

    describe "view" $ do
        it "show a view of the stars if they are viewable" $ do
            let (w,h) = surface (area small)
            length (view small) `shouldBe` (w+1) * h
            
              

            
