import Test.Hspec
import Zones

main = hspec $ do
    let dx = distances (X,(3,2)) (5,4)
        dy = distances (Y,(1,1)) (5,4)
        dz = distances (Z,(4,3)) (5,4)
    describe "distances" $ do
        it "tells manhattan distances to a point in a rectangle" $ do
            dx `shouldBe` 
                [[Dist X 5, Dist X 4, Dist X 3, Dist X 2, Dist X 3]
                ,[Dist X 4, Dist X 3, Dist X 2, Dist X 1, Dist X 2]
                ,[Dist X 3, Dist X 2, Dist X 1, Dist X 0, Dist X 1]
                ,[Dist X 4, Dist X 3, Dist X 2, Dist X 1, Dist X 2]]
            
            dy `shouldBe` 
                [[Dist Y 2,Dist Y 1,Dist Y 2,Dist Y 3,Dist Y 4]
                ,[Dist Y 1,Dist Y 0,Dist Y 1,Dist Y 2,Dist Y 3]
                ,[Dist Y 2,Dist Y 1,Dist Y 2,Dist Y 3,Dist Y 4]
                ,[Dist Y 3,Dist Y 2,Dist Y 3,Dist Y 4,Dist Y 5]]

    describe "merge distances" $ do
        it "merges manhattan distances to two point in a rectangle" $ do
          merge dx dy  `shouldBe` 
                [[Dist Y 2,Dist Y 1,Dist Y 2,Dist X 2,Dist X 3]
                ,[Dist Y 1,Dist Y 0,Dist Y 1,Dist X 1,Dist X 2]
                ,[Dist Y 2,Dist Y 1,Dist X 1,Dist X 0,Dist X 1]
                ,[Dist Y 3,Dist Y 2,Dist X 2,Dist X 1,Dist X 2]]
        it "merges manhattan distances to two points signaling ties" $ do
          merge dx dz  `shouldBe` 
                [[Dist X 5,Dist X 4,Dist X 3,Dist X 2,Tie    3]
                ,[Dist X 4,Dist X 3,Dist X 2,Dist X 1,Tie    2]
                ,[Dist X 3,Dist X 2,Dist X 1,Dist X 0,Tie    1]
                ,[Tie    4,Tie    3,Tie    2,Tie    1,Dist Z 0]]

    describe "zone" $ do
        it "merges all the distances to points in a rectangle" $ do
          zone small `shouldBe`
            [[Dist A 2,Dist A 1,Dist A 2,Dist A 3,Dist A 4,Tie    5,Dist C 5,Dist C 4,Dist C 3,Dist C 4]
            ,[Dist A 1,Dist A 0,Dist A 1,Dist A 2,Dist A 3,Tie    4,Dist C 4,Dist C 3,Dist C 2,Dist C 3]
            ,[Dist A 2,Dist A 1,Dist A 2,Dist D 2,Dist D 3,Dist E 3,Dist C 3,Dist C 2,Dist C 1,Dist C 2]
            ,[Dist A 3,Dist A 2,Dist D 2,Dist D 1,Dist D 2,Dist E 2,Dist C 2,Dist C 1,Dist C 0,Dist C 1]
            ,[Tie    3,Tie    2,Dist D 1,Dist D 0,Dist D 1,Dist E 1,Dist E 2,Dist C 2,Dist C 1,Dist C 2]
            ,[Dist B 2,Dist B 1,Tie    2,Dist D 1,Dist E 1,Dist E 0,Dist E 1,Dist E 2,Dist C 2,Dist C 3]
            ,[Dist B 1,Dist B 0,Dist B 1,Tie    2,Dist E 2,Dist E 1,Dist E 2,Dist E 3,Tie    3,Tie    4]
            ,[Dist B 2,Dist B 1,Dist B 2,Tie    3,Dist E 3,Dist E 2,Dist E 3,Dist F 3,Dist F 2,Dist F 3]
            ,[Dist B 3,Dist B 2,Dist B 3,Tie    4,Dist E 4,Dist E 3,Dist F 3,Dist F 2,Dist F 1,Dist F 2]
            ,[Dist B 4,Dist B 3,Dist B 4,Tie    5,Dist F 4,Dist F 3,Dist F 2,Dist F 1,Dist F 0,Dist F 1]
            ,[Dist B 5,Dist B 4,Dist B 5,Tie    6,Dist F 5,Dist F 4,Dist F 3,Dist F 2,Dist F 1,Dist F 2]]

    describe "infinites" $ do
        it "tells what points have infinite zones of proximity" $ do
            infinites small `shouldBe` [A,B,C,F]

    describe "largest" $ do
        it "tells what is the largest proximity zone which is not infinite" $ do
            largest small  `shouldBe` 17
            largest large  `shouldBe` 4215



-- aaaaa.cccc
-- aAaaa.cccc
-- aaaddecccc
-- aadddeccCc
-- ..dDdeeccc
-- bb.deEeecc
-- bBb.eeee..
-- bbb.eeefff
-- bbb.eeffff
-- bbb.ffffFf
            
small = [(1, 1)
        ,(1, 6)
        ,(8, 3)
        ,(3, 4)
        ,(5, 5)
        ,(8, 9)]

large = [(158, 163)
        ,(287, 68)
        ,(76, 102)
        ,(84, 244)
        ,(162, 55)
        ,(272, 335)
        ,(345, 358)
        ,(210, 211)
        ,(343, 206)
        ,(219, 323)
        ,(260, 238)
        ,(83, 94)
        ,(137, 340)
        ,(244, 172)
        ,(335, 307)
        ,(52, 135)
        ,(312, 109)
        ,(276, 93)
        ,(288, 274)
        ,(173, 211)
        ,(125, 236)
        ,(200, 217)
        ,(339, 56)
        ,(286, 134)
        ,(310, 192)
        ,(169, 192)
        ,(313, 106)
        ,(331, 186)
        ,(40, 236)
        ,(194, 122)
        ,(244, 76)
        ,(159, 282)
        ,(161, 176)
        ,(262, 279)
        ,(184, 93)
        ,(337, 284)
        ,(346, 342)
        ,(283, 90)
        ,(279, 162)
        ,(112, 244)
        ,(49, 254)
        ,(63, 176)
        ,(268, 145)
        ,(334, 336)
        ,(278, 176)
        ,(353, 135)
        ,(282, 312)
        ,(96, 85)
        ,(90, 105)
        ,(354, 312)]
