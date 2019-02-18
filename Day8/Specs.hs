import Test.Hspec
import Tree
main = hspec $ do
    describe "a tree" $ do
        describe "is formed with a node that can have child nodes" $ do
            let t = Node 
                        [Node [] [10,11,12]
                        ,Node [Node [] [99]] [2] ]
                        [1,1,2]
            it "can have add all its metadata entries" $ do
                sumEntries t `shouldBe` 138

            
            describe     "can be built from a list of ints" $ do
                it "for a tree without no subtrees" $ do
                    fst (tree [0,3,1,2,3])  `shouldBe` 
                        Node [] [1,2,3]
                it "for a tree without no subtrees and more entries than specified" $ do
                    fst (tree [0,3,1,2,3,4])  `shouldBe` 
                        Node [] [1,2,3]
                it "for a tree without 1 subtree" $ do
                    fst (tree [1,3,0,0,1,2,3]) `shouldBe` 
                        Node [Node [] []] [1,2,3]
                it "for a tree without 2 subtree" $ do
                    fst (tree [2,3,0,2,10,11,0,0,1,2,3]) `shouldBe` 
                        Node [Node [] [10,11], Node [] []] [1,2,3]
                it "for a tree without 3 subtree" $ do
                    fst (tree [3,3, 0,2,10,11, 0,1,42, 0,0, 1,2,3]) `shouldBe` 
                        Node [Node [] [10,11]
                             ,Node [] [42]
                             ,Node [] []    ] [1,2,3]
                it "works for small trees" $ do
                    fst (tree [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2])
                     `shouldBe` 
                        Node [Node [] [10,11,12]
                         ,Node [Node [] [99]] [2]] [1,1,2]
