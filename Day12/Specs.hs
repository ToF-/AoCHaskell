import Test.Hspec

notes = ["...##"
        ,"..#.."
        ,".#..."
        ,".#.#."
        ,".#.##"
        ,".##.."
        ,".####"
        ,"#.#.#"
        ,"#.###"
        ,"##.#."
        ,"##.##"
        ,"###.."
        ,"###.#"
        ,"####."]

main = hspec $ do
   describe "a pattern " $ do
    it "tells if a plant will grow or not" $ do
         "...##" `elem` notes `shouldBe` True 
