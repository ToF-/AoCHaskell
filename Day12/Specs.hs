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

    it "tells if several plants will grow or not" $ do
        let initial = "...#..#.#..##......###...###..........."
        let next    = "...#...#....#.....#..#..#..#..........."
        sustain notes initial  `shouldBe` next 
