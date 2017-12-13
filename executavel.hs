import Test.HUnit
import Text.JSON
import Testes
import ProgFuncLista

main = do
  tests <- (runTestTT testBST)
  let bd = words (showCounts tests)
  let success = read (bd !! 1) - read (bd !! 5) - read (bd !! 7)
  let results = [matricula, read (bd !! 1), read (bd !! 5), read(bd !! 7), success]
  writeFile "test-output.json" (encodeStrict (showJSONs results) ++ "\n\n%%%%%%%%%%%%%%%%%%%%%%%%%")
