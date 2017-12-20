import Test.HUnit
import Text.JSON
import Testes
import ProgFuncLista

main = do
  tests <- (runTestTT mytests)
  let bd = words (showCounts tests)
  let success = read (bd !! 1) - read (bd !! 5) - read (bd !! 7)
  let list = [("matricula",matricula), ("totalTestes",read(bd !! 1)),("erros",read (bd !! 5)), ("falhas",read (bd !! 7)), ("passaram",success)]
  let results = toJSObject list
  writeFile "test-output.json" (encodeStrict results)
