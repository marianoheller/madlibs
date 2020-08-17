import FunctorTests
import MonoidTests

putSectionTitle :: String -> IO ()
putSectionTitle s = putStrLn $ "=== " ++ s

main :: IO ()
main = do
  putSectionTitle "Monoid tests"
  monoidMain
  putSectionTitle "Functor tests"
  functorMain
