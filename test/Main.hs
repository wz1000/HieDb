module Main where

import Control.Monad (when)
import Data.List (isSuffixOf)
import System.Directory (doesFileExist, findExecutable, removeFile)
import System.Exit (ExitCode (ExitSuccess), die)
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Hspec (Expectation, Spec, afterAll_, describe, hspec, it, shouldBe, shouldSatisfy)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hiedb" $ do
    afterAll_ removeTestDb $ do
      describe "index" $
        it "indexes testing project .hie files" $ do
          runHieDbCli ["index" , ".hie", "--quiet"]
            `suceedsWithStdin` ""

      describe "ls" $
        it "lists the indexed modules" $ do
          runHieDbCli ["ls"]
            `succeedsWithStdinSatisfying` (\stdin -> do
              let inlines = lines stdin
              length inlines `shouldBe` 2
              (inlines !! 0) `shouldSatisfy` isSuffixOf ".hie/Foo.hie\tFoo\thiedb-0.1.0.0-inplace-test-lib"
              (inlines !! 1) `shouldSatisfy` isSuffixOf ".hie/One/Two/Some.hie\tOne.Two.Some\thiedb-0.1.0.0-inplace-test-lib"
              )

      describe "name-refs" $
        it "lists all references of given function" $ do
          runHieDbCli ["name-refs", "printInt"]
            `suceedsWithStdin` unlines
              ["Foo:3:7-3:15"
              ,"Foo:12:1-12:9"
              ,"Foo:13:1-13:9"
              ]


suceedsWithStdin :: IO (ExitCode, String, String) -> String -> Expectation
suceedsWithStdin action expectedStdin =
  succeedsWithStdinSatisfying action (`shouldBe`expectedStdin)


succeedsWithStdinSatisfying :: IO (ExitCode, String, String) -> (String -> Expectation) -> Expectation
succeedsWithStdinSatisfying action expectStdin  = do
  (exitCode, actualStdin, actualStdErr) <- action
  exitCode `shouldBe` ExitSuccess
  actualStdErr `shouldBe` ""
  expectStdin actualStdin


runHieDbCli :: [String] -> IO (ExitCode, String, String)
runHieDbCli args = do
  hiedb <- findHieDbExecutable
  let createProc = proc hiedb ("--database" : testDb : args)
  putStrLn $ unwords $ "RUNNING:":hiedb:args
  readCreateProcessWithExitCode createProc ""


findHieDbExecutable :: IO FilePath
findHieDbExecutable =
  maybe (die "Did not find hiedb executable") pure =<< findExecutable "hiedb"


testDb :: FilePath
testDb = "test.hiedb"


removeTestDb :: IO ()
removeTestDb = do
  ex <- doesFileExist testDb
  when ex $ removeFile testDb
