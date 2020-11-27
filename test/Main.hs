module Main where

import System.Directory (findExecutable, getCurrentDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (ExitSuccess), die)
import System.FilePath ((</>))
import System.Process (callProcess, proc, readCreateProcessWithExitCode)
import Test.Hspec (Expectation, Spec, afterAll_, beforeAll_, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hiedb" $ do
    beforeAll_ compileTestModules $ afterAll_ cleanTestData $ do
      describe "index" $
        it "indexes testing project .hie files" $ do
          runHieDbCli ["index", testTmp, "--quiet"]
            `suceedsWithStdin` ""

      describe "ls" $
        it "lists the indexed modules" $ do
          cwd <- getCurrentDirectory
          runHieDbCli ["ls"]
            `suceedsWithStdin` unlines (fmap (\x -> cwd </> testTmp </> x)
                [ "Foo.hie\tFoo\tmain"
                , "One/Two/Some.hie\tOne.Two.Some\tmain"
                ])

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


cleanTestData :: IO ()
cleanTestData = removeDirectoryRecursive testTmp

compileTestModules :: IO ()
compileTestModules =
  callProcess "ghc" $
    "-fwrite-ide-info" :
    "-hiedir=" <> testTmp :
    "-hidir=" <> testTmp :
    "-odir=" <> testTmp :
    testModules


testModules :: [FilePath]
testModules = fmap ("test/data"</>)
  [ "Foo.hs"
  , "One/Two/Some.hs"
  ]

testDb :: FilePath
testDb = testTmp </> "test.hiedb"

testTmp :: FilePath
testTmp = "test/tmp"
