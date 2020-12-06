module Main where

import GHC.Paths (libdir)
import HieDb (HieDb, HieModuleRow (..), LibDir (..), ModuleInfo (..), getAllIndexedMods, withHieDb')
import HieDb.Run (Command (..), Options (..), runCommand)
import Module (moduleNameString)
import System.Directory (findExecutable, getCurrentDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (ExitSuccess), die)
import System.FilePath ((</>))
import System.Process (callProcess, proc, readCreateProcessWithExitCode)
import Test.Hspec (Expectation, Spec, afterAll_, around, beforeAll_, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "hiedb" $
    beforeAll_ compileTestModules $
    afterAll_ cleanTestData $ do
      cliSpec
      apiSpec

apiSpec :: Spec
apiSpec = describe "api" $
  beforeAll_ (runCommandTest (Index [testTmp])) $
    around withTestDb $
      describe "HieDb.Query" $ do
        it "getAllIndexedMods returns all indexed modules" $ \conn -> do
          mods <- getAllIndexedMods conn
          case mods of
            [m1,m2] -> do
              moduleNameString (modInfoName (hieModInfo m1)) `shouldBe` "Foo"
              moduleNameString (modInfoName (hieModInfo m2)) `shouldBe` "One.Two.Some"
            xs -> fail $ "Was expecting 2 modules, but got " <> show (length xs)


cliSpec :: Spec
cliSpec =
  describe "Command line" $ do
    describe "index" $
      it "indexes testing project .hie files" $ do
        runHieDbCli ["index", testTmp, "--quiet"]
          `suceedsWithStdin` ""

    describe "ls" $
      it "lists the indexed modules" $ do
        cwd <- getCurrentDirectory
        let expectedOutput = unlines (fmap (\x -> cwd </> testTmp </> x)
              [ "Foo.hie\tFoo\tmain"
              , "One/Two/Some.hie\tOne.Two.Some\tmain"
              ])
        runHieDbCli ["ls"] `suceedsWithStdin` expectedOutput

    describe "name-refs" $
      it "lists all references of given function" $ do
        runHieDbCli ["name-refs", "printInt"]
          `suceedsWithStdin` unlines
            ["Foo:3:7-3:15"
            ,"Foo:12:1-12:9"
            ,"Foo:13:1-13:9"
            ]

    describe "point-refs" $
      it "list references at given point" $
        runHieDbCli ["point-refs", "Foo", "13", "2"]
          `suceedsWithStdin` unlines
            [ "Name printInt at (13,2) is used in:"
            , "Foo:3:7-3:15"
            , "Foo:12:1-12:9"
            , "Foo:13:1-13:9"
            ]


suceedsWithStdin :: IO (ExitCode, String, String) -> String -> Expectation
suceedsWithStdin action expectedStdin = do
  (exitCode, actualStdin, actualStdErr) <- action
  exitCode `shouldBe` ExitSuccess
  actualStdErr `shouldBe` ""
  actualStdin `shouldBe` expectedStdin


runHieDbCli :: [String] -> IO (ExitCode, String, String)
runHieDbCli args = do
  hiedb <- findHieDbExecutable
  let argsWithTestDb = "--database" : testDb : args
  let createProc = proc hiedb argsWithTestDb
  putStrLn $ unwords $ "RUNNING: hiedb" : argsWithTestDb
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

withTestDb :: (HieDb -> IO a) -> IO a
withTestDb = withHieDb' (LibDir libdir) testDb

runCommandTest :: Command -> IO ()
runCommandTest = runCommand (LibDir libdir) testOpts

testOpts :: Options
testOpts = Options
  { database = testDb
  , trace = False
  , quiet = True
  , virtualFile = False
  }
