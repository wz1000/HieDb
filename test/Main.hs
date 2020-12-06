module Main where

import GHC.Paths (libdir)
import HieDb (HieDb, HieModuleRow (..), LibDir (..), ModuleInfo (..), withHieDb')
import HieDb.Query (getAllIndexedMods, lookupHieFile, resolveUnitId)
import HieDb.Run (Command (..), Options (..), runCommand)
import HieDb.Types (HieDbErr (..))
import Module (mkModuleName, moduleNameString, stringToUnitId)
import System.Directory (findExecutable, getCurrentDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (ExitSuccess), die)
import System.FilePath ((</>))
import System.Process (callProcess, proc, readCreateProcessWithExitCode)
import Test.Hspec (Expectation, Spec, afterAll_, around, beforeAll_, describe, hspec, it, shouldBe,
                   shouldEndWith)
import Test.Orphans ()

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

        describe "getAllIndexedMods" $ do
          it "returns all indexed modules" $ \conn -> do
            mods <- getAllIndexedMods conn
            case mods of
              [m1,m2] -> do
                moduleNameString (modInfoName (hieModInfo m1)) `shouldBe` "Foo"
                moduleNameString (modInfoName (hieModInfo m2)) `shouldBe` "One.Two.Some"
              xs -> fail $ "Was expecting 2 modules, but got " <> show (length xs)

        describe "resolveUnitId" $ do
          it "resolves unit when module unambiguous" $ \conn -> do
            res <- resolveUnitId conn (mkModuleName "Foo")
            case res of
              Left e       -> fail $ "Unexpected error: " <> show e
              Right unitId -> unitId `shouldBe` stringToUnitId "main"

          it "returns NotIndexed error on not-indexed module" $ \conn -> do
            let notIndexedModule = mkModuleName "NotIndexed"
            res <- resolveUnitId conn notIndexedModule
            case res of
              Left (NotIndexed modName Nothing) -> modName `shouldBe` notIndexedModule
              Left e                            -> fail $ "Unexpected error: " <> show e
              Right unitId                      -> fail $ "Unexpected success: " <> show unitId

        describe "lookupHieFile" $ do
          it "Should lookup indexed Module" $ \conn -> do
            let modName = mkModuleName "Foo"
            res <- lookupHieFile conn modName (stringToUnitId "main")
            case res of
              Just modRow -> do
                hieModuleHieFile modRow `shouldEndWith` "Foo.hie"
                let modInfo = hieModInfo modRow
                modInfoIsReal modInfo `shouldBe` False
                modInfoName modInfo `shouldBe` modName
              Nothing -> fail "Should have looked up indexed file"
          it "Should return Nothing for not indexed Module" $ \conn -> do
            res <- lookupHieFile conn (mkModuleName "NotIndexed") (stringToUnitId "main")
            case res of
              Nothing -> pure ()
              Just _  -> fail "Lookup suceeded unexpectedly"


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
    "-fno-code" : -- don't produce unnecessary .o and .hi files
    "-fwrite-ide-info" :
    "-hiedir=" <> testTmp :
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
