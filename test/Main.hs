module Main where

import GHC.Paths (libdir)
import HieDb (HieDb, HieModuleRow (..), LibDir (..), ModuleInfo (..), withHieDb')
import HieDb.Query (getAllIndexedMods, lookupHieFile, resolveUnitId)
import HieDb.Run (Command (..), Options (..), runCommand)
import HieDb.Types (HieDbErr (..))
import Module (mkModuleName, moduleNameString, stringToUnitId)
import System.Directory (findExecutable, getCurrentDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..), die)
import System.FilePath ((</>))
import System.Process (callProcess, proc, readCreateProcessWithExitCode)
import Test.Hspec (Expectation, Spec, afterAll_, around, beforeAll_, describe, hspec, it, runIO,
                   shouldBe, shouldEndWith)
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
                moduleNameString (modInfoName (hieModInfo m1)) `shouldBe` "Sub.Module2"
                moduleNameString (modInfoName (hieModInfo m2)) `shouldBe` "Module1"
              xs -> fail $ "Was expecting 2 modules, but got " <> show (length xs)

        describe "resolveUnitId" $ do
          it "resolves unit when module unambiguous" $ \conn -> do
            res <- resolveUnitId conn (mkModuleName "Module1")
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
            let modName = mkModuleName "Module1"
            res <- lookupHieFile conn modName (stringToUnitId "main")
            case res of
              Just modRow -> do
                hieModuleHieFile modRow `shouldEndWith` "Module1.hie"
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
  -- TODO commands not covered: init, type-refs, ref-graph, dump, reachable, unreachable, html
  describe "Command line" $ do
    describe "index" $
      it "indexes testing project .hie files" $ do
        runHieDbCli ["index", testTmp, "--quiet"]
          `suceedsWithStdin` ""

    describe "ls" $
      it "lists the indexed modules" $ do
        cwd <- getCurrentDirectory
        let expectedOutput = unlines (fmap (\x -> cwd </> testTmp </> x)
              [ "Sub/Module2.hie\tSub.Module2\tmain"
              , "Module1.hie\tModule1\tmain"
              ])
        runHieDbCli ["ls"] `suceedsWithStdin` expectedOutput

    describe "name-refs" $
      it "lists all references of given function" $ do
        runHieDbCli ["name-refs", "function2"]
          `suceedsWithStdin` unlines
            [ "Module1:3:7-3:16"
            , "Module1:12:1-12:10"
            , "Module1:13:1-13:10"
            ]

    describe "point-refs" $
      it "list references at given point" $
        runHieDbCli ["point-refs", "Module1", "13", "2"]
          `suceedsWithStdin` unlines
            [ "Name function2 at (13,2) is used in:"
            , "Module1:3:7-3:16"
            , "Module1:12:1-12:10"
            , "Module1:13:1-13:10"
            ]

    describe "point-types" $ do
      it "list references at point when there's Type" $
        runHieDbCli ["point-refs", "Module1", "8", "21"]
          `suceedsWithStdin` unlines
            [ "Name String at (8,21) is used in:"
            , "Sub.Module2:6:19-6:25"
            , "Module1:8:21-8:27"
            ]
      it "Give no output at point when there's not Type" $
        runHieDbCli ["point-refs", "Module1", "7", "1"]
          `suceedsWithStdin` ""

    describe "point-defs" $ do
      it "outputs the location of symbol when definition site can be found is indexed" $
        runHieDbCli ["point-defs", "Module1", "13", "29"]
          `suceedsWithStdin` unlines
            [ "Name showInt at (13,29) is defined at:"
            , "Sub.Module2:7:1-7:8"
            ]
      it "suceeds with no output when there's no symbol at given point" $
        runHieDbCli ["point-defs", "Module1", "13", "13"]
          `suceedsWithStdin` ""
      it "fails with informative error message when the difinition can't be found" $ do
        (exitCode, actualStdout, _) <- runHieDbCli ["point-defs", "Module1", "13", "24"]
        exitCode `shouldBe` ExitFailure 1
        actualStdout `shouldBe` "Couldn't find name: $ from module GHC.Base(base)\n"

    describe "point-info" $
      it "gives information about symbol at specified location" $
        runHieDbCli ["point-info", "Sub.Module2", "10", "10"]
          `suceedsWithStdin` unlines
            [ "Span: test/data/Sub/Module2.hs:10:7-23"
            , "Constructors: {(ConDeclH98, ConDecl)}"
            , "Identifiers:"
            , "Symbol:c:Data1Constructor1:Sub.Module2:main"
            , "Data1Constructor1 defined at test/data/Sub/Module2.hs:10:7-23"
            , "    IdentifierDetails Nothing {Decl ConDec (Just SrcSpanOneLine \"test/data/Sub/Module2.hs\" 10 7 24)}"
            , "Types:\n"
            ]

    describe "name-def" $
      it "lookup definition of name" $
        runHieDbCli ["name-def", "showInt"]
          `suceedsWithStdin` "Sub.Module2:7:1-7:8\n"

    describe "type-def" $
      it "lookup definition of type" $
        runHieDbCli ["type-def", "Data1"]
          `suceedsWithStdin` "Sub.Module2:9:1-11:28\n"

    describe "cat" $
      describe "dumps module source stored in .hie file" $ do
        module1Src <- runIO . readFile $ "test" </> "data" </> "Module1.hs"
        it "when given --hiefile" $ do
          cwd <- getCurrentDirectory
          runHieDbCli ["cat", "--hiefile" , cwd </> testTmp </> "Module1.hie"]
            `suceedsWithStdin` (module1Src <> "\n")
        it "when given module name" $
          runHieDbCli ["cat", "Module1"]
            `suceedsWithStdin` (module1Src <> "\n")

    describe "lookup-hie" $
      it "looks up location of .hie file" $ do
        cwd <- getCurrentDirectory
        runHieDbCli ["lookup-hie", "Module1"]
          `suceedsWithStdin` (cwd </> testTmp </> "Module1.hie\n")

    describe "module-uids" $
      it "lists uids for given module" $
        runHieDbCli ["module-uids", "Module1"]
          `suceedsWithStdin` "main\n"
    
    describe "rm" $
      it "removes given module from DB" $ do
        runHieDbCli ["rm", "Module1"]
          `suceedsWithStdin` ""
        -- Check with 'ls' comand that there's just one module left
        cwd <- getCurrentDirectory
        runHieDbCli ["ls"] `suceedsWithStdin` (cwd </> testTmp </> "Sub/Module2.hie\tSub.Module2\tmain\n")
    


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
testModules = fmap (\m -> "test" </> "data"</> m)
  [ "Module1.hs"
  , "Sub" </> "Module2.hs"
  ]

testDb :: FilePath
testDb = testTmp </> "test.hiedb"

testTmp :: FilePath
testTmp = "test" </> "tmp"

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
