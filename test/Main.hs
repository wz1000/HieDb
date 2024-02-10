{-# LANGUAGE CPP #-}
module Main where

import GHC.Paths (libdir, ghc)
import HieDb (HieDb, HieModuleRow (..), LibDir (..), ModuleInfo (..), withHieDb, withHieFile, addRefsFromLoaded, deleteMissingRealFiles, defaultSkipOptions)
import HieDb.Query (getAllIndexedMods, lookupHieFile, resolveUnitId, lookupHieFileFromSource)
import HieDb.Run (Command (..), Options (..), runCommand)
import HieDb.Types (HieDbErr (..), SourceFile(..), runDbM)
import HieDb.Utils (makeNc)
import HieDb.Compat (stringToUnit, moduleNameString, mkModuleName, getFileHash)
import System.Directory (findExecutable, getCurrentDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..), die)
import System.FilePath ((</>))
import System.Process (callProcess, proc, readCreateProcessWithExitCode)
import System.Environment (lookupEnv)
import System.IO.Temp
import System.IO
import Test.Hspec (Expectation, Spec, afterAll_, around, beforeAll_, describe, hspec, it, runIO,
                   shouldBe, shouldEndWith)
import Test.Orphans ()
import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe)

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
              Right unit   -> unit `shouldBe` stringToUnit "main"

          it "returns NotIndexed error on not-indexed module" $ \conn -> do
            let notIndexedModule = mkModuleName "NotIndexed"
            res <- resolveUnitId conn notIndexedModule
            case res of
              Left (NotIndexed modName Nothing) -> modName `shouldBe` notIndexedModule
              Left e                            -> fail $ "Unexpected error: " <> show e
              Right unit                        -> fail $ "Unexpected success: " <> show unit

        describe "lookupHieFile" $ do
          it "Should lookup indexed Module" $ \conn -> do
            let modName = mkModuleName "Module1"
            res <- lookupHieFile conn modName (stringToUnit "main")
            case res of
              Just modRow -> do
                hieModuleHieFile modRow `shouldEndWith` "Module1.hie"
                let modInfo = hieModInfo modRow
                modInfoIsReal modInfo `shouldBe` False
                modInfoName modInfo `shouldBe` modName
              Nothing -> fail "Should have looked up indexed file"
          it "Should return Nothing for not indexed Module" $ \conn -> do
            res <- lookupHieFile conn (mkModuleName "NotIndexed") (stringToUnit "main")
            case res of
              Nothing -> pure ()
              Just _  -> fail "Lookup suceeded unexpectedly"

        describe "deleteMissingRealFiles" $ do
          it "Should delete missing indexed files and nothing else" $ \conn -> do

            originalMods <- getAllIndexedMods conn

            -- Index a new real file, and delete it
            let contents = unlines
                  [ "module Test123 where"
                  , "import Prelude"
                  , "foobarbaz :: Int"
                  , "foobarbaz = 1"
                  ]
            fp <- withSystemTempFile "Test.hs" $ \fp h -> do
              hPutStr h contents
              hClose h
              runGhc $
                "-fno-code" : -- don't produce unnecessary .o and .hi files
                "-fwrite-ide-info" :
                "-hiedir=" <> testTmp :
                [fp]
              let hie_f = testTmp </> "Test123.hie"
              hash <- getFileHash hie_f
              nc <- newIORef =<< makeNc
              runDbM nc $ withHieFile hie_f $
                addRefsFromLoaded conn hie_f (RealFile fp) hash
              pure fp

            -- Check that it was indexed
            before <- lookupHieFileFromSource conn fp
            case before of
              Nothing -> fail $ "File "<> show fp <> "wasn't indexed"
              Just _ -> pure ()

            deleteMissingRealFiles conn

            -- Check that it was deleted from the db
            after <- lookupHieFileFromSource conn fp
            case after of
              Nothing -> pure ()
              Just _ -> fail $ "deleteMissingRealFiles didn't delete file: " <> show fp

            -- Check that the other modules are still indexed
            afterMods <- getAllIndexedMods conn
            originalMods `shouldBe` afterMods

cliSpec :: Spec
cliSpec =
  -- TODO commands not covered: init, type-refs, ref-graph, dump, reachable, unreachable, html
  describe "Command line" $ do
    describe "index" $
      it "indexes testing project .hie files" $ do
        runHieDbCli ["index", testTmp, "--quiet"]
          `succeedsWithStdin` ""

    describe "ls" $
      it "lists the indexed modules" $ do
        cwd <- getCurrentDirectory
        let expectedOutput = fmap (\x -> cwd </> testTmp </> x)
              [ "Sub/Module2.hie\tSub.Module2\tmain"
              , "Module1.hie\tModule1\tmain"
              ]
        (exitCode, actualStdin, _actualStdErr) <- runHieDbCli ["ls"]
        exitCode `shouldBe` ExitSuccess
        sort (lines actualStdin) `shouldBe` sort expectedOutput

    describe "name-refs" $
      it "lists all references of given function" $ do
        runHieDbCli ["name-refs", "function2"]
          `succeedsWithStdin` unlines
            [ "Module1:3:7-3:16"
            , "Module1:12:1-12:10"
            , "Module1:13:1-13:10"
            ]

    describe "point-refs" $
      it "list references at given point" $
        runHieDbCli ["point-refs", "Module1", "13", "2"]
          `succeedsWithStdin` unlines
            [ "Module1:3:7-3:16"
            , "Module1:12:1-12:10"
            , "Module1:13:1-13:10"
            ]

    describe "point-types" $ do
      it "Prints types of symbol under cursor" $
        runHieDbCli ["point-types", "Module1", "10", "10" ]
          `succeedsWithStdin` unlines
            [ "Bool -> Bool" {- type of `not` function under cursor -}
            ]
      it "Fails for symbols that don't have type associated" $ do
        (exitCode, actualStdout, actualStderr) <- runHieDbCli ["point-types", "Module1", "8", "21"]
        actualStdout `shouldBe` ""
        exitCode `shouldBe` ExitFailure 1
        actualStderr `shouldBe` "No symbols found at (8,21) in Module1\n"

    describe "point-defs" $ do
      it "outputs the location of symbol when definition site can be found is indexed" $
        runHieDbCli ["point-defs", "Module1", "13", "29"]
          `succeedsWithStdin` unlines
            [ "Sub.Module2:8:1-8:8"
            ]
      it "Fails with informative error message when there's no symbol at given point" $ do
        (exitCode, actualStdout, actualStderr) <- runHieDbCli ["point-defs", "Module1", "13", "13"]
        actualStdout `shouldBe` ""
        exitCode `shouldBe` ExitFailure 1
        actualStderr `shouldBe` "No symbols found at (13,13) in Module1\n"

      it "fails with informative error message when the difinition can't be found" $ do
        (exitCode, actualStdout, actualStderr) <- runHieDbCli ["point-defs", "Module1", "13", "24"]
        actualStdout `shouldBe` ""
        exitCode `shouldBe` ExitFailure 1
        actualStderr `shouldBe` "Couldn't find name: $ from module GHC.Base(base)\n"

    describe "point-info" $ do
      it "gives information about symbol at specified location" $
        runHieDbCli ["point-info", "Sub.Module2", "11", "11"]
          `succeedsWithStdin` unlines
            [ "Span: test/data/Sub/Module2.hs:11:7-23"
            , "Constructors: {(ConDeclH98, ConDecl)}"
            , "Identifiers:"
            , "Symbol:c:Data1Constructor1:Sub.Module2:main"
            , "Data1Constructor1 defined at test/data/Sub/Module2.hs:11:7-23"
#if __GLASGOW_HASKELL__ >= 900
            , "    Details:  Nothing {declaration of constructor bound at: test/data/Sub/Module2.hs:11:7-23}"
#else
            , "    IdentifierDetails Nothing {Decl ConDec (Just SrcSpanOneLine \"test/data/Sub/Module2.hs\" 11 7 24)}"
#endif
            , "Types:\n"
            ]
      it "correctly prints type signatures" $
        runHieDbCli ["point-info", "Module1", "10", "10"]
          `succeedsWithStdin` unlines
            [ "Span: test/data/Module1.hs:10:8-10"
            , "Constructors: {(HsVar, HsExpr)}"
            , "Identifiers:"
            , "Symbol:v:not:GHC.Classes:ghc-prim"
            , "not defined at <no location info>"
#if __GLASGOW_HASKELL__ >= 900
            , "    Details:  Just Bool -> Bool {usage}"
#else
            , "    IdentifierDetails Just Bool -> Bool {Use}"
#endif
            , "Types:"
            , "Bool -> Bool"
            , ""
            ]

    describe "name-def" $
      it "lookup definition of name" $
        runHieDbCli ["name-def", "showInt"]
          `succeedsWithStdin` "Sub.Module2:8:1-8:8\n"

    describe "type-def" $
      it "lookup definition of type" $
        runHieDbCli ["type-def", "Data1"]
          `succeedsWithStdin` "Sub.Module2:10:1-12:28\n"

    describe "cat" $
      describe "dumps module source stored in .hie file" $ do
        module1Src <- runIO . readFile $ "test" </> "data" </> "Module1.hs"
        it "when given --hiefile" $ do
          cwd <- getCurrentDirectory
          runHieDbCli ["cat", "--hiefile" , cwd </> testTmp </> "Module1.hie"]
            `succeedsWithStdin` (module1Src <> "\n")
        it "when given module name" $
          runHieDbCli ["cat", "Module1"]
            `succeedsWithStdin` (module1Src <> "\n")

    describe "lookup-hie" $
      it "looks up location of .hie file" $ do
        cwd <- getCurrentDirectory
        runHieDbCli ["lookup-hie", "Module1"]
          `succeedsWithStdin` (cwd </> testTmp </> "Module1.hie\n")

    describe "module-uids" $
      it "lists uids for given module" $
        runHieDbCli ["module-uids", "Module1"]
          `succeedsWithStdin` "main\n"

    describe "ls-exports" $ do
      it "shows exports for Module1" $ do
        cwd <- getCurrentDirectory
        runHieDbCli ["ls-exports", "Module1"] `succeedsWithStdin` unlines
          (fmap (\x -> cwd </> testTmp </> x)
          [ "Module1.hie\tfunction1"
          , "Module1.hie\tfunction2"
          ])

      it "shows all the exports" $ do
        cwd <- getCurrentDirectory
        runHieDbCli ["ls-exports"] `succeedsWithStdinSorted` unlines
          (fmap (\x -> cwd </> testTmp </> x)
          [ "Module1.hie\tfunction1"
          , "Module1.hie\tfunction2"
          , "Sub/Module2.hie\tshowInt"
          , "Sub/Module2.hie\tData1"
          , "Sub/Module2.hie\tData1(Data1Constructor1)"
          , "Sub/Module2.hie\tData1(Data1Constructor2)"
          ])

    describe "rm" $
      it "removes given module from DB" $ do
        runHieDbCli ["rm", "Module1"]
          `succeedsWithStdin` ""
        -- Check with 'ls' comand that there's just one module left
        cwd <- getCurrentDirectory
        runHieDbCli ["ls"] `succeedsWithStdin` (cwd </> testTmp </> "Sub/Module2.hie\tSub.Module2\tmain\n")


succeedsWithStdin :: IO (ExitCode, String, String) -> String -> Expectation
succeedsWithStdin action expectedStdin = do
  (exitCode, actualStdin, _actualStdErr) <- action
  exitCode `shouldBe` ExitSuccess
  actualStdin `shouldBe` expectedStdin


succeedsWithStdinSorted :: IO (ExitCode, String, String) -> String -> Expectation
succeedsWithStdinSorted action expectedStdin = do
  (exitCode, actualStdin, _actualStdErr) <- action
  exitCode `shouldBe` ExitSuccess
  sort (lines actualStdin) `shouldBe` sort (lines expectedStdin)

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

runGhc :: [String] -> IO ()
runGhc args = do
  hc <- fromMaybe ghc <$> lookupEnv "HC"
  callProcess hc args

compileTestModules :: IO ()
compileTestModules = do
  runGhc $
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
withTestDb = withHieDb testDb

runCommandTest :: Command -> IO ()
runCommandTest = runCommand (LibDir libdir) testOpts

testOpts :: Options
testOpts = Options
  { database = testDb
  , trace = False
  , quiet = True
  , colour = False
  , context = Nothing
  , reindex = False
  , keepMissing = False
  , srcBaseDir = Nothing
  , skipIndexingOptions = defaultSkipOptions
  }
