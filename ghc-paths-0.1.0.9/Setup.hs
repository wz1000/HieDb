import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.InstalledPackageInfo
import Distribution.Simple.Program
import qualified Distribution.Simple.PackageIndex as Pkg

import System.Exit
import System.IO
import Data.IORef
import Data.Char
import Data.Maybe
import System.Directory

main = defaultMainWithHooks simpleUserHooks {
                      postConf    = defaultPostConf,
                      preBuild    = readHook,
                      preCopy     = readHook,
                      preInst     = readHook,
                      preHscolour = readHook,
                      preHaddock  = readHook,
                      preReg      = readHook,
                      preUnreg    = readHook
                     }
  where
    defaultPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    defaultPostConf args flags pkgdescr lbi = do
      libdir_ <- getDbProgramOutput (fromFlag (configVerbosity flags))
                     ghcProgram (withPrograms lbi) ["--print-libdir"]
      let libdir = reverse $ dropWhile isSpace $ reverse libdir_

          ghc_pkg = case lookupProgram ghcPkgProgram (withPrograms lbi) of
                          Just p  -> programPath p
                          Nothing -> error "ghc-pkg was not found"
          ghc     = case lookupProgram ghcProgram (withPrograms lbi) of
                          Just p  -> programPath p
                          Nothing -> error "ghc was not found"

          -- figure out docdir from base's haddock-html field
          base_pkg = case Pkg.searchByName (installedPkgs lbi) "base" of
                        Pkg.None -> error "no base package"
                        Pkg.Unambiguous (x:_) -> x
                        _ -> error "base ambiguous"
          base_html = case haddockHTMLs base_pkg of
                        [] -> ""
                        (x:_) -> x
          docdir = fromMaybe base_html $
                        fmap reverse (stripPrefix (reverse "/libraries/base")
                                                  (reverse base_html))
      c_ghc_pkg <- canonicalizePath ghc_pkg
      c_ghc     <- canonicalizePath ghc

      let buildinfo = emptyBuildInfo{
               cppOptions = ["-DGHC_PATHS_GHC_PKG=" ++ show c_ghc_pkg,
                             "-DGHC_PATHS_GHC=" ++ show c_ghc,
                             "-DGHC_PATHS_LIBDIR=" ++ show libdir,
                             "-DGHC_PATHS_DOCDIR=" ++ show docdir ]
             }
      writeFile file (show buildinfo)

    readHook :: Args -> a -> IO HookedBuildInfo
    readHook _ _ = do
      str <- readFile file
      return (Just (read str), [])

file = "ghc-paths.buildinfo"

die :: String -> IO a
die msg = do
  hFlush stdout
  hPutStr stderr msg
  exitWith (ExitFailure 1)

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing
