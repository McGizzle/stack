# The Haskell Tool Stack

## Order of Operation

1. Stack.build: build
   -> Stack.Build.ConstructPlan: constructPlan  -> [Plan]())
    * Task within the plan contains the files we want [example]()
   -> Stack.Build.Execute: executePlan

2. Stack.Build.Execute: executePlan
   -> Stack.Build.Execute: executePlan' 
       -> Stack.Build.Execute: toActions -> [Action]
         * Places singleBuild in actionDo, this runs cabal
       

  
  
  ### Plan (contains the files we want)
  ```Haskell
  data Plan = Plan
    { planTasks :: !(Map PackageName Task)
    , planFinals :: !(Map PackageName Task)
    -- ^ Final actions to be taken (test, benchmark, etc)
    , planUnregisterLocal :: !(Map GhcPkgId (PackageIdentifier, Text))
    -- ^ Text is reason we're unregistering, for display only
    , planInstallExes :: !(Map Text InstallLocation)
    -- ^ Executables that should be installed after successful building
    }
    deriving Show
  ```
  ```Haskell
  data Action = Action
    { actionId :: !ActionId
    , actionDeps :: !(Set ActionId)
    , actionDo :: !(ActionContext -> IO ())
        ^ Singlebuild
    , actionConcurrency :: !Concurrency
    }
    
 ```
 ### ConstructPlan: Task
 ```Haskell
Task {taskProvides = "testbuild-0.1.0.0", taskType = TTFiles (LocalPackage {lpPackage = Package {packageName = testbuild, packageVersion = 0.1.0.0, packageLicense = BSD3, packageFiles = <GetPackageFiles>, packageDeps = fromList [(base,IntersectVersionRanges (UnionVersionRanges (ThisVersion (mkVersion [4,7])) (LaterVersion (mkVersion [4,7]))) (EarlierVersion (mkVersion [5])))], packageTools = fromList [], packageAllDeps = fromList [base], packageGhcOptions = [], packageFlags = fromList [], packageDefaultFlags = fromList [], packageLibraries = NoLibraries, packageTests = fromList [], packageBenchmarks = fromList [], packageExes = fromList ["testbuild"], packageOpts = <GetPackageOpts>, packageHasExposedModules = False, packageBuildType = Just Simple, packageSetupDeps = Nothing},
 
lpComponents = fromList [CExe "testbuild"], lpUnbuildable = fromList [], lpWanted = True, lpTestDeps = fromList [(base,IntersectVersionRanges (UnionVersionRanges (ThisVersion (mkVersion [4,7])) (LaterVersion (mkVersion [4,7]))) (EarlierVersion (mkVersion [5])))], lpBenchDeps = fromList [(base,IntersectVersionRanges (UnionVersionRanges (ThisVersion (mkVersion [4,7])) (LaterVersion (mkVersion [4,7]))) (EarlierVersion (mkVersion [5])))], lpTestBench = Nothing, lpDir = "/Users/McGroarty/Documents/College/FYP/testbuild/", 
 
lpCabalFile = "/Users/McGroarty/Documents/College/FYP/testbuild/testbuild.cabal", lpForceDirty = False, lpDirtyFiles = Just (fromList ["README.md","src/Another.hs","src/Main.hs","src/TestFile.hs","testbuild.cabal"]), 
 
lpNewBuildCaches = fromList [(CExe "testbuild",fromList [("/Users/McGroarty/Documents/College/FYP/testbuild/README.md",

FileCacheInfo {fciModTime = ModTime (58140,50295 % 1), fciSize = 12, fciHash = "\241\213\170\212\155Tt\150\192F\SYN\232>\132\196\243#\\tr^\228\&5\229\142\199\228+\250/\214\GS"}),("/Users/McGroarty/Documents/College/FYP/testbuild/src/Another.hs",FileCacheInfo {fciModTime = ModTime (58147,48433 % 1), fciSize = 67, fciHash = "%\rm\n\222\247\141\186\210%ot\233p\219\164m\153\&6\170\nA\166\138&\\\227\SUB\202]\246\250"}),("/Users/McGroarty/Documents/College/FYP/testbuild/src/Main.hs",FileCacheInfo {fciModTime = ModTime (58147,50794 % 1), fciSize = 100, fciHash = "\151\157{;\228eto\STXm\130\185\210>-\234\242\161\149\142w\188\231\198p\208]o@\226\183\195"}),("/Users/McGroarty/Documents/College/FYP/testbuild/src/TestFile.hs",FileCacheInfo {fciModTime = ModTime (58147,48452 % 1), fciSize = 88, fciHash = "\161%\170\234=\211\248@C\209\233@\SYN\142\140h\217\EM\GSI\229G\SIr\SOHl{\186[e\240G"}),("/Users/McGroarty/Documents/College/FYP/testbuild/testbuild.cabal",FileCacheInfo {fciModTime = ModTime (58140,62568 % 1), fciSize = 644, fciHash = "\189\143\168I\218\222\228\224\&2\\8f%\146;\253vT\207'n!\248\139u\DC1\220\148a\b\224\137"})])], 
 
lpComponentFiles = fromList [(CExe "testbuild",fromList ["/Users/McGroarty/Documents/College/FYP/testbuild/README.md","/Users/McGroarty/Documents/College/FYP/testbuild/src/Another.hs","/Users/McGroarty/Documents/College/FYP/testbuild/src/Main.hs","/Users/McGroarty/Documents/College/FYP/testbuild/src/TestFile.hs","/Users/McGroarty/Documents/College/FYP/testbuild/testbuild.cabal"])], lpLocation = PLFilePath "."}) Local, taskConfigOpts = Missing: fromList []. 
Without those: ConfigureOpts {coDirs = ["--user","--package-db=clear","--package-db=global","--package-db=/Users/McGroarty/.stack/snapshots/x86_64-osx/lts-10.3/8.2.2/pkgdb","--package-db=/Users/McGroarty/Documents/College/FYP/testbuild/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/pkgdb","--libdir=/Users/McGroarty/Documents/College/FYP/testbuild/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/lib","--bindir=/Users/McGroarty/Documents/College/FYP/testbuild/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin","--datadir=/Users/McGroarty/Documents/College/FYP/testbuild/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/share","--libexecdir=/Users/McGroarty/Documents/College/FYP/testbuild/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/libexec","--sysconfdir=/Users/McGroarty/Documents/College/FYP/testbuild/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/etc","--docdir=/Users/McGroarty/Documents/College/FYP/testbuild/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/doc/testbuild-0.1.0.0","--htmldir=/Users/McGroarty/Documents/College/FYP/testbuild/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/doc/testbuild-0.1.0.0","--haddockdir=/Users/McGroarty/Documents/College/FYP/testbuild/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/doc/testbuild-0.1.0.0"], coNoDirs = ["--dependency=base=base-4.10.1.0"]}, taskPresent = fromList [("base-4.10.1.0","base-4.10.1.0")], taskAllInOne = True, taskCachePkgSrc = CacheSrcLocal "/Users/McGroarty/Documents/College/FYP/testbuild/", taskAnyMissing = False, taskBuildTypeConfig = False}
   ```
