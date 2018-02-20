module Stack.Types.Distributed where

data SingleBuild = 
        SingleBuild {
          exeEnv :: ExecuteEnv
        , task :: Task
        , installedMap :: InstalledMap
        , isFinalBuild :: Bool
                    }
