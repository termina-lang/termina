module Generator.Environment where
import Configuration.Configuration
import Semantic.AST
import Utils.Annotations
import Semantic.Types
import ControlFlow.Architecture.Types
import qualified Data.Map.Strict as M
import ControlFlow.Architecture
import Configuration.Platform

-- | Every interrupt of the platform is declared, and the ones the application
-- does not connect are dropped from the architecture
-- ('pruneUnconnectedSystemElements').
interruptGlobalEnv :: Platform -> [(Identifier, LocatedElement (GEntry SemanticAnn))]
interruptGlobalEnv plt =
    [(irq, LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | irq <- M.keys (getPlatformInterruptMap plt)]

getPlatformInitialGlobalEnv :: TerminaConfig -> Platform -> [(Identifier, LocatedElement (GEntry SemanticAnn))]
getPlatformInitialGlobalEnv _ POSIXGCC =
    interruptGlobalEnv POSIXGCC ++
    -- | SystemAPI interface. This interface extends all the system interfaces.
    -- Each target platform should declare its own SystemAPI interface.
    [("SystemAPI", LocatedElement (GType (Interface SystemInterface "SystemAPI" ["SysTime", "SysPrint", "SysGetChar"] [] [])) Internal)]
getPlatformInitialGlobalEnv _ RTEMS5LEON3SFNEXYSA7 =
    interruptGlobalEnv RTEMS5LEON3SFNEXYSA7 ++
    -- | SystemAPI interface. This interface extends all the system interfaces.
    -- Each target platform should declare its own SystemAPI interface.
    [("SystemAPI", LocatedElement (GType (Interface SystemInterface "SystemAPI" ["SysTime"] [] [])) Internal)]
getPlatformInitialGlobalEnv _ FreeRTOS10STM32L432NUCLEOL432KC =
    interruptGlobalEnv FreeRTOS10STM32L432NUCLEOL432KC ++
    -- | SystemAPI interface.
    [("SystemAPI", LocatedElement (GType (Interface SystemInterface "SystemAPI" ["SysTime"] [] [])) Internal)]
getPlatformInitialGlobalEnv _ RTEMS6ZYNQ7000PYNQZ2 =
    interruptGlobalEnv RTEMS6ZYNQ7000PYNQZ2 ++
    -- | SystemAPI interface.
    [("SystemAPI", LocatedElement (GType (Interface SystemInterface "SystemAPI" ["SysTime"] [] [])) Internal)]
getPlatformInitialGlobalEnv _ TestPlatform =
    -- An interrupt so the test harness can exercise its connection errors
    -- (e.g. SE-158/193). It is a name-resolution entry only and is not added
    -- to the program architecture.
    [("test_irq", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal),
    -- The SystemAPI of posix, so the tests reach system_entry
     ("SystemAPI", LocatedElement (GType (Interface SystemInterface "SystemAPI" ["SysTime", "SysPrint", "SysGetChar"] [] [])) Internal)]

getPlatformInitialProgram :: TerminaConfig -> Platform -> TerminaProgArch SemanticAnn
getPlatformInitialProgram _ TestPlatform = emptyTerminaProgArch
getPlatformInitialProgram _ plt =
    emptyTerminaProgArch {
        emitters = M.union (emitters emptyTerminaProgArch) . M.fromList $
            [(irq, TPInterruptEmitter irq (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | irq <- M.keys (getPlatformInterruptMap plt)]
    }
