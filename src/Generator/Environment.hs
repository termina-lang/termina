module Generator.Environment where
import Configuration.Configuration
import Semantic.AST
import Utils.Annotations
import Semantic.Types
import Parser.Types (undocumented)
import ControlFlow.Architecture.Types
import qualified Data.Map.Strict as M
import ControlFlow.Architecture
import Configuration.Platform
import Semantic.Environment (builtinEmitter, builtinType)

-- | Every interrupt of the platform is declared, and the ones the application
-- does not connect are dropped from the architecture
-- ('pruneUnconnectedSystemElements').
interruptGlobalEnv :: Platform -> [GlobalDecl SemanticAnn]
interruptGlobalEnv plt =
    [builtinEmitter irq "Interrupt" | irq <- M.keys (getPlatformInterruptMap plt)]

getPlatformInitialGlobalEnv :: TerminaConfig -> Platform -> [GlobalDecl SemanticAnn]
getPlatformInitialGlobalEnv _ POSIXGCC =
    interruptGlobalEnv POSIXGCC ++
    -- | SystemAPI interface. This interface extends all the system interfaces.
    -- Each target platform should declare its own SystemAPI interface.
    [builtinType (Interface SystemInterface "SystemAPI" ["SysTime", "SysPrint", "SysGetChar"] [] [])]
getPlatformInitialGlobalEnv _ RTEMS5LEON3SFNEXYSA7 =
    interruptGlobalEnv RTEMS5LEON3SFNEXYSA7 ++
    -- | SystemAPI interface. This interface extends all the system interfaces.
    -- Each target platform should declare its own SystemAPI interface.
    [builtinType (Interface SystemInterface "SystemAPI" ["SysTime"] [] [])]
getPlatformInitialGlobalEnv _ FreeRTOS10STM32L432NUCLEOL432KC =
    interruptGlobalEnv FreeRTOS10STM32L432NUCLEOL432KC ++
    -- | SystemAPI interface.
    [builtinType (Interface SystemInterface "SystemAPI" ["SysTime"] [] [])]
getPlatformInitialGlobalEnv _ RTEMS6ZYNQ7000PYNQZ2 =
    interruptGlobalEnv RTEMS6ZYNQ7000PYNQZ2 ++
    -- | SystemAPI interface.
    [builtinType (Interface SystemInterface "SystemAPI" ["SysTime"] [] [])]
getPlatformInitialGlobalEnv _ RTEMS6RV32IMSPIKE =
    interruptGlobalEnv RTEMS6RV32IMSPIKE ++
    -- | SystemAPI interface.
    [builtinType (Interface SystemInterface "SystemAPI" ["SysTime"] [] [])]
getPlatformInitialGlobalEnv _ RTEMS6NOEL32IMARTYA7 =
    interruptGlobalEnv RTEMS6NOEL32IMARTYA7 ++
    -- | SystemAPI interface.
    [builtinType (Interface SystemInterface "SystemAPI" ["SysTime"] [] [])]
getPlatformInitialGlobalEnv _ TestPlatform =
    -- An interrupt so the test harness can exercise its connection errors
    -- (e.g. SE-158/193). It is a name-resolution entry only and is not added
    -- to the program architecture.
    [builtinEmitter "test_irq" "Interrupt",
    -- The SystemAPI of posix, so the tests reach system_entry
     builtinType (Interface SystemInterface "SystemAPI" ["SysTime", "SysPrint", "SysGetChar"] [] [])]

getPlatformInitialProgram :: TerminaConfig -> Platform -> TerminaProgArch SemanticAnn
getPlatformInitialProgram _ TestPlatform = emptyTerminaProgArch
getPlatformInitialProgram _ plt =
    emptyTerminaProgArch {
        emitters = M.union (emitters emptyTerminaProgArch) . M.fromList $
            [(irq, TPInterruptEmitter irq (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) (undocumented Internal))) | irq <- M.keys (getPlatformInterruptMap plt)]
    }
