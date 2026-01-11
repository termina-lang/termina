module EFP.Schedulability.MAST.Printer where

import Prettyprinter
import qualified Data.Text as T
import Utils.Printer
import EFP.Schedulability.MAST.AST
import qualified Data.Map.Strict as M


class MASTPrint a where
  pprint :: a -> DocStyle

parens' :: DocStyle -> DocStyle
parens' b = parens (line <> b <> line)

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

instance MASTPrint MASTModel where
  pprint (MASTModel modelName processingResources schedulers schedServers sharedResources operations transactions) =
    pretty "Model" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Model_Name" <+> pretty "=>" <+> pretty modelName,
        pretty "Model_Date" <+> pretty "=>" <+> pretty "2025-01-01"
    ])) <> semi <> line <> line <> vsep (map pprint (M.elems processingResources))
      <> vsep (map pprint (M.elems schedulers))
      <> vsep (map pprint (M.elems schedServers))
      <> vsep (map pprint (M.elems sharedResources))
      <> vsep (map pprint (M.elems operations))
      <> vsep (map pprint (M.elems transactions)) <> line

instance MASTPrint MASTProcessingResource where
  pprint (MASTRegularProcessor name speedFactor worstISRSwitch maxInterruptPriority minInterruptPriority systemTimer) =
        pretty "Processing_Resource" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Regular_Processor",
        pretty "Name" <+> pretty "=>" <+> pretty name,
        pretty "Speed_Factor" <+> pretty "=>" <+> pretty speedFactor,
        pretty "Worst_ISR_Switch" <+> pretty "=>" <+> pretty worstISRSwitch,
        pretty "Avg_ISR_Switch" <+> pretty "=>" <+> pretty worstISRSwitch,
        pretty "Best_ISR_Switch" <+> pretty "=>" <+> pretty worstISRSwitch,
        pretty "Max_Interrupt_Priority" <+> pretty "=>" <+> pretty maxInterruptPriority,
        pretty "Min_Interrupt_Priority" <+> pretty "=>" <+> pretty minInterruptPriority,
        pretty "System_Timer" <+> pretty "=>" <+> pprint systemTimer
    ])) <> semi <> line <> line

instance MASTPrint MASTSystemTimer where
  pprint (MASTTicker worstOverhead tickInterval) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Ticker",
        pretty "Worst_Overhead" <+> pretty "=>" <+> pretty worstOverhead,
        pretty "Avg_Overhead" <+> pretty "=>" <+> pretty worstOverhead,
        pretty "Best_Overhead" <+> pretty "=>" <+> pretty worstOverhead,
        pretty "Period" <+> pretty "=>" <+> pretty tickInterval
    ]))

instance MASTPrint MASTSchedulingPolicy where
  pprint (MASTFixedPriority worstContextSwitch maxPrio minPrio) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Fixed_Priority",
        pretty "Worst_Context_Switch" <+> pretty "=>" <+> pretty worstContextSwitch,
        pretty "Avg_Context_Switch" <+> pretty "=>" <+> pretty worstContextSwitch,
        pretty "Best_Context_Switch" <+> pretty "=>" <+> pretty worstContextSwitch,
        pretty "Max_Priority" <+> pretty "=>" <+> pretty maxPrio,
        pretty "Min_Priority" <+> pretty "=>" <+> pretty minPrio
    ]))

instance MASTPrint MASTScheduler where
  pprint (MASTPrimaryScheduler name schedPolicy hostPR) =
    pretty "Scheduler" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Primary_Scheduler",
        pretty "Name" <+> pretty "=>" <+> pretty name,
        pretty "Policy" <+> pretty "=>" <+> pprint schedPolicy,
        pretty "Host" <+> pretty "=>" <+> pretty hostPR
    ])) <> semi <> line <> line

instance MASTPrint MASTSchedParameters where
  pprint (MASTFixedPrioPolicy priority) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Fixed_Priority_Policy",
        pretty "The_Priority" <+> pretty "=>" <+> pretty priority,
        pretty "Preassigned" <+> pretty "=>" <+> pretty "Yes"
    ]))
  pprint (MASTIrqFixedPrioPolicy irqPriority) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Interrupt_FP_Policy",
        pretty "The_Priority" <+> pretty "=>" <+> pretty irqPriority,
        pretty "Preassigned" <+> pretty "=>" <+> pretty "Yes"
    ]))

instance MASTPrint MASTSchedulingServer where
  pprint (MASTRegularSchedulingServer name schedParams scheduler) =
    pretty "Scheduling_Server" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Regular",
        pretty "Name" <+> pretty "=>" <+> pretty name,
        pretty "Server_Sched_Parameters" <+> pretty "=>" <+> pprint schedParams,
        pretty "Scheduler" <+> pretty "=>" <+> pretty scheduler
    ])) <> semi <> line

instance MASTPrint MASTSharedResource where
  pprint (MASTImmediateCeilingResource name ceilingPrio) =
    pretty "Shared_Resource" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Immediate_Ceiling_Resource",
        pretty "Name" <+> pretty "=>" <+> pretty name,
        pretty "Ceiling" <+> pretty "=>" <+> pretty ceilingPrio
    ])) <> semi <> line

instance MASTPrint MASTOperation where

  pprint (MASTSimpleOperation opId wcet [] []) =
    pretty "Operation" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Simple",
        pretty "Name" <+> pretty "=>" <+> pretty opId,
        pretty "Worst_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Avg_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Best_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet
    ])) <> semi <> line
  pprint (MASTSimpleOperation opId wcet sharedResToLock []) =
    pretty "Operation" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Simple",
        pretty "Name" <+> pretty "=>" <+> pretty opId,
        pretty "Worst_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Avg_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Best_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Shared_Resources_To_Lock" <+> pretty "=>" <+>
          parens' ((indentTab . align) (vsep (punctuate comma (map pretty sharedResToLock))))
    ])) <> semi <> line
  pprint (MASTSimpleOperation opId wcet [] sharedResToUnlock) =
    pretty "Operation" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Simple",
        pretty "Name" <+> pretty "=>" <+> pretty opId,
        pretty "Worst_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Avg_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Best_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Shared_Resources_To_Unlock" <+> pretty "=>" <+>
          parens' ((indentTab . align) (vsep (punctuate comma (map pretty sharedResToUnlock))))
    ])) <> semi <> line
  pprint (MASTSimpleOperation opId wcet sharedResToLock sharedResToUnlock) =
    pretty "Operation" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Simple",
        pretty "Name" <+> pretty "=>" <+> pretty opId,
        pretty "Worst_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Avg_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Best_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Shared_Resources_To_Lock" <+> pretty "=>" <+>
          parens' ((indentTab . align) (vsep (punctuate comma (map pretty sharedResToLock)))),
        pretty "Shared_Resources_To_Unlock" <+> pretty "=>" <+>
          parens' ((indentTab . align) (vsep (punctuate comma (map pretty sharedResToUnlock))))
    ])) <> semi <> line
  pprint (MASTCompositeOperation opId subOps) =
    pretty "Operation" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Composite",
        pretty "Name" <+> pretty "=>" <+> pretty opId,
        pretty "Composite_Operation_List" <+> pretty "=>" <+>
          parens' ((indentTab . align) (vsep (punctuate comma (map pretty subOps))))
    ])) <> semi <> line
  pprint (MASTEnclosingOperation opId wcet subOps) =
    pretty "Operation" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Enclosing",
        pretty "Name" <+> pretty "=>" <+> pretty opId,
        pretty "Worst_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Avg_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Best_Case_Execution_Time" <+> pretty "=>" <+> pretty wcet,
        pretty "Composite_Operation_List" <+> pretty "=>" <+> parens' ((indentTab . align) (vsep (punctuate comma (map pretty subOps))))
    ])) <> semi <> line

instance MASTPrint MASTEventHandler where
  pprint (MASTTimedActivityEventHandler inputEvent outputEvent actiivityOp schedServer) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "System_Timed_Activity",
        pretty "Input_Event" <+> pretty "=>" <+> pretty inputEvent,
        pretty "Output_Event" <+> pretty "=>" <+> pretty outputEvent,
        pretty "Activity_Operation" <+> pretty "=>" <+> pretty actiivityOp,
        pretty "Activity_Server" <+> pretty "=>" <+> pretty schedServer
    ]))
  pprint (MASTActivityEventHandler inputEvent outputEvent actiivityOp schedServer) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Activity",
        pretty "Input_Event" <+> pretty "=>" <+> pretty inputEvent,
        pretty "Output_Event" <+> pretty "=>" <+> pretty outputEvent,
        pretty "Activity_Operation" <+> pretty "=>" <+> pretty actiivityOp,
        pretty "Activity_Server" <+> pretty "=>" <+> pretty schedServer
    ]))
  pprint (MASTMulticastEventHandler inputEvent outputEvent) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Multicast",
        pretty "Input_Event" <+> pretty "=>" <+> pretty inputEvent,
        pretty "Output_Events_List" <+> pretty "=>" <+> parens' ((indentTab . align) (vsep (punctuate comma (map pretty outputEvent))))
    ]))

instance MASTPrint MASTExternalEvent where
  pprint (MASTPeriodicExternalEvent eventId period) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Periodic",
        pretty "Name" <+> pretty "=>" <+> pretty eventId,
        pretty "Period" <+> pretty "=>" <+> pretty period
    ]))
  pprint (MASTBurstyExternalEvent eventId boundInterval maxEvents) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty "Bursty",
        pretty "Name" <+> pretty "=>" <+> pretty eventId,
        pretty "Bound_Interval" <+> pretty "=>" <+> pretty boundInterval,
        pretty "Max_Arrivals" <+> pretty "=>" <+> pretty maxEvents
    ]))

instance MASTPrint MASTInternalEvent where
  pprint (MASTRegularInternalEvent eventId (Just (MASTGlobalDeadline deadline refEvent))) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
            pretty "Type" <+> pretty "=>" <+> pretty "Regular",
            pretty "Name" <+> pretty "=>" <+> pretty eventId,
            pretty "Timing_Requirements" <+> pretty "=>" <+> parens' ((indentTab . align) (vsep . punctuate comma $ [
                  pretty "Type" <+> pretty "=>" <+> pretty "Hard_Global_Deadline",
                  pretty "Deadline" <+> pretty "=>" <+> pretty deadline,
                  pretty "Referenced_Event" <+> pretty "=>" <+> pretty refEvent
              ]))
        ]))
  pprint (MASTRegularInternalEvent eventId Nothing) =
    parens' ((indentTab . align) (vsep . punctuate comma $ [
            pretty "Type" <+> pretty "=>" <+> pretty "Regular",
            pretty "Name" <+> pretty "=>" <+> pretty eventId
        ]))

instance MASTPrint MASTTransaction where
  pprint (MASTRegularTransaction transId extEvents intEvents evHandlers) =
    pretty "Transaction" <> parens' ((indentTab . align) (vsep . punctuate comma $ [
        pretty "Type" <+> pretty "=>" <+> pretty " Regular",
        pretty "Name" <+> pretty "=>" <+> pretty transId,
        pretty "External_Events" <+> pretty "=>" <+>
          parens' ((indentTab . align) (vsep (punctuate comma (map pprint extEvents)))),
        pretty "Internal_Events" <+> pretty "=>" <+>
          parens' ((indentTab . align) (vsep (punctuate comma (map pprint intEvents)))),
        pretty "Event_Handlers" <+> pretty "=>" <+>
          parens' ((indentTab . align) (vsep (punctuate comma (map pprint evHandlers))))
    ])) <> semi <> line

runMASTPrinter :: MASTModel -> T.Text
runMASTPrinter = render . pprint