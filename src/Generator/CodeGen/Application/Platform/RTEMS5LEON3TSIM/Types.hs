{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Platform.RTEMS5LEON3TSIM.Types where
import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Generator.CodeGen.Application.OS.RTEMS.RTEMS5.Types


-- | rtems_isr_entry type
rtems_isr_entry :: CType
rtems_isr_entry = typeDef "rtems_isr_entry"

-- | rtems_status_code __rtems__install_isr(rtems_vector_number vector,
--                                          rtems_isr_entry handler);
__rtems__install_isr :: CExpression
__rtems__install_isr = "__rtems__install_isr" @:
    CTFunction rtems_status_code 
        [
            -- | rtems_vector_number vector
            rtems_vector_number,
            -- | rtems_interrupt_handler handler
            rtems_isr_entry
        ]