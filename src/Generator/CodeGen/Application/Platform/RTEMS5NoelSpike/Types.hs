{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Platform.RTEMS5NoelSpike.Types where
import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Generator.CodeGen.Application.OS.RTEMS.RTEMS5.Types


-- | rtems_interrupt_handler type
rtems_interrupt_handler :: CType
rtems_interrupt_handler = typeDef "rtems_interrupt_handler"

-- | rtems_status_code __rtems__install_isr(rtems_vector_number vector,
--                                          rtems_interrupt_handler handler);
__rtems__install_isr :: CExpression
__rtems__install_isr = "__rtems__install_isr" @:
    CTFunction rtems_status_code 
        [
            -- | rtems_vector_number vector
            rtems_vector_number,
            -- | rtems_interrupt_handler handler
            rtems_interrupt_handler
        ]