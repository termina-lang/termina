module Generator.CodeGen.Application.OS.RTEMS.Utils where
    
import ControlFlow.AST

-- | Returns the value of the "priority" modifier, if present in the list of modifiers.
-- If not, it returns 255, which is the default value for the priority (the lowest).
getPriority :: [Modifier] -> TInteger
getPriority [] = TInteger 255 DecRepr
getPriority ((Modifier "priority" (Just (I priority _))) : _) = priority
getPriority (_ : modifiers) = getPriority modifiers

-- | Returns the value of the "stack_size" modifier, if present in the list of modifiers.
-- If not, it returns 4096, which is the default value for the stack size (RTEMS_MINIUMUM_STACK_SIZE)
getStackSize :: [Modifier] -> TInteger
getStackSize [] = TInteger 4096 DecRepr
getStackSize ((Modifier "stack_size" (Just (I stackSize _))) : _) = stackSize
getStackSize (_ : modifiers) = getStackSize modifiers