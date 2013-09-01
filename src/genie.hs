data Unary = Negate | Inverse deriving (Eq, Show)
data Binary = Add | Multiply | Power deriving (Eq, Show)
data Op = UnaryOp Unary | BinaryOp Binary deriving (Eq, Show)
data Push = Const Float | Var deriving (Eq, Show)
data Instr =  InstrOp Op | InstrPush Push deriving (Eq, Show)
type Code = [Instr]
type Func = Float -> Float
type Stack = [Func]

buildFunc :: Code -> Maybe Func
buildFunc code = out where
	stack = buildStack code (Just [])
	out = case stack of
		Nothing -> Nothing
		Just stack' -> Just $ head stack'

buildStack :: Code -> Maybe Stack -> Maybe Stack
buildStack _ Nothing = Nothing
buildStack [] stack = stack
buildStack (instr:rest) (Just stack) = buildStack rest newStack where
	newStack = case instr of
		InstrOp op -> buildStackOp op stack
		InstrPush push -> Just $ buildStackPush push stack

buildStackOp :: Op -> Stack -> Maybe Stack
buildStackOp (UnaryOp op) stack = buildStackUnaryOp op stack
buildStackOp (BinaryOp op) stack = buildStackBinaryOp op stack

buildStackUnaryOp :: Unary -> Stack -> Maybe Stack
buildStackUnaryOp op stack
	| length stack < 1 = Nothing
	| otherwise = Just newStack where
		(a : rest) = stack
		newStack = case op of
			Negate -> (\x -> -1 * (a x)) : rest
			Inverse -> (\x -> 1 / (a x)) : rest

buildStackBinaryOp :: Binary -> Stack -> Maybe Stack
buildStackBinaryOp op stack
	| length stack < 2 = Nothing
	| otherwise = Just newStack where
		(a : b : rest) = stack
		newStack = case op of
			Add -> (\x -> (a x) + (b x)) : rest
			Multiply -> (\x -> (a x) * (b x)) : rest
			Power -> (\x -> (a x) ** (b x)) : rest

buildStackPush :: Push -> Stack -> Stack
buildStackPush (Const c) stack = (\x -> c) : stack
buildStackPush Var stack = (\x -> x) : stack