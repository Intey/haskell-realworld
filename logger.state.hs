module Logger where

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

instance Functor Logger where
    fmap f (Logger (a, l)) = Logger (f a, (l ++ pure "fmapped"))

instance Applicative Logger where
    pure a = Logger (a, [])
    (Logger (f, l)) <*> (Logger (x, ll)) = Logger ((f x), l ++ ll)


instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (v1, l1) = execLogger m
                  n      = k v1
                  (v2, l2) = execLogger n
              in Logger (v2, l1 ++ l2)

record :: String -> Logger ()
record s = Logger ((), [s])


liftM :: (Monad m) => (a -> b) -> (m a) -> (m b)
liftM f m = m >>= \i -> return (f i)


-- just type of function where we are didn't know what is 's' and 'a'
-- we propagate thinks that we get 's' and return tuple if 'a' and 's'
type SimpleState s a = s -> (a, s)
type StringState a = SimpleState String a

{- 'implementation' of type SimpleState
SimpleState - is function that not binded to types, so we didn't know it's type.
But it's a function thats get 'a', then 's' and return tuple of '(a, s)'
-}
returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)


-- same declaration but in 'carry' manner.
returnSt' :: a -> SimpleState s a
returnSt' a s = (a,s)


{-
So there we bind types and get this function. We prevent user ability to change
Int value But not we has function as defined by 
'(StringState Int) :: String -> (Int, String)'
-}
mytype :: StringState Int -- or SimpleState String Int
mytype str = (0, str)


bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s in
                       k a s'


-- get s and put this as state and...second value..?
getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)

-- real state
newtype State s a = State { runState :: s -> (a, s) }

-- create state from value a. State is a function, that gets second arg (other
-- state?) and return tuple of
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState st f = State $ \s -> let (a', s') = runState st s in runState (f a') s' 
