module FSM where

-- | Определить язык, который поддерживает заданный автомат.
lang :: (s -> Bool)     -- ^ Предикат конечного состояния.
     -> (s -> [(a, s)]) -- ^ Функция переходов.
     -> s               -- ^ Начальное состояние
     -> [[a]]
lang p t x
  | p x = [[]] -- автомат на конечном состоянии распознаёт только пустое слово
  | otherwise = concat (map g (t x))
  where
    g (c, y) = map (c:) (lang p t y)

-- | Определить все последовательности состояний автомата,
-- которые принимают заданное слово.
states :: Eq a
       => (s -> Bool)     -- ^ Предикат конечного состояния.
       -> (s -> [(a, s)]) -- ^ Функция переходов.
       -> s               -- ^ Начальное состояние.
       -> [a]             -- ^ Слово.
       -> [[s]]
states _ _ _ _ = [] -- реализуйте эту функцию самостоятельно

-- | Состояния.
data State
  = H                       -- начальное состояние
  | S1 | S2 | S3 | S4 | S5  -- промежуточные состояния
  | K                       -- конечное состояние
  deriving (Eq, Show)

-- | Конечный автомат.
data Auto = Auto
  { initialState :: State             -- ^ Начальное состояние.
  , isFinal      :: State -> Bool     -- ^ Предикат конечного состояния.
  , transition   :: State -> [Trans]  -- ^ Функция переходов.
  }

-- | Переход.
type Trans = (Char, State)

-- | Определить язык, который поддерживает заданный автомат.
autoLang :: Auto -> [String]
autoLang auto = lang (isFinal auto) (transition auto) (initialState auto)

-- | Определить все последовательности состояний автомата,
-- которые принимают заданное слово.
autoStates :: Auto -> String -> [[State]]
autoStates auto = states (isFinal auto) (transition auto) (initialState auto)

-- | Пример детерминированного конечного автомата.
sampleDetAuto :: Auto
sampleDetAuto = Auto
  { initialState = H
  , isFinal      = (== K)
  , transition   = g
  }
  where
    g H = [('a', K)]
    g K = []
    g _ = []

-- | Пример недетерминированного конечного автомата.
sampleNonDetAuto :: Auto
sampleNonDetAuto = Auto
  { initialState = S1
  , isFinal      = (== K)
  , transition   = g
  }
  where
    g S1 = [('b', K), ('a', S2), ('a', S1)]
    g S2 = [('b', K), ('a', S1), ('a', S2)]
    g K  = []
    g _  = []

-- | Пример конечного автомата.
sampleAuto :: Auto
sampleAuto = Auto
  { initialState = H
  , isFinal      = (== K)
  , transition   = g
  }
  where
    g H  = [('c', S1), ('c', S3)]
    g S1 = [('c', S2), ('a', S5)]
    g S2 = [('a', K)]
    g S3 = [('a', S4)]
    g S4 = [('c', K), ('b', S4)]
    g S5 = [('b', S1)]
    g K  = []

