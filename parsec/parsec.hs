{-# LANGUAGE RankNTypes
           #-}

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P

-- data State s u = State { stateInput :: s
--                             , statePos :: SourcePos
--                             , stateUser :: u }
-- 
-- data SourcePos = SourcePos 
--                    String  -- ^Name
--                    Int     -- ^Column
--                    Int     -- ^Row
-- 
-- newtype Parsec s u a = 
--   Parsec { unParsec :: forall b. 
--                        State s u
--                     -> (a -> State s u -> ParseError -> b)
--                     -> (ParseError -> b)
--                     -> (a -> State s u -> ParseError -> b)
--                     -> (ParseError -> b)
--                     -> b }
-- 
-- data ParseError = ParseError

--statement = P.sepBy P.whiteSpace
