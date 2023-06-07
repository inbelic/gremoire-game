module Core.History
  ( History
  , Event(..)
  , Alteration(..)
  -- External Interface
  , begin
  , current
  , past
  , record
  , write
  -- Abstract helpers
  , before
  , after
  , mostRecent
  ) where

-- Module to help define some ways of working with the history

import Internal.Game.Types(History(..), Event(..), Alteration(..))

-- Begin with an empty history
begin :: History
begin = History ([], [])

-- Look at the current events only
current :: History -> [Event]
current (History (cur, _)) = cur

-- Look at the past events only
past :: History -> [Event]
past (History (_, past)) = past

-- Record an event to the current history
record :: Event -> History -> History
record event (History (cur, past)) = History (event : cur, past)

-- Write and merge the current history to the past history
write :: History -> History
write (History (cur, past)) = History ([], cur ++ past)


-- Get all the events before an alteration in the past
before :: (Event -> Bool) -> History -> [Event]
before f = dropWhile (not . f) . past

after :: (Event -> Bool) -> History -> [Event]
after f = takeWhile (not . f) . past

mostRecent :: (Event -> Bool) -> History -> Maybe Event
mostRecent f hist = case before f hist of
                  [] -> Nothing
                  (x:_) -> Just x
