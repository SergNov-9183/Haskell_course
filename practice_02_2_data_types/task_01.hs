data LogLevel = Error | Warning | Info
          deriving (Show,Eq,Ord,Enum)
 
cmp :: LogLevel -> LogLevel -> Ordering
cmp = flip compare