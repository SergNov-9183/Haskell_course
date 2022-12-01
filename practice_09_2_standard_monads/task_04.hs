import Data.IORef

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
    let step = do
        val <- readIORef ref
        if p val then
            action >> step
        else
            return ()
    step