module SMACCMPilot.GCS.Gateway.Queue
  ( QueueInput
  , QueueOutput
  , newQueue
  , queuePop
  , queuePush
  ) where

import Control.Monad
import Control.Concurrent.STM

newtype QueueInput a = QueueInput { unQueueInput :: TQueue a }
newtype QueueOutput a = QueueOutput { unQueueOutput :: TQueue a }

newQueue :: IO (QueueOutput a, QueueInput a)
newQueue = newTQueueIO >>= \q -> return (QueueOutput q, QueueInput q)

queuePop :: QueueInput a -> IO a
queuePop q = atomically (readTQueue (unQueueInput q))

queuePush :: QueueOutput a -> a -> IO ()
queuePush q v = void (atomically (writeTQueue (unQueueOutput q) v))


