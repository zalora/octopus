## octopus

Octopus is a program that lets you run shell commands defined in job files (key job = command value) that serializes jobs in a queue and does per-user throttling (a user can't queue many jobs), meant to be used with sproxy.

It has a fairly simple API:

```haskell
get "/" $ json =<< liftIO jobs

get "/queue/:name" $ run (\name -> command name >>= \c -> atomically $ readTVar cmdQ >>= dumpTQueue . fromJust . M.lookup c ) json

post "/enqueue/:name" $ dispatch cmdQ ownerMap chanSource

get "/attach/:name" $ setHeader "Cache-Control" "no-cache" >> run attachRunner chanSource
```

The goal is to let all users run workers in production in a safe way.

