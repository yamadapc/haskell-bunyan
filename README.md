haskell-bunyan
==============
`node-bunyan` implementation in Haskell. Needs benchmarking and some caching.

## Usage
```haskell
import System.Log.Bunyan

main :: IO ()
main = do
    logger <- newLogger def { loggerName = "app" }
    ldebug logger [] "Hello"
    linfo logger ["value" .= ("sunny" :: String)] "Hello"
```

Will log:
```jsonl
{"hostname":"Pedros-MacBook-Pro.local","time":"2015-11-01T13:11:01.161Z","msg":"Hello","name":"app","pid":48910,"level":20,"v":0}
{"hostname":"Pedros-MacBook-Pro.local","time":"2015-11-01T13:11:01.164Z","value":"sunny","msg":"Hello","name":"app","pid":48910,"level":30,"v":0}
```

Which with the `bunyan` command-line utility, will be formated as:
![](https://www.dropbox.com/s/01on6brqm9r5ced/Screenshot%202015-11-01%2014.11.56.png?dl=1)

## License
This code is licensed under the MIT license for Pedro Tacla Yamada.
