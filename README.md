# inline-rust

This package allows you to write Rust inline in your Haskell source using
quasiquotes. Here is a short example. For more examples, check out the
[examples](examples) directory.

```haskell
-- examples/Hello.hs
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int

extendContext basic
setCrateRoot []

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  x <- readLn
  y <- [rustIO| i32 {
    let x = $(x: i32);
    println!("Rust: Your number is {}", x);
    x + 1
  } |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y

```

If you want to use this with GHCi, make sure to pass in `-fobject-code`.
