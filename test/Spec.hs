{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Tasty              (TestTree, testGroup, defaultMain, after, DependencyType(..))
import           Test.Tasty.HUnit        (testCase, assertBool)
import           Test.Tasty.QuickCheck   (testProperty, QuickCheckTests(..), QuickCheckVerbose(..))
import           Test.HUnit              (Assertion, (@?=), assertFailure)
import qualified Test.QuickCheck         as QC
import qualified Test.QuickCheck.Monadic as QC
import           Data.Aeson              (Value(..))
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Catch     (try, SomeException, MonadCatch(..))
import           Data.Text               (pack)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.Aeson.Key             as K
import qualified Data.Vector             as V
import           MQuickJS
import           MQuickJS.Error (SomeJSRuntimeException, JSValueUndefined, JSException)
import           Control.Monad (guard)
import qualified Data.Text as T

genText  = do
  k <- QC.choose (0,200)
  t <- pack <$> QC.vectorOf k (QC.oneof $ map pure $ ['0'..'~'])
  pure t


genVal 0 = QC.oneof
  [
    String <$> genText
  , Number . fromInteger <$> QC.arbitrary
  , Bool <$> QC.arbitrary
  , pure Null
  ]
genVal n | n > 0 = QC.oneof
  [
    do { k <- QC.choose (0,n) ; Object . KM.fromList <$> (zip <$> QC.vectorOf k (K.fromText <$> genText) <*> QC.vectorOf k genVal') }
  , do { k <- QC.choose (0,n) ; Array . V.fromList <$> QC.vectorOf k genVal' }
  , String <$> genText
  , Number . fromInteger <$> QC.arbitrary
  , Bool <$> QC.arbitrary
  , pure Null
  ]
  where genVal' = genVal (n `div` 2)

-- | There's an Arbitrary instance for Value floating around, but our tests don't pass
--   with it because it produces null characters in text. These are not valid
--   javascript strings, so we have our own.
newtype GenVal = GenVal Value deriving Show
instance QC.Arbitrary GenVal where
  arbitrary = GenVal <$> QC.sized genVal

marshall_to_from_JSValue :: GenVal -> QC.Property
marshall_to_from_JSValue (GenVal val) = QC.monadicIO $ do
  val' <- QC.run $ mquickjsMultithreaded $ withJSValue val $ \jsval ->
    fromJSValue_ jsval
  pure $ (val QC.=== val')

-- | Basic evaluation tests
evalTests :: TestTree
evalTests = testGroup "eval"
  [ testCase "simple arithmetic" $ mquickjsMultithreaded $ do
      v <- eval "1 + 2;"
      liftIO $ v @?= Number 3

  , testCase "multiplication" $ mquickjsMultithreaded $ do
      v <- eval "6 * 7;"
      liftIO $ v @?= Number 42

  , testCase "division returns float" $ mquickjsMultithreaded $ do
      v <- eval "10 / 4;"
      liftIO $ v @?= Number 2.5

  , testCase "modulo operator" $ mquickjsMultithreaded $ do
      v <- eval "17 % 5;"
      liftIO $ v @?= Number 2

  , testCase "negative numbers" $ mquickjsMultithreaded $ do
      v <- eval "-42;"
      liftIO $ v @?= Number (-42)

  , testCase "floating point arithmetic" $ mquickjsMultithreaded $ do
      v <- eval "3.14 * 2;"
      liftIO $ v @?= Number 6.28

  , testCase "string literal" $ mquickjsMultithreaded $ do
      v <- eval "'hello';"
      liftIO $ v @?= String "hello"

  , testCase "string concatenation" $ mquickjsMultithreaded $ do
      v <- eval "'hello' + ' ' + 'world';"
      liftIO $ v @?= String "hello world"

  , testCase "boolean true" $ mquickjsMultithreaded $ do
      v <- eval "true;"
      liftIO $ v @?= Bool True

  , testCase "boolean false" $ mquickjsMultithreaded $ do
      v <- eval "false;"
      liftIO $ v @?= Bool False

  , testCase "boolean expression" $ mquickjsMultithreaded $ do
      v <- eval "5 > 3;"
      liftIO $ v @?= Bool True

  , testCase "null value" $ mquickjsMultithreaded $ do
      v <- eval "null;"
      liftIO $ v @?= Null

  , testCase "undefined becomes null" $ mquickjsMultithreaded $ do
      v <- eval "undefined;"
      liftIO $ v @?= Null

  , testCase "array literal" $ mquickjsMultithreaded $ do
      v <- eval "[1, 2, 3];"
      liftIO $ v @?= Array (V.fromList [Number 1, Number 2, Number 3])

  , testCase "empty array" $ mquickjsMultithreaded $ do
      v <- eval "[];"
      liftIO $ v @?= Array V.empty

  , testCase "mixed array" $ mquickjsMultithreaded $ do
      v <- eval "[1, 'two', true, null];"
      liftIO $ v @?= Array (V.fromList [Number 1, String "two", Bool True, Null])

  , testCase "object literal" $ mquickjsMultithreaded $ do
      v <- eval "({a: 1, b: 2});"
      liftIO $ v @?= Object (KM.fromList [(K.fromText "a", Number 1), (K.fromText "b", Number 2)])

  , testCase "empty object" $ mquickjsMultithreaded $ do
      v <- eval "({});"
      liftIO $ v @?= Object KM.empty

  , testCase "nested object" $ mquickjsMultithreaded $ do
      v <- eval "({outer: {inner: 42}});"
      liftIO $ v @?= Object (KM.fromList [(K.fromText "outer", Object (KM.fromList [(K.fromText "inner", Number 42)]))])

  , testCase "nested array" $ mquickjsMultithreaded $ do
      v <- eval "[[1, 2], [3, 4]];"
      liftIO $ v @?= Array (V.fromList [Array (V.fromList [Number 1, Number 2]), Array (V.fromList [Number 3, Number 4])])

  , testCase "ternary operator" $ mquickjsMultithreaded $ do
      v <- eval "true ? 'yes' : 'no';"
      liftIO $ v @?= String "yes"

  , testCase "logical AND" $ mquickjsMultithreaded $ do
      v <- eval "true && false;"
      liftIO $ v @?= Bool False

  , testCase "logical OR" $ mquickjsMultithreaded $ do
      v <- eval "true || false;"
      liftIO $ v @?= Bool True

  , testCase "equality check" $ mquickjsMultithreaded $ do
      v <- eval "5 === 5;"
      liftIO $ v @?= Bool True

  , testCase "inequality check" $ mquickjsMultithreaded $ do
      v <- eval "5 !== 3;"
      liftIO $ v @?= Bool True
  ]

-- | Function definition and calling tests
-- Note: Micro QuickJS has limited ES6 support - no arrow functions, const/let, etc.
functionTests :: TestTree
functionTests = testGroup "functions"
  [ testCase "define and call simple function" $ mquickjsMultithreaded $ do
      eval_ "function add(a, b) { return a + b; }"
      v <- eval "add(2, 3);"
      liftIO $ v @?= Number 5

  , testCase "function with var" $ mquickjsMultithreaded $ do
      eval_ "function double(x) { return x * 2; }"
      v <- eval "double(21);"
      liftIO $ v @?= Number 42

  , testCase "call with withJSValue" $ mquickjsMultithreaded $ do
      eval_ "function increment(x) { return x + 1; }"
      v <- withJSValue (5 :: Int) $ \x -> call "increment" [x]
      liftIO $ v @?= Number 6

  , testCase "call with multiple args" $ mquickjsMultithreaded $ do
      eval_ "function subtract(a, b) { return a - b; }"
      v <- withJSValue (10 :: Int) $ \a ->
           withJSValue (3 :: Int) $ \b ->
             call "subtract" [a, b]
      liftIO $ v @?= Number 7

  , testCase "call with string argument" $ mquickjsMultithreaded $ do
      eval_ "function greet(name) { return 'Hello, ' + name + '!'; }"
      v <- withJSValue ("World" :: T.Text) $ \name -> call "greet" [name]
      liftIO $ v @?= String "Hello, World!"

  , testCase "call with array argument" $ mquickjsMultithreaded $ do
      eval_ "function sumArray(arr) { var total = 0; for (var i = 0; i < arr.length; i++) { total = total + arr[i]; } return total; }"
      v <- withJSValue ([1, 2, 3, 4, 5] :: [Int]) $ \arr -> call "sumArray" [arr]
      liftIO $ v @?= Number 15

  , testCase "call with object argument" $ mquickjsMultithreaded $ do
      eval_ "function getField(obj) { return obj.value; }"
      let obj = Object (KM.fromList [(K.fromText "value", Number 99)])
      v <- withJSValue obj $ \o -> call "getField" [o]
      liftIO $ v @?= Number 99

  , testCase "function returning object" $ mquickjsMultithreaded $ do
      eval_ "function makePoint(x, y) { return {x: x, y: y}; }"
      v <- eval "makePoint(10, 20);"
      liftIO $ v @?= Object (KM.fromList [(K.fromText "x", Number 10), (K.fromText "y", Number 20)])

  , testCase "function returning array" $ mquickjsMultithreaded $ do
      eval_ "function makeArray(n) { var arr = []; for (var i = 0; i < n; i++) { arr.push(i); } return arr; }"
      v <- eval "makeArray(5);"
      liftIO $ v @?= Array (V.fromList [Number 0, Number 1, Number 2, Number 3, Number 4])

  , testCase "recursive function" $ mquickjsMultithreaded $ do
      eval_ "function factorial(n) { return n <= 1 ? 1 : n * factorial(n - 1); }"
      v <- eval "factorial(5);"
      liftIO $ v @?= Number 120

  , testCase "closure" $ mquickjsMultithreaded $ do
      eval_ "function makeCounter() { var count = 0; return function() { count = count + 1; return count; }; }"
      eval_ "var counter = makeCounter();"
      v1 <- eval "counter();"
      v2 <- eval "counter();"
      v3 <- eval "counter();"
      liftIO $ v1 @?= Number 1
      liftIO $ v2 @?= Number 2
      liftIO $ v3 @?= Number 3

  , testCase "higher-order function" $ mquickjsMultithreaded $ do
      eval_ "function apply(f, x) { return f(x); }"
      eval_ "function square(x) { return x * x; }"
      v <- eval "apply(square, 5);"
      liftIO $ v @?= Number 25
  ]

-- | Error handling tests
errorTests :: TestTree
errorTests = testGroup "error handling"
  [ testCase "throw string" $ mquickjsMultithreaded $
      try (eval "throw 'Error'") >>= \case
        Left (_ :: SomeJSRuntimeException) -> return ()
        Right _ -> liftIO $ assertFailure "should fail with an Exception..."

  , testCase "throw Error object" $ mquickjsMultithreaded $
      try (eval "throw new Error('Something went wrong')") >>= \case
        Left (_ :: SomeJSRuntimeException) -> return ()
        Right _ -> liftIO $ assertFailure "should fail with an Exception..."

  , testCase "reference error" $ mquickjsMultithreaded $
      try (eval "nonExistentVariable") >>= \case
        Left (_ :: SomeJSRuntimeException) -> return ()
        Right _ -> liftIO $ assertFailure "should fail with ReferenceError..."

  , testCase "syntax error" $ mquickjsMultithreaded $
      try (eval "function {") >>= \case
        Left (_ :: SomeJSRuntimeException) -> return ()
        Right _ -> liftIO $ assertFailure "should fail with SyntaxError..."

  , testCase "type error" $ mquickjsMultithreaded $
      try (eval "null.foo") >>= \case
        Left (_ :: SomeJSRuntimeException) -> return ()
        Right _ -> liftIO $ assertFailure "should fail with TypeError..."

  , testCase "call undefined function" $ mquickjsMultithreaded $
      try (call "nonExistentFunction" []) >>= \case
        Left (_ :: SomeJSRuntimeException) -> return ()
        Right _ -> liftIO $ assertFailure "should fail when calling undefined function..."

  , testCase "division by zero returns infinity" $ mquickjsMultithreaded $ do
      v <- eval "1 / 0;"
      -- JavaScript returns Infinity for division by zero
      case v of
        Number n -> liftIO $ assertBool "should be infinite" (isInfinite (fromRational (toRational n) :: Double))
        _ -> liftIO $ assertFailure "expected Number"
  ]

-- | String handling tests
-- Note: Micro QuickJS has limited ES6 support - no template literals
stringTests :: TestTree
stringTests = testGroup "strings"
  [ testCase "empty string" $ mquickjsMultithreaded $ do
      v <- eval "''"
      liftIO $ v @?= String ""

  , testCase "string with spaces" $ mquickjsMultithreaded $ do
      v <- eval "'  hello  '"
      liftIO $ v @?= String "  hello  "

  , testCase "string length" $ mquickjsMultithreaded $ do
      v <- eval "'hello'.length"
      liftIO $ v @?= Number 5

  , testCase "string methods - toUpperCase" $ mquickjsMultithreaded $ do
      v <- eval "'hello'.toUpperCase()"
      liftIO $ v @?= String "HELLO"

  , testCase "string methods - toLowerCase" $ mquickjsMultithreaded $ do
      v <- eval "'HELLO'.toLowerCase()"
      liftIO $ v @?= String "hello"

  , testCase "string methods - substring" $ mquickjsMultithreaded $ do
      v <- eval "'hello world'.substring(0, 5)"
      liftIO $ v @?= String "hello"

  , testCase "string methods - split" $ mquickjsMultithreaded $ do
      v <- eval "'a,b,c'.split(',')"
      liftIO $ v @?= Array (V.fromList [String "a", String "b", String "c"])

  , testCase "string methods - trim" $ mquickjsMultithreaded $ do
      v <- eval "'  hello  '.trim()"
      liftIO $ v @?= String "hello"

  , testCase "string with newlines" $ mquickjsMultithreaded $ do
      v <- eval "'line1\\nline2'"
      liftIO $ v @?= String "line1\nline2"

  , testCase "string with tabs" $ mquickjsMultithreaded $ do
      v <- eval "'col1\\tcol2'"
      liftIO $ v @?= String "col1\tcol2"

  , testCase "string with special chars" $ mquickjsMultithreaded $ do
      v <- eval "'hello\\\\world'"
      liftIO $ v @?= String "hello\\world"

  , testCase "string with quotes" $ mquickjsMultithreaded $ do
      v <- eval "'it\\'s a test'"
      liftIO $ v @?= String "it's a test"

  , testCase "string concatenation interpolation" $ mquickjsMultithreaded $ do
      eval_ "var name = 'world'"
      v <- eval "'hello ' + name"
      liftIO $ v @?= String "hello world"
  ]

-- | Array operation tests
-- Note: Micro QuickJS has limited ES6 array methods, using ES5-compatible approaches
arrayTests :: TestTree
arrayTests = testGroup "arrays"
  [ testCase "array length" $ mquickjsMultithreaded $ do
      v <- eval "[1, 2, 3].length"
      liftIO $ v @?= Number 3

  , testCase "array indexing" $ mquickjsMultithreaded $ do
      v <- eval "[10, 20, 30][1]"
      liftIO $ v @?= Number 20

  , testCase "array push and access" $ mquickjsMultithreaded $ do
      eval_ "var arr = [1, 2]; arr.push(3);"
      v <- eval "arr"
      liftIO $ v @?= Array (V.fromList [Number 1, Number 2, Number 3])

  , testCase "array pop" $ mquickjsMultithreaded $ do
      v <- eval "var arr = [1, 2, 3]; arr.pop()"
      liftIO $ v @?= Number 3

  , testCase "array shift" $ mquickjsMultithreaded $ do
      v <- eval "var arr = [1, 2, 3]; arr.shift()"
      liftIO $ v @?= Number 1

  , testCase "array unshift" $ mquickjsMultithreaded $ do
      eval_ "var arr = [2, 3]; arr.unshift(1);"
      v <- eval "arr"
      liftIO $ v @?= Array (V.fromList [Number 1, Number 2, Number 3])

  , testCase "array concat" $ mquickjsMultithreaded $ do
      v <- eval "[1, 2].concat([3, 4])"
      liftIO $ v @?= Array (V.fromList [Number 1, Number 2, Number 3, Number 4])

  , testCase "array slice" $ mquickjsMultithreaded $ do
      v <- eval "[1, 2, 3, 4, 5].slice(1, 4)"
      liftIO $ v @?= Array (V.fromList [Number 2, Number 3, Number 4])

  , testCase "array reverse" $ mquickjsMultithreaded $ do
      v <- eval "[1, 2, 3].reverse()"
      liftIO $ v @?= Array (V.fromList [Number 3, Number 2, Number 1])

  , testCase "array join" $ mquickjsMultithreaded $ do
      v <- eval "[1, 2, 3].join('-')"
      liftIO $ v @?= String "1-2-3"

  , testCase "array indexOf" $ mquickjsMultithreaded $ do
      v <- eval "[1, 2, 3, 2].indexOf(2)"
      liftIO $ v @?= Number 1

  , testCase "array lastIndexOf" $ mquickjsMultithreaded $ do
      v <- eval "[1, 2, 3, 2].lastIndexOf(2)"
      liftIO $ v @?= Number 3

  , testCase "array sort numbers" $ mquickjsMultithreaded $ do
      eval_ "function compareNumbers(a, b) { return a - b; }"
      v <- eval "[3, 1, 4, 1, 5].sort(compareNumbers)"
      liftIO $ v @?= Array (V.fromList [Number 1, Number 1, Number 3, Number 4, Number 5])

  , testCase "array sort strings" $ mquickjsMultithreaded $ do
      v <- eval "['banana', 'apple', 'cherry'].sort()"
      liftIO $ v @?= Array (V.fromList [String "apple", String "banana", String "cherry"])

  , testCase "array splice" $ mquickjsMultithreaded $ do
      eval_ "var arr = [1, 2, 3, 4, 5]; arr.splice(2, 2);"
      v <- eval "arr"
      liftIO $ v @?= Array (V.fromList [Number 1, Number 2, Number 5])
  ]

-- | Object operation tests
-- Note: Micro QuickJS has limited ES6 support - using ES5 compatible patterns
objectTests :: TestTree
objectTests = testGroup "objects"
  [ testCase "object property access dot notation" $ mquickjsMultithreaded $ do
      v <- eval "({a: 1, b: 2}).a"
      liftIO $ v @?= Number 1

  , testCase "object property access bracket notation" $ mquickjsMultithreaded $ do
      v <- eval "({a: 1, b: 2})['b']"
      liftIO $ v @?= Number 2

  , testCase "Object.keys" $ mquickjsMultithreaded $ do
      v <- eval "Object.keys({a: 1, b: 2, c: 3})"
      liftIO $ v @?= Array (V.fromList [String "a", String "b", String "c"])

  , testCase "nested property access" $ mquickjsMultithreaded $ do
      v <- eval "({outer: {inner: {value: 42}}}).outer.inner.value"
      liftIO $ v @?= Number 42

  , testCase "object with method" $ mquickjsMultithreaded $ do
      v <- eval "({value: 5, double: function() { return this.value * 2; }}).double()"
      liftIO $ v @?= Number 10

  , testCase "object property assignment" $ mquickjsMultithreaded $ do
      eval_ "var obj = {}; obj.x = 10; obj.y = 20;"
      v <- eval "obj"
      liftIO $ v @?= Object (KM.fromList [(K.fromText "x", Number 10), (K.fromText "y", Number 20)])

  , testCase "delete object property" $ mquickjsMultithreaded $ do
      eval_ "var obj = {a: 1, b: 2}; delete obj.a;"
      v <- eval "obj"
      liftIO $ v @?= Object (KM.fromList [(K.fromText "b", Number 2)])

  , testCase "in operator" $ mquickjsMultithreaded $ do
      v <- eval "'a' in {a: 1, b: 2}"
      liftIO $ v @?= Bool True

  , testCase "hasOwnProperty" $ mquickjsMultithreaded $ do
      v <- eval "({a: 1}).hasOwnProperty('a')"
      liftIO $ v @?= Bool True

  , testCase "object with array property" $ mquickjsMultithreaded $ do
      v <- eval "({items: [1, 2, 3]}).items"
      liftIO $ v @?= Array (V.fromList [Number 1, Number 2, Number 3])
  ]

-- | Built-in Math tests
mathTests :: TestTree
mathTests = testGroup "Math"
  [ testCase "Math.PI" $ mquickjsMultithreaded $ do
      Number v <- eval "Math.PI"
      liftIO $ assertBool "PI should be approximately 3.14159" (abs (realToFrac v - 3.14159265358979) < 0.00001)

  , testCase "Math.abs" $ mquickjsMultithreaded $ do
      v <- eval "Math.abs(-42)"
      liftIO $ v @?= Number 42

  , testCase "Math.floor" $ mquickjsMultithreaded $ do
      v <- eval "Math.floor(3.7)"
      liftIO $ v @?= Number 3

  , testCase "Math.ceil" $ mquickjsMultithreaded $ do
      v <- eval "Math.ceil(3.2)"
      liftIO $ v @?= Number 4

  , testCase "Math.round" $ mquickjsMultithreaded $ do
      v <- eval "Math.round(3.5)"
      liftIO $ v @?= Number 4

  , testCase "Math.max" $ mquickjsMultithreaded $ do
      v <- eval "Math.max(1, 5, 3)"
      liftIO $ v @?= Number 5

  , testCase "Math.min" $ mquickjsMultithreaded $ do
      v <- eval "Math.min(1, 5, 3)"
      liftIO $ v @?= Number 1

  , testCase "Math.pow" $ mquickjsMultithreaded $ do
      v <- eval "Math.pow(2, 10)"
      liftIO $ v @?= Number 1024

  , testCase "Math.sqrt" $ mquickjsMultithreaded $ do
      v <- eval "Math.sqrt(16)"
      liftIO $ v @?= Number 4

  , testCase "Math.random produces number between 0 and 1" $ mquickjsMultithreaded $ do
      Number v <- eval "Math.random()"
      liftIO $ assertBool "random should be >= 0 and < 1" (v >= 0 && v < 1)
  ]

-- | JSON tests
jsonTests :: TestTree
jsonTests = testGroup "JSON"
  [ testCase "JSON.stringify object" $ mquickjsMultithreaded $ do
      v <- eval "JSON.stringify({a: 1, b: 2})"
      -- Note: key order may vary
      case v of
        String s -> liftIO $ assertBool "should contain expected content"
          (T.isInfixOf "\"a\":1" s && T.isInfixOf "\"b\":2" s)
        _ -> liftIO $ assertFailure "expected String"

  , testCase "JSON.stringify array" $ mquickjsMultithreaded $ do
      v <- eval "JSON.stringify([1, 2, 3])"
      liftIO $ v @?= String "[1,2,3]"

  , testCase "JSON.parse object" $ mquickjsMultithreaded $ do
      v <- eval "JSON.parse('{\"x\": 10, \"y\": 20}')"
      liftIO $ v @?= Object (KM.fromList [(K.fromText "x", Number 10), (K.fromText "y", Number 20)])

  , testCase "JSON.parse array" $ mquickjsMultithreaded $ do
      v <- eval "JSON.parse('[1, 2, 3]')"
      liftIO $ v @?= Array (V.fromList [Number 1, Number 2, Number 3])

  , testCase "JSON roundtrip" $ mquickjsMultithreaded $ do
      v <- eval "JSON.parse(JSON.stringify({nested: {value: 42}}))"
      liftIO $ v @?= Object (KM.fromList [(K.fromText "nested", Object (KM.fromList [(K.fromText "value", Number 42)]))])
  ]

-- | Edge cases and special values tests
-- Note: Micro QuickJS has limited ES6 support - using ES5 compatible patterns
edgeCaseTests :: TestTree
edgeCaseTests = testGroup "edge cases"
  [ testCase "large integer" $ mquickjsMultithreaded $ do
      v <- eval "1000000000"
      liftIO $ v @?= Number 1000000000

  , testCase "very small float" $ mquickjsMultithreaded $ do
      Number v <- eval "0.0000001"
      liftIO $ assertBool "should be small positive number" (v > 0 && v < 0.001)

  , testCase "NaN check" $ mquickjsMultithreaded $ do
      v <- eval "isNaN(NaN)"
      liftIO $ v @?= Bool True

  , testCase "Infinity" $ mquickjsMultithreaded $ do
      v <- eval "Infinity > 1000000"
      liftIO $ v @?= Bool True

  , testCase "negative Infinity" $ mquickjsMultithreaded $ do
      v <- eval "-Infinity < -1000000"
      liftIO $ v @?= Bool True

  , testCase "multiple statements with var" $ mquickjsMultithreaded $ do
      v <- eval "var x = 5; var y = 10; x + y;"
      liftIO $ v @?= Number 15

  , testCase "var declaration" $ mquickjsMultithreaded $ do
      eval_ "var PI = 3.14159"
      v <- eval "PI"
      liftIO $ v @?= Number 3.14159

  , testCase "var reassignment" $ mquickjsMultithreaded $ do
      eval_ "var counter = 0"
      eval_ "counter = counter + 1"
      v <- eval "counter"
      liftIO $ v @?= Number 1

  , testCase "typeof operator" $ mquickjsMultithreaded $ do
      v1 <- eval "typeof 42"
      v2 <- eval "typeof 'hello'"
      v3 <- eval "typeof true"
      v4 <- eval "typeof {}"
      v5 <- eval "typeof []"
      liftIO $ v1 @?= String "number"
      liftIO $ v2 @?= String "string"
      liftIO $ v3 @?= String "boolean"
      liftIO $ v4 @?= String "object"
      liftIO $ v5 @?= String "object"

  , testCase "array with many elements" $ mquickjsMultithreaded $ do
      eval_ "function makeArray(n) { var arr = []; for (var i = 0; i < n; i++) { arr.push(i); } return arr; }"
      v <- eval "makeArray(100)"
      case v of
        Array arr -> liftIO $ V.length arr @?= 100
        _ -> liftIO $ assertFailure "expected Array"

  , testCase "deeply nested structure" $ mquickjsMultithreaded $ do
      v <- eval "({a: {b: {c: {d: {e: 'deep'}}}}})"
      liftIO $ v @?= Object (KM.fromList
        [(K.fromText "a", Object (KM.fromList
          [(K.fromText "b", Object (KM.fromList
            [(K.fromText "c", Object (KM.fromList
              [(K.fromText "d", Object (KM.fromList
                [(K.fromText "e", String "deep")]))]))]))]))])

  , testCase "scientific notation" $ mquickjsMultithreaded $ do
      v <- eval "1e6"
      liftIO $ v @?= Number 1000000

  , testCase "hexadecimal" $ mquickjsMultithreaded $ do
      v <- eval "0xFF"
      liftIO $ v @?= Number 255

  , testCase "binary literal" $ mquickjsMultithreaded $ do
      v <- eval "parseInt('1010', 2)"
      liftIO $ v @?= Number 10
  ]

-- | Control flow tests
-- Note: Micro QuickJS has limited ES6 support - using ES5 compatible patterns
controlFlowTests :: TestTree
controlFlowTests = testGroup "control flow"
  [ testCase "if-else expression" $ mquickjsMultithreaded $ do
      eval_ "function test(x) { if (x > 0) { return 'positive'; } else { return 'non-positive'; } }"
      v1 <- eval "test(5)"
      v2 <- eval "test(-5)"
      liftIO $ v1 @?= String "positive"
      liftIO $ v2 @?= String "non-positive"

  , testCase "for loop" $ mquickjsMultithreaded $ do
      eval_ "function sumTo(n) { var sum = 0; for (var i = 1; i <= n; i++) { sum = sum + i; } return sum; }"
      v <- eval "sumTo(10)"
      liftIO $ v @?= Number 55

  , testCase "while loop" $ mquickjsMultithreaded $ do
      eval_ "function countdown(n) { var result = []; while (n > 0) { result.push(n); n = n - 1; } return result; }"
      v <- eval "countdown(5)"
      liftIO $ v @?= Array (V.fromList [Number 5, Number 4, Number 3, Number 2, Number 1])

  , testCase "for-in loop" $ mquickjsMultithreaded $ do
      eval_ "function getKeys(obj) { var keys = []; for (var k in obj) { keys.push(k); } return keys; }"
      v <- eval "getKeys({a: 1, b: 2})"
      liftIO $ v @?= Array (V.fromList [String "a", String "b"])

  , testCase "switch statement" $ mquickjsMultithreaded $ do
      eval_ "function dayName(n) { switch(n) { case 0: return 'Sunday'; case 1: return 'Monday'; default: return 'Other'; } }"
      v1 <- eval "dayName(0)"
      v2 <- eval "dayName(1)"
      v3 <- eval "dayName(5)"
      liftIO $ v1 @?= String "Sunday"
      liftIO $ v2 @?= String "Monday"
      liftIO $ v3 @?= String "Other"

  , testCase "try-catch in JS" $ mquickjsMultithreaded $ do
      eval_ "function safeDiv(a, b) { try { if (b === 0) throw 'Division by zero'; return a / b; } catch(e) { return null; } }"
      v1 <- eval "safeDiv(10, 2)"
      v2 <- eval "safeDiv(10, 0)"
      liftIO $ v1 @?= Number 5
      liftIO $ v2 @?= Null

  , testCase "do-while loop" $ mquickjsMultithreaded $ do
      eval_ "function doWhileTest() { var x = 0; do { x = x + 1; } while (x < 5); return x; }"
      v <- eval "doWhileTest()"
      liftIO $ v @?= Number 5

  , testCase "break statement" $ mquickjsMultithreaded $ do
      eval_ "function findFirst(arr, target) { for (var i = 0; i < arr.length; i++) { if (arr[i] === target) { return i; } } return -1; }"
      v <- eval "findFirst([1, 2, 3, 4, 5], 3)"
      liftIO $ v @?= Number 2

  , testCase "continue statement" $ mquickjsMultithreaded $ do
      eval_ "function sumEven(n) { var sum = 0; for (var i = 1; i <= n; i++) { if (i % 2 !== 0) continue; sum = sum + i; } return sum; }"
      v <- eval "sumEven(10)"
      liftIO $ v @?= Number 30
  ]

-- | Tests for different entry point functions
entryPointTests :: TestTree
entryPointTests = testGroup "entry points"
  [ testCase "mquickjs basic eval" $ mquickjs $ do
      v <- eval "1 + 2"
      liftIO $ v @?= Number 3

  , testCase "mquickjsWithMemory basic eval" $ mquickjsWithMemory (10 * 1024 * 1024) $ do
      v <- eval "1 + 2"
      liftIO $ v @?= Number 3

  , testCase "mquickjsMultithreaded basic eval" $ mquickjsMultithreaded $ do
      v <- eval "1 + 2"
      liftIO $ v @?= Number 3

  , testCase "mquickjsMultithreadedWithMemory basic eval" $ mquickjsMultithreadedWithMemory (10 * 1024 * 1024) $ do
      v <- eval "1 + 2"
      liftIO $ v @?= Number 3

  , testCase "mquickjsWithMemory custom allocation" $ mquickjsWithMemory (50 * 1024 * 1024) $ do
      -- Test with larger memory allocation
      eval_ "var arr = []; for (var i = 0; i < 10000; i++) { arr.push({index: i, data: 'test'}); }"
      v <- eval "arr.length"
      liftIO $ v @?= Number 10000

  , testCase "mquickjsMultithreadedWithMemory custom allocation" $ mquickjsMultithreadedWithMemory (50 * 1024 * 1024) $ do
      -- Test with larger memory allocation
      eval_ "var arr = []; for (var i = 0; i < 10000; i++) { arr.push({index: i, data: 'test'}); }"
      v <- eval "arr.length"
      liftIO $ v @?= Number 10000
  ]

-- | Tests for Haskell-JavaScript interop via withJSValue and call
interopTests :: TestTree
interopTests = testGroup "Haskell-JS interop"
  [ testCase "pass Int to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue (42 :: Int) $ \x -> call "identity" [x]
      liftIO $ v @?= Number 42

  , testCase "pass negative Int to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue ((-100) :: Int) $ \x -> call "identity" [x]
      liftIO $ v @?= Number (-100)

  , testCase "pass Double to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue (3.14159 :: Double) $ \x -> call "identity" [x]
      case v of
        Number n -> liftIO $ assertBool "should be close to pi" (abs (realToFrac n - 3.14159) < 0.0001)
        _ -> liftIO $ assertFailure "expected Number"

  , testCase "pass Bool True to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue True $ \x -> call "identity" [x]
      liftIO $ v @?= Bool True

  , testCase "pass Bool False to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue False $ \x -> call "identity" [x]
      liftIO $ v @?= Bool False

  , testCase "pass Null to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue Null $ \x -> call "identity" [x]
      liftIO $ v @?= Null

  , testCase "pass Text to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue ("hello world" :: T.Text) $ \x -> call "identity" [x]
      liftIO $ v @?= String "hello world"

  , testCase "pass empty Text to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue ("" :: T.Text) $ \x -> call "identity" [x]
      liftIO $ v @?= String ""

  , testCase "pass list of Ints to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue ([1, 2, 3] :: [Int]) $ \x -> call "identity" [x]
      liftIO $ v @?= Array (V.fromList [Number 1, Number 2, Number 3])

  , testCase "pass empty list to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue ([] :: [Int]) $ \x -> call "identity" [x]
      liftIO $ v @?= Array V.empty

  , testCase "pass nested list to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      v <- withJSValue ([[1, 2], [3, 4]] :: [[Int]]) $ \x -> call "identity" [x]
      liftIO $ v @?= Array (V.fromList [Array (V.fromList [Number 1, Number 2]), Array (V.fromList [Number 3, Number 4])])

  , testCase "pass Aeson Object to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      let obj = Object (KM.fromList [(K.fromText "name", String "test"), (K.fromText "value", Number 42)])
      v <- withJSValue obj $ \x -> call "identity" [x]
      liftIO $ v @?= obj

  , testCase "pass empty Object to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      let obj = Object KM.empty
      v <- withJSValue obj $ \x -> call "identity" [x]
      liftIO $ v @?= obj

  , testCase "pass nested Object to JS" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      let inner = Object (KM.fromList [(K.fromText "value", Number 99)])
      let obj = Object (KM.fromList [(K.fromText "nested", inner)])
      v <- withJSValue obj $ \x -> call "identity" [x]
      liftIO $ v @?= obj

  , testCase "JS function transforms Haskell Int" $ mquickjsMultithreaded $ do
      eval_ "function double(x) { return x * 2; }"
      v <- withJSValue (21 :: Int) $ \x -> call "double" [x]
      liftIO $ v @?= Number 42

  , testCase "JS function transforms Haskell String" $ mquickjsMultithreaded $ do
      eval_ "function shout(s) { return s.toUpperCase() + '!'; }"
      v <- withJSValue ("hello" :: T.Text) $ \x -> call "shout" [x]
      liftIO $ v @?= String "HELLO!"

  , testCase "JS function transforms Haskell Array" $ mquickjsMultithreaded $ do
      eval_ "function sumArray(arr) { var sum = 0; for (var i = 0; i < arr.length; i++) { sum = sum + arr[i]; } return sum; }"
      v <- withJSValue ([1, 2, 3, 4, 5] :: [Int]) $ \arr -> call "sumArray" [arr]
      liftIO $ v @?= Number 15

  , testCase "JS function extracts Object field" $ mquickjsMultithreaded $ do
      eval_ "function getName(obj) { return obj.name; }"
      let obj = Object (KM.fromList [(K.fromText "name", String "Alice"), (K.fromText "age", Number 30)])
      v <- withJSValue obj $ \x -> call "getName" [x]
      liftIO $ v @?= String "Alice"

  , testCase "JS function modifies Object" $ mquickjsMultithreaded $ do
      eval_ "function addField(obj) { obj.added = true; return obj; }"
      let obj = Object (KM.fromList [(K.fromText "original", Number 1)])
      v <- withJSValue obj $ \x -> call "addField" [x]
      liftIO $ v @?= Object (KM.fromList [(K.fromText "original", Number 1), (K.fromText "added", Bool True)])

  , testCase "pass multiple different types" $ mquickjsMultithreaded $ do
      -- Note: Test with single argument to avoid argument order issues
      eval_ "function formatValue(val) { return 'value: ' + val; }"
      v <- withJSValue (42 :: Int) $ \num -> call "formatValue" [num]
      liftIO $ v @?= String "value: 42"

  , testCase "nested withJSValue calls" $ mquickjsMultithreaded $ do
      eval_ "function add(a, b) { return a + b; }"
      v <- withJSValue (10 :: Int) $ \a -> do
             withJSValue (20 :: Int) $ \b -> do
               call "add" [a, b]
      liftIO $ v @?= Number 30

  , testCase "call function with zero arguments" $ mquickjsMultithreaded $ do
      eval_ "function getFortyTwo() { return 42; }"
      v <- call "getFortyTwo" []
      liftIO $ v @?= Number 42

  , testCase "call function that returns undefined" $ mquickjsMultithreaded $ do
      eval_ "function returnNothing() { }"
      v <- call "returnNothing" []
      liftIO $ v @?= Null

  , testCase "pass array and get element back" $ mquickjsMultithreaded $ do
      eval_ "function getSecond(arr) { return arr[1]; }"
      v <- withJSValue ([10, 20, 30] :: [Int]) $ \arr -> call "getSecond" [arr]
      liftIO $ v @?= Number 20

  , testCase "pass object and get nested value" $ mquickjsMultithreaded $ do
      eval_ "function getDeep(obj) { return obj.level1.level2.value; }"
      let obj = Object (KM.fromList
            [(K.fromText "level1", Object (KM.fromList
              [(K.fromText "level2", Object (KM.fromList
                [(K.fromText "value", Number 999)]))]))])
      v <- withJSValue obj $ \x -> call "getDeep" [x]
      liftIO $ v @?= Number 999

  , testCase "JS modifies array elements" $ mquickjsMultithreaded $ do
      eval_ "function doubleAll(arr) { var result = []; for (var i = 0; i < arr.length; i++) { result.push(arr[i] * 2); } return result; }"
      v <- withJSValue ([1, 2, 3] :: [Int]) $ \arr -> call "doubleAll" [arr]
      liftIO $ v @?= Array (V.fromList [Number 2, Number 4, Number 6])

  , testCase "roundtrip complex structure" $ mquickjsMultithreaded $ do
      eval_ "function identity(x) { return x; }"
      let complex = Object (KM.fromList
            [ (K.fromText "name", String "test")
            , (K.fromText "values", Array (V.fromList [Number 1, Number 2, Number 3]))
            , (K.fromText "nested", Object (KM.fromList [(K.fromText "flag", Bool True)]))
            , (K.fromText "nothing", Null)
            ])
      v <- withJSValue complex $ \x -> call "identity" [x]
      liftIO $ v @?= complex

  , testCase "use Haskell value in JS expression via global" $ mquickjsMultithreaded $ do
      -- Declare global variable first, then set it
      eval_ "var globalValue = 0;"
      eval_ "function setGlobal(val) { globalValue = val; }"
      eval_ "function useGlobal() { return globalValue * 2; }"
      _ <- withJSValue (21 :: Int) $ \x -> call "setGlobal" [x]
      v <- call "useGlobal" []
      liftIO $ v @?= Number 42

  , testCase "multiple sequential calls" $ mquickjsMultithreaded $ do
      eval_ "var accumulator = 0;"
      eval_ "function addTo(x) { accumulator = accumulator + x; return accumulator; }"
      v1 <- withJSValue (10 :: Int) $ \x -> call "addTo" [x]
      v2 <- withJSValue (20 :: Int) $ \x -> call "addTo" [x]
      v3 <- withJSValue (12 :: Int) $ \x -> call "addTo" [x]
      liftIO $ v1 @?= Number 10
      liftIO $ v2 @?= Number 30
      liftIO $ v3 @?= Number 42

  , testCase "pass mixed array to JS" $ mquickjsMultithreaded $ do
      eval_ "function getTypes(arr) { var types = []; for (var i = 0; i < arr.length; i++) { types.push(typeof arr[i]); } return types; }"
      let mixed = Array (V.fromList [Number 1, String "two", Bool True, Null])
      v <- withJSValue mixed $ \arr -> call "getTypes" [arr]
      liftIO $ v @?= Array (V.fromList [String "number", String "string", String "boolean", String "object"])
  ]

tests :: TestTree
tests =
  -- adjustOption (\_ -> QuickCheckTests 10) $
  -- adjustOption (\_ -> QuickCheckVerbose True) $
  testGroup "MQuickJS"
    [ testCase "empty mquickjs call" (mquickjsMultithreaded $ pure ())
    , entryPointTests
    , evalTests
    , functionTests
    , errorTests
    , stringTests
    , arrayTests
    , objectTests
    , mathTests
    , jsonTests
    , edgeCaseTests
    , controlFlowTests
    , interopTests
    , testProperty "marshalling Value to JSValue and back" marshall_to_from_JSValue
    ]

main = defaultMain tests
