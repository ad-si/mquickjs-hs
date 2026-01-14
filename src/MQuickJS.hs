{-# LANGUAGE BangPatterns, QuasiQuotes, TemplateHaskell, OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}

{-|
Module      : MQuickJS
Description : Haskell bindings to the [Micro QuickJS](https://github.com/bellard/mquickjs) library
License     : MIT

This is a very basic wrapper for the [Micro QuickJS](https://github.com/bellard/mquickjs) library.

The current functionality includes evaluating JS code, calling a JS function in the global scope
and marshalling 'Value's to and from 'JSValue's.
-}
module MQuickJS (JSValue, JSContextPtr, mquickjs, mquickjsWithMemory, mquickjsMultithreaded, mquickjsMultithreadedWithMemory, call, eval, eval_, withJSValue, fromJSValue_) where

import           Foreign
import           Foreign.C                   (CString, CInt, CDouble, CSize)
import           Data.ByteString             (ByteString, useAsCString, useAsCStringLen, packCString)
import           Data.Text.Encoding          (encodeUtf8)
import qualified Language.C.Inline           as C
import           Control.Monad.Catch         (MonadThrow(..), MonadCatch(..), MonadMask(..), finally)
import           Control.Monad               (when, forM_)
import           Control.Monad.Reader        (MonadReader, runReaderT, ask)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.IO.Unlift     (MonadUnliftIO(..), UnliftIO(..), askUnliftIO)
import           Data.Aeson                  (Key,Value(..), encode, toJSON)
import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Key              as Key
import           Data.Scientific             (fromFloatDigits, toRealFloat, toBoundedInteger, isInteger)
import           Data.Text                   (Text)
import           Data.Vector                 (fromList, imapM_)
import           Data.Aeson.KeyMap         (KeyMap, empty, insert, toList)
import           Data.String.Conv            (toS)
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
import           Control.Concurrent          (rtsSupportsBoundThreads, runInBoundThread)

import           MQuickJS.Types
import           MQuickJS.Error


C.context mquickjsCtx
C.include "<stddef.h>"
C.include "mquickjs.h"

-- Reference to the stdlib defined in example_stdlib.c
C.verbatim "extern const JSSTDLibraryDef js_stdlib;"

foreign import ccall "JS_NewContext"
  jsNewContext :: Ptr Word8 -> CSize -> Ptr () -> IO (Ptr JSContext)

foreign import ccall "JS_FreeContext"
  jsFreeContext :: Ptr JSContext -> IO ()



-- Micro QuickJS uses automatic GC, no manual reference counting needed
jsFreeValue :: JSContextPtr -> JSValue -> IO ()
jsFreeValue _ _ = return ()  -- No-op in Micro QuickJS



type JSContextPtr = Ptr JSContext
type JSValueConstPtr = Ptr JSValueConst

jsIs_ :: (MonadIO m, Storable p, Eq n, Num n) => p -> (Ptr p -> IO n) -> m Bool
jsIs_ val fun = do
  b <- liftIO $ with val fun
  return $ b == 1

-- jsIsNumber :: MonadIO m => JSValue -> m Bool
-- jsIsNumber val = jsIs_ val $ \valPtr -> [C.block| int { return JS_IsNumber(*$(JSValueConst *valPtr)); } |]

-- Micro QuickJS uses JS_GetClassID instead of JS_IsArray
jsIsArray :: MonadIO m => JSContextPtr -> JSValue -> m Bool
jsIsArray ctxPtr val = do
  classId <- liftIO $ [C.block| int { return JS_GetClassID($(JSContext *ctxPtr), $(JSValue val)); } |]
  return $ classId == 1  -- JS_CLASS_ARRAY = 1

-- Micro QuickJS uses JS_GetClassID for Date detection (JS_CLASS_DATE = 7)
jsIsDate :: MonadIO m => JSContextPtr -> JSValue -> m Bool
jsIsDate ctxPtr val = do
  classId <- liftIO $ [C.block| int { return JS_GetClassID($(JSContext *ctxPtr), $(JSValue val)); } |]
  return $ classId == 7  -- JS_CLASS_DATE = 7


jsIsTryAll :: MonadThrow m =>
  JSValue -> [JSValue -> m Bool] -> [JSTypeEnum] -> JSTypeEnum -> m JSTypeEnum
jsIsTryAll _ [] _ def = return def
jsIsTryAll jsval (f:funs)(l:lbls) def = do
  b <- f jsval
  if b then return l else jsIsTryAll jsval funs lbls def
jsIsTryAll _ _ _ _ = throwM $ InternalError $ "jsIsTryAll_ unreachable case"


-- Micro QuickJS type detection - simpler approach using helper functions
jsIs :: (MonadIO m, MonadThrow m) => JSContextPtr -> JSValue -> m JSTypeEnum
jsIs ctx jsval = liftIO $ do
  -- Check for exception first
  isException <- [C.block| int { return JS_IsException($(JSValue jsval)); } |]
  if isException /= 0 then return $ JSTypeFromTag JSTagException
  else do
    -- Check for null/undefined
    isNull <- [C.block| int { return $(JSValue jsval) == JS_NULL; } |]
    if isNull /= 0 then return $ JSTypeFromTag JSTagNull
    else do
      isUndef <- [C.block| int { return $(JSValue jsval) == JS_UNDEFINED; } |]
      if isUndef /= 0 then return $ JSTypeFromTag JSTagUndefined
      else do
        -- Check for boolean
        isBoolTrue <- [C.block| int { return $(JSValue jsval) == JS_TRUE; } |]
        isBoolFalse <- [C.block| int { return $(JSValue jsval) == JS_FALSE; } |]
        if isBoolTrue /= 0 || isBoolFalse /= 0 then return $ JSTypeFromTag JSTagBool
        else do
          -- Check for number (int or float)
          isNum <- [C.block| int { return JS_IsNumber($(JSContext *ctx), $(JSValue jsval)); } |]
          if isNum /= 0 then return JSIsNumber
          else do
            -- Check for string
            isStr <- [C.block| int { return JS_IsString($(JSContext *ctx), $(JSValue jsval)); } |]
            if isStr /= 0 then return $ JSTypeFromTag JSTagPtr  -- Using JSTagPtr for strings
            else do
              -- Check class ID for arrays, dates, objects
              classId <- [C.block| int { return JS_GetClassID($(JSContext *ctx), $(JSValue jsval)); } |]
              case classId of
                1 -> return JSIsArray   -- JS_CLASS_ARRAY
                7 -> return JSIsDate    -- JS_CLASS_DATE
                _ -> return $ JSTypeFromTag JSTagPtr  -- Generic object



-- Micro QuickJS null value constant
jsNullValue :: JSValue
jsNullValue = 7  -- JS_NULL = JS_VALUE_MAKE_SPECIAL(JS_TAG_NULL, 0) = 7

-- Micro QuickJS: JS_NewBool doesn't need ctx parameter
jsNewBool :: JSContextPtr -> Bool -> IO JSValue
jsNewBool _ bool = do
  let b = if bool then 1 else 0
  return $ [C.pure| JSValue { JS_NewBool($(int b)) } |]

jsNewFloat64 :: JSContextPtr -> CDouble -> IO JSValue
jsNewFloat64 ctxPtr num =
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewFloat64($(JSContext *ctxPtr), $(double num)); } |]

jsNewInt64 :: JSContextPtr -> Int64 -> IO JSValue
jsNewInt64 ctxPtr num = do
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewInt64($(JSContext *ctxPtr), $(int64_t num)); } |]

jsNewString :: JSContextPtr -> ByteString -> IO JSValue
jsNewString ctxPtr s = C.withPtr_ $ \ptr -> useAsCStringLen s $ \(cstringPtr, cstringLen) -> do
  let len = fromIntegral cstringLen
  [C.block| void { *$(JSValue *ptr) = JS_NewStringLen($(JSContext *ctxPtr), $(const char *cstringPtr), $(size_t len)); } |]



checkIsException :: (MonadThrow m, MonadIO m) => Text -> JSContextPtr -> JSValue -> m ()
checkIsException loc ctxPtr val = do
  isEx <- liftIO $ [C.block| int { return JS_IsException($(JSValue val)); } |]
  when (isEx /= 0) $ do
    err <- getErrorMessage ctxPtr
    throwM $ JSException loc err



jsonToJSValue :: (MonadThrow m, MonadIO m) => JSContextPtr -> Value -> m JSValue
jsonToJSValue _ Null = pure jsNullValue
jsonToJSValue ctx (Bool b) = liftIO $ jsNewBool ctx b
jsonToJSValue ctx (Number n) =
  if not (isInteger n) then liftIO $ jsNewFloat64 ctx (toRealFloat n)
  else case toBoundedInteger n of
    Just i -> liftIO $ jsNewInt64 ctx i
    Nothing -> throwM $ InternalError "Value does not fit in Int64"
jsonToJSValue ctx (String s) = liftIO $ jsNewString ctx $ toS s
jsonToJSValue ctxPtr (Array xs) = do
  -- Micro QuickJS: JS_NewArray takes initial length parameter
  arrVal <- liftIO $ [C.block| JSValue { return JS_NewArray($(JSContext *ctxPtr), 0); } |]

  checkIsException "jsonToJSValue/Array/1" ctxPtr arrVal

  flip imapM_ xs $ \index value -> do
    val <- jsonToJSValue ctxPtr value
    checkIsException "jsonToJSValue/Array/2" ctxPtr val

    let idx = fromIntegral index
    -- Micro QuickJS: JS_SetPropertyUint32 returns JSValue (exception or undefined)
    res <- liftIO $ [C.block| JSValue {
      return JS_SetPropertyUint32($(JSContext *ctxPtr), $(JSValue arrVal), $(uint32_t idx), $(JSValue val));
    } |]
    checkIsException "jsonToJSValue/Array/set" ctxPtr res

  return arrVal
jsonToJSValue ctxPtr (Object o) = do
  objVal <- liftIO $ [C.block| JSValue { return JS_NewObject($(JSContext *ctxPtr)); } |]

  checkIsException "jsonToJSValue/Object/1" ctxPtr objVal

  forM_ (toList o) $ \(key,value) -> do
    val <- jsonToJSValue ctxPtr value
    checkIsException "jsonToJSValue/Object/2" ctxPtr val

    -- Micro QuickJS: JS_SetPropertyStr returns JSValue (exception or undefined)
    res <- liftIO $ useAsCString (encodeUtf8 $ Key.toText key) $ \cstringPtr ->
      [C.block| JSValue {
        return JS_SetPropertyStr($(JSContext *ctxPtr), $(JSValue objVal), $(const char *cstringPtr), $(JSValue val));
      } |]
    checkIsException "jsonToJSValue/Object/set" ctxPtr res

  return objVal


-- Micro QuickJS: Use JS_ToNumber for boolean conversion (no JS_ToBool)
jsToBool :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m Bool
jsToBool ctxPtr val = do
    (res, code) <- liftIO $ C.withPtr $ \doublePtr ->
      [C.block| int { return JS_ToNumber($(JSContext *ctxPtr), $(double *doublePtr), $(JSValue val)); } |]
    if code == 0 then return (res /= 0)
    else getErrorMessage ctxPtr >>= throwM . JSException "jsToBool"

-- Micro QuickJS: Use JS_ToInt32 (no JS_ToInt64)
jsToInt64 :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m Int64
jsToInt64 ctxPtr val = do
  (res, code) <- liftIO $ C.withPtr $ \intPtr ->
    [C.block| int { return JS_ToInt32($(JSContext *ctxPtr), $(int *intPtr), $(JSValue val)); } |]
  if code == 0 then return (fromIntegral res)
  else getErrorMessage ctxPtr >>= throwM . JSException "jsToInt64"

-- Micro QuickJS: JS_ToNumber instead of JS_ToFloat64
jsToFloat64 :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m CDouble
jsToFloat64 ctxPtr val = do
  (res, code) <- liftIO $ C.withPtr $ \doublePtr ->
    [C.block| int { return JS_ToNumber($(JSContext *ctxPtr), $(double *doublePtr), $(JSValue val)); } |]
  if code == 0 then return res
  else getErrorMessage ctxPtr >>= throwM . JSException "jsToFloat64"



-- Micro QuickJS: JS_ToCString requires JSCStringBuf parameter, no JS_FreeCString needed
jsToString :: MonadIO m => JSContextPtr -> JSValue -> m ByteString
jsToString ctxPtr val = liftIO $ do
    -- JSCStringBuf is 5 bytes
    allocaBytes 5 $ \bufPtr -> do
      cstring <- [C.block| const char * {
        return JS_ToCString($(JSContext *ctxPtr), $(JSValue val), (JSCStringBuf *)$(void *bufPtr));
      } |]
      if cstring == nullPtr then return ""
      else packCString cstring  -- No free needed in Micro QuickJS


jsToJSON :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> m Value
jsToJSON ctx jsval = do
  ty <- jsIs ctx jsval
  case ty of
    JSTypeFromTag JSTagException -> do
      err <- getErrorMessage ctx
      throwM $ JSException "jsToJSON/JSTagException" err
    JSTypeFromTag JSTagNull -> return Null
    JSTypeFromTag JSTagUndefined -> return Null
    JSTypeFromTag JSTagBool -> do
      b <- jsToBool ctx jsval
      return $ Bool b
    JSIsNumber -> do
      n <- jsToFloat64 ctx jsval
      return $ Number $ fromFloatDigits n
    JSIsArray -> do
      len <- do
        lenVal <- jsGetPropertyStr ctx jsval "length"
        len' <- jsToInt64 ctx lenVal
        return len'
      vs <- jsArrayToJSON ctx jsval 0 (fromIntegral len)
      return $ Array $ fromList vs
    JSIsDate -> do
      -- Micro QuickJS: Use stack-based calling for Date.getTime()
      timestampRaw <- liftIO $ [C.block| JSValue {
        JSValue date = $(JSValue jsval);
        JSContext *ctx = $(JSContext *ctx);
        JSValue getter = JS_GetPropertyStr(ctx, date, "getTime");
        // Stack-based call: push this, func, then call
        JS_PushArg(ctx, date);      // this
        JS_PushArg(ctx, getter);    // func
        return JS_Call(ctx, 0);     // 0 args
      } |]
      checkIsException "jsToJSON/Date" ctx timestampRaw
      timestamp <- jsToFloat64 ctx timestampRaw
      return $ toJSON $ posixSecondsToUTCTime $ realToFrac $ timestamp / 1000
    JSTypeFromTag JSTagPtr -> do
      -- JSTagPtr can be string or object - check which one
      isStr <- liftIO $ [C.block| int { return JS_IsString($(JSContext *ctx), $(JSValue jsval)); } |]
      if isStr /= 0 then do
        s <- jsToString ctx jsval
        return $ String $ toS s
      else do
        o <- jsObjectToJSON ctx jsval
        return $ Object o
    JSTypeFromTag f -> throwM $ UnsupportedTypeTag f
    JSIsError -> throwM $ InternalError "JSIsError unreachable"


jsArrayToJSON :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> Int -> Int -> m [Value]
jsArrayToJSON ctxPtr jsval index len =
  if index < len then do
    v <- do
      let idx = fromIntegral index
      val <- liftIO $ [C.block| JSValue {
        return JS_GetPropertyUint32($(JSContext *ctxPtr), $(JSValue jsval), $(uint32_t idx));
      } |]

      checkIsException "jsArrayToJSON" ctxPtr val
      jsToJSON ctxPtr val

    vs <- jsArrayToJSON ctxPtr jsval (index+1) len
    return $ v:vs
  else return []






forLoop :: (Num a, Ord a, Monad m) => a -> (a -> m ()) -> m ()
forLoop end f = go 0
  where
    go !x | x < end   = f x >> go (x+1)
          | otherwise = return ()




-- Micro QuickJS: Use JavaScript Object.keys() for property enumeration
-- since JS_GetOwnPropertyNames is not available in the public API
jsObjectToJSON :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> m (KeyMap Value)
jsObjectToJSON ctxPtr obj = do
    -- Get Object.keys(obj) via JavaScript evaluation
    keysArray <- liftIO $ [C.block| JSValue {
      JSContext *ctx = $(JSContext *ctxPtr);
      JSValue obj = $(JSValue obj);

      // Get Object constructor from global
      JSValue global = JS_GetGlobalObject(ctx);
      JSValue objectCtor = JS_GetPropertyStr(ctx, global, "Object");
      JSValue keysFunc = JS_GetPropertyStr(ctx, objectCtor, "keys");

      // Stack-based call: Object.keys(obj)
      JS_PushArg(ctx, obj);        // arg
      JS_PushArg(ctx, keysFunc);   // func
      JS_PushArg(ctx, objectCtor); // this
      return JS_Call(ctx, 1);      // 1 argument
    } |]

    checkIsException "jsObjectToJSON/keys" ctxPtr keysArray

    -- Get array length
    lenVal <- jsGetPropertyStr ctxPtr keysArray "length"
    len <- jsToInt64 ctxPtr lenVal

    -- Iterate through keys
    collectProps ctxPtr obj keysArray 0 (fromIntegral len)
  where
    collectProps :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> JSValue -> Int -> Int -> m (KeyMap Value)
    collectProps ctx objVal keysArr !index end
      | index < end = do
          let idx = fromIntegral index

          -- Get key at index
          keyVal <- liftIO $ [C.block| JSValue {
            return JS_GetPropertyUint32($(JSContext *ctx), $(JSValue keysArr), $(uint32_t idx));
          } |]
          checkIsException "jsObjectToJSON/getKey" ctx keyVal
          keyStr <- jsToString ctx keyVal

          -- Get property value
          propVal <- liftIO $ useAsCString keyStr $ \ckey ->
            [C.block| JSValue {
              return JS_GetPropertyStr($(JSContext *ctx), $(JSValue objVal), $(const char *ckey));
            } |]
          checkIsException "jsObjectToJSON/getProp" ctx propVal
          val <- jsToJSON ctx propVal

          rest <- collectProps ctx objVal keysArr (index + 1) end
          return $ insert (Key.fromText $ toS keyStr) val rest

      | otherwise = return empty



getErrorMessage :: MonadIO m => JSContextPtr -> m Text
getErrorMessage ctxPtr = liftIO $ do
  ex <- [C.block| JSValue { return JS_GetException($(JSContext *ctxPtr)); } |]
  res <- jsToString ctxPtr ex
  return $ toS res



jsGetPropertyStr :: MonadIO m => JSContextPtr -> JSValue -> ByteString -> m JSValue
jsGetPropertyStr ctxPtr val str = liftIO $
  useAsCString str $ \prop ->
    [C.block| JSValue { return JS_GetPropertyStr($(JSContext *ctxPtr), $(JSValue val), $(const char *prop)); } |]


-- Micro QuickJS: Stack-based function calling
-- Push args in reverse order, then func, then this, then call with argc
-- See mquickjs.c: arg[n-1] first, then ..., then arg[0], then func, then this_obj
jsCall :: JSContextPtr -> JSValue -> CInt -> (Ptr JSValue) -> IO JSValue
jsCall ctxt fun_obj argc argv = do
  -- Push arguments in reverse order (last arg first), then func, then this (JS_NULL)
  forM_ (reverse [0..(fromIntegral argc - 1)]) $ \i -> do
    arg <- peekElemOff argv i
    let argVal = arg
    [C.block| void {
      JSContext *ctx = $(JSContext *ctxt);
      JSValue v = $(JSValue argVal);
      JS_PushArg(ctx, v);
    } |]
  let funVal = fun_obj
  [C.block| void {
    JSContext *ctx = $(JSContext *ctxt);
    JS_PushArg(ctx, $(JSValue funVal));
    JS_PushArg(ctx, JS_NULL);
  } |]
  [C.block| JSValue { return JS_Call($(JSContext *ctxt), $(int argc)); } |]


jsEval :: JSContextPtr -> CString -> CSize -> CString -> CInt -> IO JSValue
jsEval ctxPtr input input_len filename eval_flags =
  [C.block| JSValue { return JS_Eval($(JSContext *ctxPtr), $(const char *input), $(size_t input_len), $(const char *filename), $(int eval_flags)); } |]


evalRaw :: JSContextPtr -> JSEvalType -> ByteString -> IO JSValue
evalRaw ctx eTyp code =
    useAsCString "script.js" $ \cfilename ->
        useAsCStringLen code $ \(ccode, ccode_len) ->
            jsEval ctx ccode (fromIntegral ccode_len) cfilename (toCType eTyp)




evalAs :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m) => JSEvalType -> ByteString -> m Value
evalAs eTyp code = do
  ctx <- ask
  val <- liftIO $ evalRaw ctx eTyp code
  -- checkIsException "evalAs" ctx val
  jsToJSON ctx val `finally` freeJSValue val



{-|
Evaluates the given string and returns a 'Value' (if the result can be converted).
-}
eval :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m) => ByteString -> m Value
eval = evalAs WithRetval  -- Micro QuickJS needs JS_EVAL_RETVAL flag to return the value

evalAs_ :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => JSEvalType -> ByteString -> m ()
evalAs_ eTyp code = do
  ctx <- ask
  val <- liftIO $ evalRaw ctx eTyp code
  checkIsException "evalAs_" ctx val
  freeJSValue val



{-|
More efficient than 'eval' if we don't care about the value of the expression,
e.g. if we are evaluating a function definition or performing other side-effects such as
printing to console/modifying state.
-}
eval_ :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => ByteString -> m ()
eval_ = evalAs_ Global


fromJSValue_ :: (MonadCatch m, MonadReader JSContextPtr m, MonadIO m) => JSValue -> m Value
fromJSValue_ val = do
  ctx <- ask
  jsToJSON ctx val



-- fromJSValue :: (Aeson.FromJSON a, MonadCatch m, MonadReader JSContextPtr m, MonadIO m) => JSValue -> m a
-- fromJSValue val = do
--   jsonval <- fromJSValue_ val

--   case Aeson.fromJSON jsonval of
--     Aeson.Success a -> return a
--     Aeson.Error err -> throwM $ InternalError err



{-|
Takes a value with a defined 'ToJSON' instance. This value is marshalled to a 'JSValue'
and passed as an argument to the callback function, provided as the second argument to 'withJSValue'
-}
withJSValue :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m, Aeson.ToJSON a) => a -> (JSValue -> m b) -> m b
withJSValue v f = do

  ctx <- ask
  val <- jsonToJSValue ctx (Aeson.toJSON v)
  f val `finally` freeJSValue val




callRaw :: (MonadThrow m, MonadIO m) => JSContextPtr -> ByteString -> [JSValue] -> m JSValue
callRaw ctxPtr funName args = do
    globalObject <- liftIO $ [C.block| JSValue { return JS_GetGlobalObject($(JSContext *ctxPtr)); } |]

    fun <- jsGetPropertyStr ctxPtr globalObject funName

    ty <- jsIs ctxPtr fun
    case ty of
      JSTypeFromTag JSTagException -> do
        err <- getErrorMessage ctxPtr
        throwM $ JSException "callRaw" err
      JSTypeFromTag JSTagUndefined -> throwM $ JSValueUndefined $ toS funName
      JSTypeFromTag JSTagPtr -> do
        -- Micro QuickJS: Stack-based calling
        res <- liftIO $ withArrayLen args $ \len argv -> jsCall ctxPtr fun (fromIntegral len) argv
        return res
      _ -> throwM $ JSValueIncorrectType {name = toS funName, expected = JSTypeFromTag JSTagPtr, found = ty }


-- call :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => String -> [JSValue] -> m JSValue
-- call funName args = do
--   ctx <- ask
--   val <- callRaw ctx funName args
--   checkIsException ctx val
--   return val



call :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m) => ByteString -> [JSValue] -> m Value
call funName args = do
  ctx <- ask
  val <- callRaw ctx funName args
  jsToJSON ctx val `finally` freeJSValue val


freeJSValue :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => JSValue -> m ()
freeJSValue val = do
  ctx <- ask
  liftIO $ jsFreeValue ctx val

{-|
This function initialises a new JS runtime and performs the given computation within this context.

For example, we can evaluate an expression:

>mquickjs $ do
>  res <- eval "1+2"
>  liftIO $ print res

Declare a function and call it on an argument:

>mquickjs $ do
>  _ <- eval_ "f = (x) => x+1"
>  res <- eval "f(2)"
>  liftIO $ print res

Pass a Haskell value to the JS runtime:

>mquickjs $ do
>  _ <- eval_ "f = (x) => x+1"
>  res <- withJSValue (3::Int) $ \x -> call "f" [x]
>  liftIO $ print res

-}
mquickjs :: MonadIO m => ReaderT (Ptr JSContext) m b -> m b
mquickjs = mquickjsWithMemory (10 * 1024 * 1024)  -- 10MB default

mquickjsWithMemory :: MonadIO m => Int -> ReaderT (Ptr JSContext) m b -> m b
mquickjsWithMemory memSize f = do
  (memBuf, ctx) <- liftIO $ do
    _memBuf <- mallocBytes memSize
    _ctx <- [C.block| JSContext * {
      return JS_NewContext($(uint8_t *_memBuf), $(size_t memSizeC), &js_stdlib);
    } |]
    return (_memBuf, _ctx)

  res <- runReaderT f ctx
  cleanup memBuf ctx
  return res
  where
    memSizeC = fromIntegral memSize
    cleanup memBuf ctx = liftIO $ do
      jsFreeContext ctx
      free memBuf

{-|
This env differs from regular 'mquickjs', in that it wraps the computation in the 'runInBoundThread' function.
This is needed when running the Haskell program mutithreaded (e.g. when using the testing framework Tasty),
since mquickjs does not like being called from an OS thread other than the one it was started in.
Because Haskell uses lightweight threads, this might happen if threaded mode is enabled, as is the case in Tasty.
This problem does not occur when running via Main.hs, if compiled as single threaded...
For more info see the paper [Extending the Haskell Foreign Function Interface with Concurrency](https://simonmar.github.io/bib/papers/conc-ffi.pdf)
-}
mquickjsMultithreaded :: MonadUnliftIO m => ReaderT (Ptr JSContext) m b -> m b
mquickjsMultithreaded = mquickjsMultithreadedWithMemory (10 * 1024 * 1024)  -- 10MB default

mquickjsMultithreadedWithMemory :: MonadUnliftIO m => Int -> ReaderT (Ptr JSContext) m b -> m b
mquickjsMultithreadedWithMemory memSize f
  | rtsSupportsBoundThreads = do
    (u :: UnliftIO m) <- askUnliftIO

    liftIO $ runInBoundThread $ do
      memBuf <- mallocBytes memSize
      ctx <- [C.block| JSContext * {
        return JS_NewContext($(uint8_t *memBuf), $(size_t memSizeC), &js_stdlib);
      } |]

      res <- unliftIO u $ runReaderT f ctx
      cleanup memBuf ctx
      return res
  | otherwise = mquickjsWithMemory memSize f
  where
    memSizeC = fromIntegral memSize
    cleanup memBuf ctx = do
      jsFreeContext ctx
      free memBuf
