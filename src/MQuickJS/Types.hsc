{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module MQuickJS.Types where

import qualified Data.Map                  as Map
import           Data.Bits                 (Bits)
import           Foreign.C.Types
import           Foreign.Ptr               (plusPtr)
import           Foreign.Storable          (Storable(..))
import           Language.C.Inline
import           Language.C.Inline.Context (Context(..), TypesTable)
import qualified Language.C.Types          as C

#include <stddef.h>
#include "mquickjs.h"

-- Micro QuickJS uses a simple uint64_t or uint32_t for JSValue (no struct)
#if INTPTR_MAX >= INT64_MAX
type JSValue = CULong  -- 64-bit
#else
type JSValue = CUInt   -- 32-bit
#endif

type JSValueConst = JSValue

newtype JSContext = JSContext { _unusedContext :: CUChar }


type JSBool = CInt

-- Note: JSAtom, JSPropertyEnum, and JSRefCountHeader are not exposed in Micro QuickJS public API



class ToCType ty cty where
  toCType :: ty -> cty


class FromCType ty cty where
  fromCType :: cty -> Maybe ty


-- Micro QuickJS has a simpler tag system
-- Note: JS_TAG_SPECIAL is not included as a separate constructor because
-- JS_TAG_BOOL has the same value (3). The specific special tags (Bool, Null, etc.)
-- are the user-facing values.
data JSTagEnum = JSTagInt
               | JSTagPtr
               | JSTagBool
               | JSTagNull
               | JSTagUndefined
               | JSTagUninitialized
               | JSTagCatchOffset
               | JSTagException
  deriving (Show, Eq)

instance Num a => ToCType JSTagEnum a where
  toCType JSTagInt              = #{const JS_TAG_INT}
  toCType JSTagPtr              = #{const JS_TAG_PTR}
  toCType JSTagBool             = #{const JS_TAG_BOOL}
  toCType JSTagNull             = #{const JS_TAG_NULL}
  toCType JSTagUndefined        = #{const JS_TAG_UNDEFINED}
  toCType JSTagUninitialized    = #{const JS_TAG_UNINITIALIZED}
  toCType JSTagCatchOffset      = #{const JS_TAG_CATCH_OFFSET}
  toCType JSTagException        = #{const JS_TAG_EXCEPTION}


instance (Eq a, Num a) => FromCType JSTagEnum a where
  fromCType (#{const JS_TAG_INT}) = Just JSTagInt
  fromCType (#{const JS_TAG_PTR}) = Just JSTagPtr
  fromCType (#{const JS_TAG_BOOL}) = Just JSTagBool
  fromCType (#{const JS_TAG_NULL}) = Just JSTagNull
  fromCType (#{const JS_TAG_UNDEFINED}) = Just JSTagUndefined
  fromCType (#{const JS_TAG_UNINITIALIZED}) = Just JSTagUninitialized
  fromCType (#{const JS_TAG_CATCH_OFFSET}) = Just JSTagCatchOffset
  fromCType (#{const JS_TAG_EXCEPTION}) = Just JSTagException
  fromCType _ = Nothing

data JSTypeEnum = JSTypeFromTag JSTagEnum
                | JSIsNumber
                | JSIsArray
                | JSIsDate
                | JSIsError
  deriving Show

-- Micro QuickJS eval flags (different from original QuickJS)
data JSEvalType = Global | WithRetval

instance Num a => ToCType JSEvalType a where
  toCType Global = 0  -- No special flags for global eval
  toCType WithRetval = #{const JS_EVAL_RETVAL}  -- Return last value instead of undefined


mquickjsCtx :: Context
mquickjsCtx = baseCtx <> fptrCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = mquickjsTypesTable
      }

mquickjsTypesTable :: TypesTable
mquickjsTypesTable = Map.fromList
  [
    (C.TypeName "JSValue", [t| JSValue |])
  , (C.TypeName "JSValueConst", [t| JSValueConst |])
  , (C.TypeName "JSContext", [t| JSContext |])
  , (C.TypeName "JSBool", [t| JSBool |])
  ]

