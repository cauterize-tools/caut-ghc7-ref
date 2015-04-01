{-# LANGUAGe MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Cauterize.Generated.{{hscLibName}} (
{{#hscTypes}}
{{#hstDetail}}
  {{hstConstructor}}(..),
{{/hstDetail}}
{{/hscTypes}}
  ) where

import Cauterize.GHC7.Support.BuiltIn
import Cauterize.GHC7.Support.Prototypes
import Cauterize.GHC7.Support.Result

{{#hscTypes}}
{{> array_tmpl.hs}}
{{> builtin_tmpl.hs}}
{{> combination_tmpl.hs}}
{{> record_tmpl.hs}}
{{> synonym_tmpl.hs}}
{{> union_tmpl.hs}}
{{> vector_tmpl.hs}}

{{/hscTypes}}
