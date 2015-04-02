{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Cauterize.Generated.{{hscLibName}}.Types (
{{#hscTypes}}
{{#hstDetail}}
  {{hstConstructor}}(..),
{{/hstDetail}}
{{/hscTypes}}
  ) where

import Cauterize.Generated.{{hscLibName}}.BuiltIn

import Cauterize.GHC7.Support.Prototypes
import Cauterize.GHC7.Support.Result

{{#hscTypes}}
{{> array_tmpl.hs}}
{{> combination_tmpl.hs}}
{{> record_tmpl.hs}}
{{> synonym_tmpl.hs}}
{{> union_tmpl.hs}}
{{> vector_tmpl.hs}}
{{/hscTypes}}
