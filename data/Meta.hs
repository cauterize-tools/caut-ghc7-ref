module Cauterize.Generated.{{hscLibName}}.Meta (
  Meta{{hscLibName}}(..),
  ) where

import Cauterize.Generated.{{hscLibName}}.Types

data Meta{{hscLibName}}
{{#hscFirstType}}
{{#hstDetail}}
  = Meta{{hstConstructor}} {{hstConstructor}}
{{/hstDetail}}
{{/hscFirstType}}
{{#hscRestTypes}}
{{#hstDetail}}
  | Meta{{hstConstructor}} {{hstConstructor}}
{{/hstDetail}}
{{/hscRestTypes}}
  deriving (Show, Eq, Ord)
