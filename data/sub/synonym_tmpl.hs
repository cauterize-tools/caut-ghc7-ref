{{#HsTSynonym}}
{{#hstDetail}}
data {{hstConstructor}} = {{hstConstructor}} {{hstSynnedCtor}} deriving (Show, Eq, Ord)
instance CautType {{hstConstructor}} where
  cautName _ = "{{hstName}}"
  cautHash _ = {{hstHashListStr}}
  cautSize _ = ({{hstSize.hstMinSize}},{{hstSize.hstMaxSize}})
instance CautSynonym {{hstConstructor}} where
instance Serializable CautResult {{hstConstructor}} where
  serialize t@({{hstConstructor}} a) = genSynonymSerialize a t
  deserialize = genSynonymDeserialize (undefined :: {{hstConstructor}}) {{hstConstructor}}
{{/hstDetail}}

{{/HsTSynonym}}
