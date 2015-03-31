{{#HsTSynonym}}
{{#hstDetail}}
data {{hstConstructor}} = {{hstConstructor}} {{hstSynnedCtor}} deriving (Show, Eq)
instance CautType {{hstConstructor}} where; cautName _ = "{{hstName}}"
instance CautSynonym {{hstConstructor}} where
instance Serializable CautResult {{hstConstructor}} where
  serialize t@({{hstConstructor}} a) = genSynonymSerialize a t
  deserialize = genSynonymDeserialize (undefined :: {{hstConstructor}}) {{hstConstructor}}
{{/hstDetail}}
{{/HsTSynonym}}
