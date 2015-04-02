{{#HsTArray}}
{{#hstDetail}}
data {{hstConstructor}} = {{hstConstructor}} (Vector {{hstArrayRefCtor}}) deriving (Show, Eq, Ord)
instance CautType {{hstConstructor}} where
  cautName _ = "{{hstName}}"
  cautHash _ = {{hstHashListStr}}
  cautSize _ = ({{hstSize.hstMinSize}},{{hstSize.hstMaxSize}})
instance CautArray {{hstConstructor}} where; arrayLength _ = {{hstArrayLen}}
instance Serializable CautResult {{hstConstructor}} where
  serialize t@({{hstConstructor}} vs) = genArraySerialize vs t
  deserialize = genArrayDeserialize (undefined :: {{hstConstructor}}) {{hstConstructor}}
{{/hstDetail}}

{{/HsTArray}}
