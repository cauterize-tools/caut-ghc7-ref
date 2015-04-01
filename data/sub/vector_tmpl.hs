{{#HsTVector}}
{{#hstDetail}}
data {{hstConstructor}} = {{hstConstructor}} (Vector CBool) deriving (Show, Eq, Ord)
instance CautType {{hstConstructor}} where; cautName _ = "{{hstName}}"
instance CautVector {{hstConstructor}} where; vectorMaxLength _ = {{hstVectorMaxLen}}; vectorTagWidth _ = {{hstVectorLenWidth}}
instance Serializable CautResult {{hstConstructor}} where
  serialize t@({{hstConstructor}} vs) = genVectorSerialize vs t
  deserialize = genVectorDeserialize (undefined :: {{hstConstructor}}) {{hstConstructor}}
{{/hstDetail}}
{{/HsTVector}}
