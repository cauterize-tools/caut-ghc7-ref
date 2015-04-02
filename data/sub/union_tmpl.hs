{{#HsTUnion}}
{{#hstDetail}}
data {{hstConstructor}}
{{#hstUnionFields}}
{{#hstfsFirstField}}
{{#HsTDataField}}
  = {{hstConstructor}}{{hstfCtor}} {{hstdfRefCtor}}
{{/HsTDataField}}
{{#HsTEmptyField}}
  = {{hstConstructor}}{{hstfCtor}}
{{/HsTEmptyField}}
{{/hstfsFirstField}}
{{#hstfsRemaining}}
{{#HsTDataField}}
  | {{hstConstructor}}{{hstfCtor}} {{hstdfRefCtor}}
{{/HsTDataField}}
{{#HsTEmptyField}}
  | {{hstConstructor}}{{hstfCtor}}
{{/HsTEmptyField}}
{{/hstfsRemaining}}
{{/hstUnionFields}}
  deriving (Show, Eq, Ord)
instance CautType {{hstConstructor}} where
  cautName _ = "{{hstName}}"
  cautHash _ = {{hstHashListStr}}
  cautSize _ = ({{hstSize.hstMinSize}},{{hstSize.hstMaxSize}})
instance CautUnion {{hstConstructor}} where; unionTagWidth _ = {{hstUnionTagWidth}}
instance Serializable CautResult {{hstConstructor}} where
  serialize r = traceUnion $
    case r of
{{#hstUnionFields}}
{{#hstfsAllFields}}
{{#HsTDataField}}
      {{hstConstructor}}{{hstfCtor}} v -> genUnionFieldSerialize r {{hstfIndex}} "{{hstfName}}" v
{{/HsTDataField}}
{{#HsTEmptyField}}
      {{hstConstructor}}{{hstfCtor}} -> genUnionFieldSerializeEmpty r {{hstfIndex}}
{{/HsTEmptyField}}
{{/hstfsAllFields}}
{{/hstUnionFields}}
    where
      traceUnion = withTrace (TUnion $ cautName r)
  deserialize = traceUnion $ do
    tag <- decodeUnionTag u
    case tag of
{{#hstUnionFields}}
{{#hstfsAllFields}}
{{#HsTDataField}}
      {{hstfIndex}} -> genUnionFieldDeserialize "{{hstfName}}" {{hstConstructor}}{{hstfCtor}}
{{/HsTDataField}}
{{#HsTEmptyField}}
      {{hstfIndex}} -> return {{hstConstructor}}{{hstfCtor}}
{{/HsTEmptyField}}
{{/hstfsAllFields}}
{{/hstUnionFields}}
      v -> failUnionTag v
    where
      u = undefined :: {{hstConstructor}}
      traceUnion = withTrace (TUnion $ cautName u)
{{/hstDetail}}

{{/HsTUnion}}
