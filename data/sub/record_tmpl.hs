{{#HsTRecord}}
{{#hstDetail}}
data {{hstConstructor}} = {{hstConstructor}}
{{#hstRecordFields}}
{{#hstfsFirstField}}
  { {{hstNamePrefix}}{{hstfCtor}} :: {{hstdfRefCtor}}
{{/hstfsFirstField}}
{{#hstfsRemaining}}
  , {{hstNamePrefix}}{{hstfCtor}} :: {{hstdfRefCtor}}
{{/hstfsRemaining}}
  } deriving (Show, Ord, Eq)
{{/hstRecordFields}}
instance CautType {{hstConstructor}} where; cautName _ = "{{hstName}}"
instance CautRecord {{hstConstructor}} where
instance Serializable CautResult {{hstConstructor}} where
  serialize r = traceRecord $ do
{{#hstRecordFields}}
{{#hstfsAllFields}}
    genFieldSerialize (TRecordField "{{hstfName}}") ({{hstNamePrefix}}{{hstfCtor}} r)
{{/hstfsAllFields}}
{{/hstRecordFields}}
    where
      traceRecord = withTrace (TRecord $ cautName r)
  deserialize = traceRecord $
    return {{hstConstructor}}
{{#hstRecordFields}}
{{#hstfsAllFields}}
      `ap` genFieldDeserialize (TRecordField "{{hstfName}}")
{{/hstfsAllFields}}
{{/hstRecordFields}}
    where
      u = undefined :: {{hstConstructor}}
      traceRecord = withTrace (TRecord $ cautName u)
{{/hstDetail}}

{{/HsTRecord}}
