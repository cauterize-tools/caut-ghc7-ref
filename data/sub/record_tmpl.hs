{{#HsTRecord}}
{{#hstDetail}}
data {{hstConstructor}} = {{hstConstructor}}
{{#HsTFieldSet}}
{{#hstfsFirstField}}
  { {{hstNamePrefix}}{{hstdfCtor}} :: {{hstdfRefCtor}}
{{/hstfsFirstField}}
{{#hstfsRemaining}}
  , {{hstNamePrefix}}{{hstdfCtor}} :: {{hstdfRefCtor}}
{{/hstfsRemaining}}
  } deriving (Show, Ord, Eq)
{{/HsTFieldSet}}
{-
instance CautType {{hstConstructor}} where; cautName _ = "{{hstName}}"
instance CautRecord {{hstConstructor}} where
instance Serializable CautResult {{hstConstructor}} where
  serialize r = traceRecord $ do
    genFieldSerialize (TRecordField "field_bool") (aRecordFieldBool r)
    genFieldSerialize (TRecordField "field_u8") (aRecordFieldU8 r)
    where
      traceRecord = withTrace (TRecord $ cautName r)
  deserialize = traceRecord $ do
    a0 <- genFieldDeserialize (TRecordField "field_bool")
    a1 <- genFieldDeserialize (TRecordField "field_u8")
    return $ ARecord a0 a1
    where
      u = undefined :: ARecord
      traceRecord = withTrace (TRecord $ cautName u)
-}
{{/hstDetail}}
{{/HsTRecord}}
