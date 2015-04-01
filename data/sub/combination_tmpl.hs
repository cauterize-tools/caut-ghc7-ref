{{#HsTCombination}}
{{#hstDetail}}
data {{hstConstructor}} = {{hstConstructor}}
{{#hstCombinationFields}}
{{#hstfsFirstField}}
{{#HsTDataField}}
  { {{hstNamePrefix}}{{hstfCtor}} :: Maybe {{hstdfRefCtor}}
{{/HsTDataField}}
{{#HsTEmptyField}}
  { {{hstNamePrefix}}{{hstfCtor}} :: Maybe ()
{{/HsTEmptyField}}
{{/hstfsFirstField}}
{{#hstfsRemaining}}
{{#HsTDataField}}
  , {{hstNamePrefix}}{{hstfCtor}} :: Maybe {{hstdfRefCtor}}
{{/HsTDataField}}
{{#HsTEmptyField}}
  , {{hstNamePrefix}}{{hstfCtor}} :: Maybe ()
{{/HsTEmptyField}}
{{/hstfsRemaining}}
{{/hstCombinationFields}}
  } deriving (Show, Ord, Eq)
instance CautType {{hstConstructor}} where; cautName _ = "{{hstName}}"
instance CautCombination {{hstConstructor}} where; combinationTagWidth _ = {{hstCombinationFlagsWidth}}; combinationMaxIndex _ = {{hstCombinationMaxIndex}}
instance Serializable CautResult {{hstConstructor}} where
  serialize r = traceComb $ do
{{#hstCombinationFields}}
{{#hstfsFirstField}}
    encodeCombTag r [ fieldPresent $ {{hstNamePrefix}}{{hstfCtor}} r
{{/hstfsFirstField}}
{{#hstfsRemaining}}
                    , fieldPresent $ {{hstNamePrefix}}{{hstfCtor}} r
{{/hstfsRemaining}}
{{/hstCombinationFields}}
                    ]
{{#hstCombinationFields}}
{{#hstfsDataFields}}
    genCombFieldSerialize "{{hstfName}}" $ {{hstNamePrefix}}{{hstfCtor}} r
{{/hstfsDataFields}}
{{/hstCombinationFields}}
    where
      traceComb = withTrace (TCombination $ cautName r)
  deserialize = traceComb $ do
    flags <- decodeCombTag u
    return {{hstConstructor}}
{{#hstCombinationFields}}
{{#hstfsAllFields}}
{{#HsTDataField}}
      `ap` genCombFieldDeserialize "{{hstfName}}" (flags `isFlagSet` {{hstfIndex}})
{{/HsTDataField}}
{{#HsTEmptyField}}
      `ap` return (Just ())
{{/HsTEmptyField}}
{{/hstfsAllFields}}
{{/hstCombinationFields}}
    where
      u = undefined :: {{hstConstructor}}
      traceComb = withTrace (TCombination $ cautName u)
{{/hstDetail}}

{{/HsTCombination}}
