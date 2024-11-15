# MAYZ Protocol Version Management

## Table of Contents
- [MAYZ Protocol Version Management](#mayz-protocol-version-management)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
- [Current Implementation](#current-implementation)
  - [Hierarchical Version System](#hierarchical-version-system)
    - [Example Versions Generated](#example-versions-generated)
  - [Version Storage](#version-storage)
  - [Strict Dependency Model](#strict-dependency-model)
  - [Update Cascade Pattern](#update-cascade-pattern)
- [Types of Changes and Impacts](#types-of-changes-and-impacts)
- [Different Versioning Approaches](#different-versioning-approaches)
  - [Block Versioning](#block-versioning)
  - [Granular Versioning](#granular-versioning)
  - [Semantic Versioning](#semantic-versioning)
- [Compatibility Implementation](#compatibility-implementation)
  - [Defensive Deserialization](#defensive-deserialization)
  - [Version Handling](#version-handling)
- [Best Practices and Guidelines](#best-practices-and-guidelines)
  - [Design](#design)
  - [Implementation](#implementation)
  - [Testing](#testing)
- [Limitations and Recommendations](#limitations-and-recommendations)
  - [Restrictions](#restrictions)
  - [Future Enhancements](#future-enhancements)
- [Implementation Example](#implementation-example)
- [Update Procedures](#update-procedures)
  - [Steps for Updating](#steps-for-updating)
  - [Compatibility Checklist](#compatibility-checklist)
- [Conclusion](#conclusion)

## Introduction

This document provides a structured overview of the versioning approach for the MAYZ Protocol, examining the current strategy, its limitations, and alternative solutions for managing smart contract versions. Each versioning method will be explored to determine how to handle dependencies, data structures, and validation logic changes, ensuring smooth evolution and maintainability.

# Current Implementation

## Hierarchical Version System

The protocol implements a cascading dependency structure:

1. Protocol Contract (Base level)
   - Changes here trigger updates in all dependent contracts
   
2. Fund Block (Protocol-dependent)
   - Fund Contract
   - Fund Holding Contract  
   - Invest Unit Contract
   - Changes affect the entire block plus dependent Swap/Delegation contracts

3. Independent Contracts (Fund Block-dependent)
   - Swap Contract
   - Delegation Contract
   - Can be updated independently of each other

The version of each contract is defined in relation to other contracts it depends on.

In the current model, any changes—whether to logic, datum, or redeemer—require updating the version of the block the contract belongs to. This ensures that dependent contracts or blocks will also update to reflect the change, enforcing version consistency and compatibility across interdependent contracts.

### Example Versions Generated

```haskell

-- | Helper function to create a version number with a dependency
mkVersionWithDependency :: [Integer] -> Integer -> Integer
mkVersionWithDependency xs ownVersion
  = foldr (\x acc -> acc * 100 + x) ownVersion xs

-- Fund Contract Examples
protocolVersion = 1
fundVersion = 2
fundContractVersion1 = protocolVersion * 100 + fundVersion  -- Result: 102

protocolVersion = 3
fundVersion = 5
fundContractVersion2 = mkVersionWithDependency[protocolVersion] fundVersion  -- Result: 305

-- Swap Contract Examples
protocolVersion = 1
fundVersion = 2
swapVersion = 3
swapContractVersion1 = protocolVersion * 10000 + fundVersion * 100 + swapVersion  -- Result: 10203

protocolVersion = 2
fundVersion = 4
swapVersion = 5
swapContractVersion2 = protocolVersion * 10000 + fundVersion * 100 + swapVersion  -- Result: 20405

-- Delegation Contract Examples
protocolVersion = 1
fundVersion = 2
delegationVersion = 3
delegationContractVersion1 = protocolVersion * 10000 + fundVersion * 100 + delegationVersion  -- Result: 10203

protocolVersion = 2
fundVersion = 4
delegationVersion = 6
delegationContractVersion2 = mkVersionWithDependency[protocolVersion, fundVersion] delegationVersion  -- Result: 20406
```

This structured versioning ensures that each contract's version reflects dependencies, allowing the protocol to manage interdependencies between contracts effectively.

## Version Storage

Each contract datum includes a version identifier as its first field, ensuring consistent tracking across contracts:

```haskell
data FundDatumType = FundDatumType {
    fdVersion :: Integer,
    -- other fields...
}
```

## Strict Dependency Model

Currently, any change to a contract that another contract depends on (whether in logic, datum structure, or redeemer parameters) requires all dependent contracts to receive new versions. This rigid approach ensures that all contracts remain tightly aligned with the latest changes but imposes high overhead, as every dependency update requires a full cascade of updates across all related contracts.

## Update Cascade Pattern

Current dependency flow:
1. Protocol changes → All contracts require updates.
2. Fund block changes → Updates Fund/FundHolding/InvestUnit + Swap/Delegation.
3. Swap/Delegation → Can update independently.

# Types of Changes and Impacts

In analyzing changes, we categorize them as follows:

1. **Logic Changes**: Internal code modifications that do not alter the interfaces or structures.
2. **Datum Changes**: Adjustments to data structures, which have a high impact on dependent contracts.
3. **Redeemer Changes**: Updates to validation parameters, affecting contracts that verify these parameters.

Due to the strict dependency model, all of these change types necessitate version updates across all dependent contracts. The current block-based approach, where blocks of contracts are versioned and updated together, is implemented to maintain compatibility in these scenarios.

# Different Versioning Approaches

## Block Versioning

In this approach, related contracts are grouped and updated together as a cohesive block. This is the current approach implemented within the MAYZ Protocol to manage versions. Block versioning allows updates for major structural changes across highly interdependent contracts and ensures that when one contract in a block is updated, dependent contracts within that block are updated as well. This method helps simplify complex dependency management at the expense of potentially updating contracts unnecessarily for minor adjustments.

## Granular Versioning

Granular versioning would allow for versioning at the individual contract level, making it possible to deploy isolated updates while maintaining compatibility between versions. This method is particularly useful for minor adjustments or specific corrections that do not require other contracts to be updated.

## Semantic Versioning

A more nuanced approach, semantic versioning applies a structured versioning schema, where each contract has a version breakdown (major, minor, patch) with explicit compatibility dependencies. This approach combines dependency tracking with flexibility, enabling more targeted updates.

Using semantic versioning, dependencies are explicitly registered with compatible version ranges, allowing a contract to specify dependency compatibility, as shown below:

```haskell
-- | Defines types of changes that may affect other contracts
data ChangeType = 
    LogicChange    -- ^ Internal changes not affecting interfaces
    | DatumChange  -- ^ Changes in datum structure
    | RedeemChange -- ^ Changes in redeemers
    deriving (Eq, Show)

-- | Semantic version structure
data Version = Version {
    major :: Integer,  -- ^ Incompatible changes (datum/redeemer)
    minor :: Integer,  -- ^ New compatible features
    patch :: Integer   -- ^ Bug fixes
} deriving (Eq, Show)

-- | Define dependencies between contracts
data ContractDependency = ContractDependency {
    contractName :: T.Text,
    minVersion :: Version,
    maxVersion :: Version,
    changeTypes :: [ChangeType]
}

-- | Version configuration for each contract
data ContractVersion = ContractVersion {
    version :: Version,
    dependencies :: [ContractDependency]
}

-- | Example configuration for each contract
protocolVersion :: ContractVersion
protocolVersion = ContractVersion 
    (Version 1 0 0) 
    []  -- protocol has no dependencies

investUnitVersion :: ContractVersion
investUnitVersion = ContractVersion 
    (Version 1 0 0)
    [ ContractDependency 
        "Protocol" 
        (Version 1 0 0) 
        (Version 1 99 99) 
        [DatumChange]
    ]

fundVersion :: ContractVersion
fundVersion = ContractVersion 
    (Version 1 0 0)
    [ ContractDependency 
        "Protocol" 
        (Version 1 0 0) 
        (Version 1 99 99) 
        [DatumChange, RedeemChange],
      ContractDependency 
        "InvestUnit" 
        (Version 1 0 0) 
        (Version 1 99 99) 
        [DatumChange]
    ]

-- | Helper function to create a unique version considering dependencies
mkUniqueVersionWithDeps :: ContractVersion -> Integer
mkUniqueVersionWithDeps ContractVersion{..} = 
    let Version maj min pat = version
        depsVersions = map (\dep -> 
            let Version dmaj dmin _ = minVersion dep
            in dmaj * 100 + dmin) dependencies
    in mkUniqueVersion depsVersions (maj * 10000 + min * 100 + pat)
```

# Compatibility Implementation

To facilitate more flexible versioning, certain compatibility mechanisms can be implemented:

## Defensive Deserialization

Handling missing or additional data fields defensively during deserialization allows contracts to remain compatible with datum structures across versions, making them less susceptible to breaking on structural updates.

```haskell
parseData ::PlutusTx.FromData a =>  BuiltinData -> BuiltinString -> a
parseData d s = case PlutusTx.fromBuiltinData  d of
    Just d -> d
    _       -> traceError s
------------------
dataToListData :: BuiltinData -> BI.BuiltinList BuiltinData
dataToListData bd = BI.snd (BI.unsafeDataAsConstr bd)
------------------
getInvestUnitDatum_Safe :: BuiltinData -> (T.CS, [InvestUnitToken], Integer) 
getInvestUnitDatum_Safe datumRaw = 
    let datumList = dataToListData datumRaw
        -- Get basic fields that are always expected to exist
        fundPolicyCS = parseData (BI.head datumList) "error getting fundPolicyCS"
        investUnitRaw = BI.head (BI.tail datumList)
        minAda = parseData (BI.head (BI.tail (BI.tail datumList))) "error getting minAda"
        
        -- Manually deserialize InvestUnit
        investUnitList = dataToListData investUnitRaw
        investUnitValues = parseData (BI.head investUnitList) "error getting investUnit values"
    in (fundPolicyCS, investUnitValues, minAda)
```

## Version Handling

Structuring data types to include fields as `Maybe` types enables backward compatibility, allowing the addition of new fields without impacting existing versions.

```haskell
data DatumV1 = DatumV1 {
    field1 :: Integer,
    field2 :: Text
}

data DatumV2 = DatumV2 {
    field1 :: Integer,
    field2 :: Text,
    field3 :: Maybe Integer
}
```

# Best Practices and Guidelines

## Design

- Plan for extensibility and minimal dependencies.
- Use clear documentation to outline critical interfaces.

## Implementation

- Employ defensive deserialization to handle structure changes.
- Ensure all critical fields are validated to avoid incomplete or invalid data.

## Testing

- Validate compatibility across versions to ensure smooth updates.
- Simulate updates to test for data migration issues.

# Limitations and Recommendations

## Restrictions

To avoid version incompatibilities, the following should be adhered to:
- Avoid changing the types of existing fields.
- Do not remove fields in use.
- Minimize changes to critical validation logic without a coordinated update.

## Future Enhancements

To enhance flexibility and maintain compatibility, consider implementing:
- Automated verification systems for version compatibility.
- Bridge contracts between versions to manage structural changes.
- Automatic mapping of data structures across versions.

# Implementation Example

Below is an example illustrating extensible datum definitions and safe parsing practices to support forward compatibility:

```haskell
data InvestUnitDatum = InvestUnitDatum {
    fundPolicyCS :: CS,
    investUnit :: InvestUnit,
    minAda :: Integer,
    extraData :: Maybe BuiltinData
}

getInvestUnitData :: BuiltinData -> Maybe (CS, InvestUnit, Integer) 
getInvestUnitData raw = 
    let fields = dataToListData raw
    in if length fields < 3
       then Nothing
       else Just (
           parseData (head fields),
           parseData (head $ tail fields),
           parseData (head $ tail $ tail fields)
       )
```

# Update Procedures

## Steps for Updating

1. Identify the type of change required.
2. Evaluate its impact on dependent contracts.
3. Choose an appropriate versioning strategy.
4. Implement changes with compatibility in mind.
5. Test across all impacted contracts.
6. Coordinate the deployment of all updated contracts.

## Compatibility Checklist

- [ ] Verify changes do not break existing contracts.
- [ ] Handle missing cases in deserialization.
- [ ] Ensure tests cover migration scenarios.
- [ ] Update documentation to reflect new versions.
- [ ] Define and follow a structured deployment plan.

# Conclusion

Effective version management in smart contracts requires careful planning, structured testing, and clear documentation. By adopting a structured approach to version control, MAYZ Protocol can manage evolution flexibly while maintaining the stability and integrity of its contracts.

Future work will focus on balancing flexibility and backward compatibility, minimizing the impact of updates across dependent contracts.