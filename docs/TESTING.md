# MAYZ Protocol: Testing Documentation

## Table of Contents

- [MAYZ Protocol: Testing Documentation](#mayz-protocol-testing-documentation)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Unit Tests](#unit-tests)
    - [Description](#description)
    - [Running Unit Tests](#running-unit-tests)
      - [Test Parameters](#test-parameters)
    - [Test Structure](#test-structure)
    - [Transactions and Redeemers Tested](#transactions-and-redeemers-tested)
    - [Key Test Cases](#key-test-cases)
    - [Relevant Information](#relevant-information)
  - [Property-Based Tests](#property-based-tests)
    - [Overview](#overview)
    - [Running Property-Based Tests](#running-property-based-tests)
      - [Test Parameters](#test-parameters-1)
    - [Test Structure](#test-structure-1)
      - [Contracts Tests](#contracts-tests)
      - [Helpers Tests](#helpers-tests)
    - [Key Concepts](#key-concepts)
      - [Generators](#generators)
      - [Properties](#properties)
      - [Shrinking](#shrinking)
    - [Example Property Test](#example-property-test)
    - [Generators](#generators-1)
    - [Key Areas Tested](#key-areas-tested)
    - [Benefits of Property-Based Testing](#benefits-of-property-based-testing)
    - [Limitations and Considerations](#limitations-and-considerations)
    - [Conclusion](#conclusion)
  - [Performance Tests](#performance-tests)
    - [Overview](#overview-1)
    - [Running Performance Tests](#running-performance-tests)
      - [Test Parameters](#test-parameters-2)
    - [Test Structure](#test-structure-2)
    - [Key Areas Tested](#key-areas-tested-1)
      - [PowRational Function](#powrational-function)
      - [CalculateDepositCommissionsUsingMonths Function](#calculatedepositcommissionsusingmonths-function)
      - [Deposit Operations](#deposit-operations)
    - [Test Parameters](#test-parameters-3)
    - [Limitations and Future Work](#limitations-and-future-work)
    - [Interpreting Results](#interpreting-results)
    - [Conclusion](#conclusion-1)
  - [Automatic Tests](#automatic-tests)
    - [Overview](#overview-2)
    - [Key Concepts](#key-concepts-1)
    - [Running Automatic Tests](#running-automatic-tests)
      - [Test Parameters](#test-parameters-4)
      - [Configuration File](#configuration-file)
    - [Log Parser Tool](#log-parser-tool)
    - [Test Structure and PATH Relation](#test-structure-and-path-relation)
    - [Current Test Coverage](#current-test-coverage)
    - [Scope and Limitations](#scope-and-limitations)
    - [Benefits of This Approach](#benefits-of-this-approach)
    - [Main Components](#main-components)
    - [Key Functions](#key-functions)
    - [Rule Definition](#rule-definition)
      - [Configuration File](#configuration-file-1)
    - [Test Case Generation Process](#test-case-generation-process)
    - [Specific Test Categories](#specific-test-categories)
    - [Test Parameters and Generators](#test-parameters-and-generators)
    - [Limitations and Considerations](#limitations-and-considerations-1)
    - [Future Enhancements](#future-enhancements)
  
## Introduction

The MAYZ Protocol employs a comprehensive testing strategy to ensure the correctness, security, and performance of its smart contracts and associated components. Our testing approach includes four main test suites:
1. Unit Tests
2. Property-Based Tests
3. Performance Tests
4. Automatic Tests
This document focuses on the Unit Tests, with future sections to be added for the other test suites.

## Unit Tests

### Description

Unit tests are designed to verify the correctness of individual components and functions within the MAYZ Protocol. These tests focus on isolated pieces of code, ensuring that each part of the system behaves as expected under various conditions.

### Running Unit Tests

To run the unit tests, use the following command from the project root:
```
cabal test UnitTests
```

For more detailed output, you can use:
```
cabal test UnitTests --test-show-details=always
```

#### Test Parameters
- `-j1`: This parameter limits the number of jobs to 1, ensuring tests run sequentially. Useful for debugging or when tests might interfere with each other.
- `--test-options='-p /pattern/'`: This option allows you to run only tests matching a specific pattern. Replace 'pattern' with the desired test name or pattern.

Example:
```
cabal test UnitTests -j1 --test-options='-p /Protocol/'
```

This runs unit tests sequentially, only for tests containing "Protocol" in their name.

### Test Structure

Our unit tests are organized into groups based on the contract or component being tested. The main structure includes:
- Contracts Tests
  - Protocol Tests
  - Fund Tests
  - FundHolding Tests
  - InvestUnit Tests
  - SwapOffer Tests
- Helpers Tests
  - Value Tests
  - Commission Tests
  - Scale and Rounding Tests
  
### Transactions and Redeemers Tested

The unit tests cover a wide range of transactions and redeemers, including but not limited to:
1. Protocol Transactions:
   - Protocol Create (MintID)
   - Protocol Datum Update
   - Protocol Update MinADA
2. Fund Transactions:
   - Fund Create (MintID)
   - Fund Datum Update
   - Fund Update MinADA
   - FundHolding Create
   - FundHolding Delete
   - Fund Finish
   - Fund Delete
3. FundHolding Transactions:
   - FundHolding Create (MintID)
   - FundHolding Update MinADA
   - Fund Deposit
   - Fund Withdraw
   - FundHolding Collect Commissions (Protocol, Managers, Delegators)
   - Fund ReIndexing
   - FundHolding Balance Assets
   - FundHolding Delete
4. InvestUnit Transactions:
   - Fund ReIndexing
   - InvestUnit Update MinADA
5. SwapOffer Transactions:
   - SwapOffer Create (MintID)
   - SwapOffer Update Status
   - SwapOffer Update Asked Commission Rate
   - SwapOffer Update Sell Restrictions
   - SwapOffer Update MinADA
   - SwapOffer Deposit
   - SwapOffer Withdraw
   - SwapOffer Swap FT for ADA
   - SwapOffer Swap ADA for FT
   - SwapOffer Delete

### Key Test Cases

Some of the key aspects tested in our unit tests include:
- Correct minting and burning of tokens
- Proper datum updates and validations
- Commission calculations and distributions
- Fund creation, management, and deletion
- SwapOffer functionality and restrictions
- Minimum ADA requirements
- Re-indexing process for funds
- Admin actions and permissions
- Error cases and invalid operations
  
### Relevant Information

- The unit tests use mock data and contexts to simulate various scenarios.
- Tests are designed to check both successful operations and failure cases.
- Each test group focuses on a specific contract or component of the protocol.
- The testing framework allows for easy addition of new test cases as the protocol evolves.

## Property-Based Tests

### Overview
Property-based tests complement unit tests by generating random inputs to verify that certain properties or invariants hold true across a wide range of scenarios. This approach helps uncover edge cases and unexpected behaviors that might be missed by traditional unit testing.

### Running Property-Based Tests

To run the property-based tests, use the following command:
```
cabal test PropertyBasedTests
```

For more detailed output:
```
cabal test PropertyBasedTests --test-show-details=always
```

#### Test Parameters
- `-j1`: This parameter limits the number of jobs to 1, ensuring tests run sequentially. Useful for debugging or when tests might interfere with each other.
- `--test-options='-p /pattern/'`: This option allows you to run only tests matching a specific pattern. Replace 'pattern' with the desired test name or pattern.
- `--test-options='--quickcheck-tests=n'`: Sets the number of test cases QuickCheck should generate for each property. Replace 'n' with the desired number (e.g., 100, 1000).

Example:
```
cabal test PropertyBasedTests -j1 --test-options='-p /Protocol/ --quickcheck-tests=1000'
```

This runs property-based tests sequentially, only for tests containing "Protocol" in their name, with 1000 test cases per property.
s

### Test Structure
Property-based tests are organized similarly to unit tests, grouped by contract or component:

#### Contracts Tests
- Protocol Policy Tests
- Protocol Validator Tests
- Fund Policy Tests
- Fund Validator Tests
- FundHolding Policy Tests
- FundHolding Validator Tests
- InvestUnit Validator Tests
- SwapOffer Policy Tests
- SwapOffer Validator Tests

#### Helpers Tests
- Value Tests
- Commission Tests
- Scale and Rounding Tests

### Key Concepts

#### Generators
Custom QuickCheck generators are defined to create valid, random instances of complex types like `ProtocolDatumType`, `FundDatumType`, etc.

#### Properties
Instead of specific test cases, properties are defined that should hold true for all (or a large subset of) possible inputs.

#### Shrinking
When a test fails, QuickCheck attempts to find the simplest counterexample by "shrinking" the input.

### Example Property Test
Here's an example of a property test for the Protocol Validator:

```
prop_protocolValidator_changeValue :: ProtocolDatumType -> RandomSingleton -> Bool
prop_protocolValidator_changeValue protocolDatum randomSingleton =
let ctx = createMockContext protocolDatum randomSingleton
result = evaluateScriptValidator protocolValidator protocolDatum updateProtocolRedeemer ctx
in not (null result)
```

This property checks that changing the Protocol UTxO value always results in a validation error.

### Generators
Custom generators ensure that our random inputs are valid within the context of the MAYZ protocol. For example:

```
instance Arbitrary ProtocolDatumType where
arbitrary = ProtocolDatumType
<$> arbitrary
<> arbitrary
<> arbitrary
-- ... other fields
<*> (listOf1 arbitrary suchThat (\x -> length x > 0 && length x <= 10))
```

This generator creates valid `ProtocolDatumType` instances with constraints on certain fields.

### Key Areas Tested
- **Token Minting and Burning**: Ensuring correct behavior for various NFTs and FTs.
- **Datum Updates**: Verifying that datum updates maintain invariants and fail when they should.
- **Value Changes**: Checking that unauthorized value changes are rejected.
- **Commission Calculations**: Validating commission logic across different scenarios.
- **Re-indexing**: Testing the complex re-indexing process for funds.
- **Boundary Conditions**: Exploring edge cases in numerical calculations and token amounts.

### Benefits of Property-Based Testing
- **Increased Test Coverage**: Generates a wide range of test cases, potentially uncovering edge cases.
- **Reduced Bias**: Helps eliminate bias that might occur in manually written test cases.
- **Simplified Test Maintenance**: Properties often remain stable even as implementation details change.
- **Documentation**: Properties serve as executable specifications of how the system should behave.

### Limitations and Considerations
- **Slower Execution**: Property-based tests generally take longer to run than unit tests.
- **Complex Setup**: Defining appropriate generators and properties can be challenging for complex systems.
- **False Positives**: Occasionally, valid but extremely rare cases might be flagged as errors.

### Conclusion
Property-based testing complements our unit tests by providing a more exhaustive exploration of the MAYZ protocol's behavior. By defining and testing invariants, we increase confidence in the correctness and robustness of our smart contracts across a wide range of scenarios.


## Performance Tests

### Overview
Performance tests are designed to evaluate the resource usage and efficiency of critical components within the MAYZ Protocol. These tests ensure that the protocol operates within acceptable resource limits, particularly important for on-chain operations where computational costs directly translate to transaction fees.

### Running Performance Tests

To run the performance tests, use the following command:
```
cabal test PerformanceTests
```

For more detailed output:
```
cabal test PerformanceTests --test-show-details=always
```

#### Test Parameters
- `-j1`: This parameter limits the number of jobs to 1, ensuring tests run sequentially. Useful for debugging or when tests might interfere with each other.
- `--test-options='-p /pattern/'`: This option allows you to run only tests matching a specific pattern. Replace 'pattern' with the desired test name or pattern.
- `--test-options='--quickcheck-tests=n'`: Sets the number of test cases QuickCheck should generate for each property. Replace 'n' with the desired number (e.g., 100, 1000).

Example:
```
cabal test PerformanceTests -j1 --test-options='-p /Deposits/ --quickcheck-tests=1000'
```

This runs performance tests sequentially, only for tests containing "Deposits" in their name, with 1000 test cases per property.

### Test Structure
The performance tests are organized into two main categories:

1. Unit Tests
   - Signature validation tests

2. Property-Based Tests
   - Resource usage tests for various operations

### Key Areas Tested

#### PowRational Function
- Tests the `PowRational` function's memory usage
- Ensures it uses less than 3.5Mb in standard version
- Checks optimized version uses less than 1.4Mb

#### CalculateDepositCommissionsUsingMonths Function
- Evaluates memory usage of commission calculations
- Standard version should use less than 6.5Mb
- Optimized version should use less than 2.5Mb

#### Deposit Operations
- Tests overall deposit functionality
- Checks memory usage is below 10Mb for standard version
- Optimized version with 10 max tokens should use less than 6.2Mb
- Verifies deposits are valid in both standard and optimized versions

### Test Parameters
- Tests generate random inputs within specified ranges
- Commission rates, dates, and deposit amounts are randomized
- For deposit tests, random token sets are generated

### Limitations and Future Work
- Current tests focus primarily on memory usage
- CPU usage and execution time metrics could be added
- More comprehensive tests for complex operations like re-indexing are needed
- Edge cases and stress testing scenarios should be expanded

### Interpreting Results
- Tests pass if resource usage is below specified thresholds
- Failed tests indicate potential performance regressions
- Results help identify areas for optimization

### Conclusion
While these performance tests provide a foundation for assessing the MAYZ Protocol's resource efficiency, there's room for expansion and refinement. Future iterations should include more diverse scenarios, stricter thresholds, and additional metrics to ensure the protocol's scalability and cost-effectiveness on the Cardano blockchain.

## Automatic Tests

### Overview
Automatic tests form the foundation of the MAYZ Protocol's testing strategy. These tests systematically explore a wide range of scenarios by manipulating transaction structures and validating them against predefined rules, ensuring comprehensive coverage of basic contract behaviors.

### Key Concepts

1. **TxSpecs (Transaction Specifications)**: A standardized format for defining transactions, including inputs, outputs, minting operations, redeemers, and other relevant parameters.

2. **Test Case Generation**: The system automatically generates test cases by altering elements of the TxSpecs with both valid and invalid variants.

3. **Rule-Based Validation**: Test outcomes are compared against a set of rules defined in an Excel spreadsheet, which specifies expected results for various scenarios.

4. **Wildcard Matching**: The rule system supports wildcards and pattern matching to flexibly define expected outcomes for groups of similar test cases.

5. **PATH Structure**: A hierarchical representation of test case components, directly mapping to TestCaseParams attributes.

6. **TestCaseParams**: A comprehensive set of parameters defining specific aspects of each test case, including input options, output options, and minting options.

7. **Dynamic Test Case Creation**: Tests are generated based on TxSpecs and rules, allowing for automatic expansion of test coverage as the protocol evolves.

8. **Baseline Behavior Validation**: These tests cover the basic, expected behavior of each script with each redeemer, forming a foundational layer of testing.

### Running Automatic Tests

To run the automatic tests, use the following command:
```
cabal test AutomaticTests
```

For more detailed output:
```
cabal test AutomaticTests --test-show-details=always
```

#### Test Parameters
- `-j1`: This parameter limits the number of jobs to 1, ensuring tests run sequentially. Useful for debugging or when tests might interfere with each other.
- `--test-options='-p /pattern/'`: This option allows you to run only tests matching a specific pattern. Replace 'pattern' with the desired test name or pattern.
- `--test-options='--quickcheck-tests=n'`: Sets the number of test cases QuickCheck should generate for each property. Replace 'n' with the desired number (e.g., 100, 1000).

Example:
```
cabal test AutomaticTests -j1 --test-options='-p // --quickcheck-tests=1'
```

This runs automatic tests sequentially, with 1 test case per property.

```
cabal test AutomaticTests -j1 --test-options='-p /Deposits/ --quickcheck-tests=1000'
```

This runs automatic tests sequentially, only for tests containing "Deposits" in their name, with 1000 test cases per property.

#### Configuration File
The automatic tests rely on a configuration file that defines the rules and expected errors for each test case. This file is crucial for the proper execution and validation of the tests.

File Location: `tests/config/tests.xlsx`

**Note**: The path to this configuration file is currently not configurable and is hardcoded in the test suite. Ensure this file is present and up-to-date before running the automatic tests.

The Excel file contains the following key information:
- Test case specifications
- Expected outcomes for each test scenario
- Error messages to be matched against test results
- Rules for validating test outcomes

Check more about the [Rule Definitions](#rule-definition).

It's essential to keep this configuration file synchronized with any changes made to the smart contracts or test scenarios. Regular updates to this file ensure that the automatic tests accurately reflect the current state and requirements of the MAYZ Protocol.

### Log Parser Tool

To assist in creating and maintaining the configuration file, we provide a log parser tool. This tool can parse the output of automatic tests and generate an initial Excel file with test rules.

Location: `tests/tools/`

The log parser tool automates the process of:
1. Analyzing test output logs
2. Extracting information about test failures
3. Generating an Excel file that can be used as a starting point for the `tests/config/tests.xlsx` configuration

For detailed instructions on using the log parser tool, refer to its README file:
[Log Parser Tool README](../tests/tools/README.md)

This tool is particularly useful when:
- Setting up initial test configurations
- Updating test rules after significant changes to the protocol
- Analyzing and incorporating new failure cases into the test suite

While the generated Excel file provides a good starting point, it's crucial to review and refine the rules manually to ensure they accurately reflect the expected behavior of the MAYZ Protocol.

### Test Structure and PATH Relation

The PATH in the rule set directly corresponds to the structure of TestCaseParams:

1. Transaction Type
2. Script Type
3. Redeemer
4. Component (e.g., Inputs, Outputs, Mints)
5. Specific Test Case (e.g., InputValid, InputInvalid)

For example, a PATH like "Fund_Create_Tx.Fund_Policy.Fund_MintID.Outputs.Fund.OutputInvalid.TxOutInvalidNone" maps to:
- Transaction: Fund Creation
- Script: Fund Policy
- Redeemer: MintID
- Component: Outputs
- Test Case: Invalid Output (None)

This structure allows for precise targeting of test scenarios and easy mapping between rules and generated test cases.

### Current Test Coverage

The automatic test suite currently runs 2,200 test cases, all of which are passing. These tests cover the basic, expected behavior of each script with each redeemer. While not exhaustive, they provide a crucial baseline for ensuring the correctness of fundamental protocol operations.

### Scope and Limitations

1. **Baseline Coverage**: These tests focus on standard, regulated scenarios, ensuring that basic contract behaviors are consistently validated.

2. **Automated Consistency**: By automating these tests, we reduce the risk of overlooking fundamental test cases during manual testing.

3. **Complementary Testing**: Automatic tests are designed to work alongside unit and property tests. They handle common scenarios, allowing manual tests to focus on more complex, edge-case behaviors.

4. **Logic-Specific Testing**: While automatic tests cover structural aspects of transactions, they don't deeply test the logic within datums and redeemers. These aspects are better addressed in unit and property tests.

### Benefits of This Approach

1. **Consistency**: Ensures all basic scenarios are tested across all contract types and redeemers.
2. **Efficiency**: Reduces manual effort in writing repetitive test cases.
3. **Scalability**: Easily expandable as new contract features or transaction types are added.
4. **Foundation for Advanced Testing**: Provides a solid base, allowing manual tests to focus on more complex scenarios.

### Main Components

1. **TestParams**: Contains essential parameters for testing, including currency symbols, token names, and other protocol-specific data.

2. **RuleTree**: A data structure that represents the hierarchical organization of test rules, allowing for efficient rule matching and application.

3. **TxSpecs**: Defines the structure of a transaction, including inputs, outputs, mints, and other relevant data.

4. **TestCaseParams**: Represents the parameters for a specific test case, including input options, output options, and minting options.

### Key Functions

1. **transaction_Tests_Gen**: Generates a comprehensive set of tests for a given transaction type, covering various aspects such as inputs, outputs, minting, validity range, and signatures.

2. **generateTestCase**: Creates individual test cases based on the provided parameters and configuration.

3. **updateConfigTreeFromRuleTree**: Updates the test configuration tree based on the rules defined in the Excel spreadsheet.

4. **adminTokens_Tests_Gen**: Generates tests specifically for admin token functionality, checking various scenarios with admin and emergency tokens.

### Rule Definition

Rules are defined in an Excel spreadsheet with the following key columns:
- Use: Indicates if the rule should be applied
- TX NAME: The transaction type
- SCRIPT: The contract being tested
- REDEEMER: The specific redeemer being used
- PATH: A dot-separated path defining the test case structure
- OUTCOME: Expected result (TestSuccess, TestFailure, TestNone)
- MESSAGE: Expected error message for failures
- SOURCE: The source of the error (e.g., SelfRedeemer, CombinedRedeemerResults)

#### Configuration File
File Location: `tests/config/tests.xlsx`

### Test Case Generation Process

1. The system reads the TxSpecs for a given transaction type.
2. It systematically alters components of the transaction (inputs, outputs, mints, etc.).
3. For each alteration, it generates a test case using the `generateTestCase` function.
4. The test case is executed, and the result is captured.
5. The system matches the test case against the rules defined in the Excel spreadsheet using the `updateConfigTreeFromRuleTree` function.
6. It verifies if the actual outcome matches the expected outcome defined in the rules.

### Specific Test Categories

1. **Fund Policy Tests**: Tests related to fund creation, deletion, deposits, and withdrawals.
2. **Protocol Validator Tests**: Validates protocol-level operations and constraints.
3. **FundHolding Tests**: Ensures correct behavior of fund holding operations.
4. **InvestUnit Tests**: Verifies the functionality of investment unit-related operations.
5. **SwapOffer Tests**: Checks the correctness of swap offer creation and execution.

### Test Parameters and Generators

The system uses various parameter generators to create diverse test scenarios:

1. **intRangeParam**: Generates integer values within a specified range.
2. **posixTimeRangeParam**: Creates POSIXTime values for testing time-related functionalities.
3. **investUnitParam**: Generates investment unit data for testing fund operations.
4. **dependentDepositParam**: Creates deposit amounts based on other parameters.
5. **dependentWithdrawParam**: Generates withdrawal amounts considering various factors like commissions and fund lifecycle.

### Limitations and Considerations

1. **Complexity**: The system's flexibility comes at the cost of increased complexity in setup and maintenance.
2. **Rule Management**: Keeping the Excel-based rules synchronized with contract changes requires diligence.
3. **Performance**: Generating and running a large number of test cases can be time-consuming.
4. **Scope**: While comprehensive for basic scenarios, these tests do not cover all possible edge cases or complex logical interactions.

### Future Enhancements

1. **Dynamic Rule Generation**: Implement a system to programmatically generate rules based on contract specifications.
2. **Performance Optimization**: Introduce parallel test execution and smarter test case selection.
3. **Improved Reporting**: Develop more detailed and actionable test result reports.
4. **Integration with Manual Tests**: Develop a system to easily identify which scenarios are covered by automatic tests and which require manual testing focus.

The automatic testing suite provides a robust foundation for ensuring the correctness of the MAYZ Protocol's smart contracts. By systematically covering basic scenarios, it allows manual testing efforts to focus on more complex, logic-specific cases, thereby enhancing the overall quality and reliability of the protocol.