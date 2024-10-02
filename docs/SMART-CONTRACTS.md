# MAYZ Protocol: Smart Contracts Documentation

## Table of Contents
- [MAYZ Protocol: Smart Contracts Documentation](#mayz-protocol-smart-contracts-documentation)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Protocol Contract](#protocol-contract)
    - [Datums](#datums)
    - [Redeemers](#redeemers)
    - [Key Functions](#key-functions)
  - [Fund Contract](#fund-contract)
    - [Datums](#datums-1)
    - [Redeemers](#redeemers-1)
    - [Key Functions](#key-functions-1)
  - [Fund Holding Contract](#fund-holding-contract)
    - [Datums](#datums-2)
    - [Redeemers](#redeemers-2)
    - [Key Functions](#key-functions-2)
  - [Invest Unit Contract](#invest-unit-contract)
    - [Datums](#datums-3)
    - [Redeemers](#redeemers-3)
    - [Key Functions](#key-functions-3)
  - [Swap Offer Contract](#swap-offer-contract)
    - [Datums](#datums-4)
    - [Redeemers](#redeemers-4)
    - [Key Functions](#key-functions-4)
  - [Delegation Contract](#delegation-contract)
    - [Datums](#datums-5)
    - [Redeemers](#redeemers-5)
    - [Key Functions](#key-functions-5)
  - [Script Contract](#script-contract)
    - [Datums](#datums-6)
    - [Redeemers](#redeemers-6)
    - [Key Functions](#key-functions-6)
  - [Minting Policies](#minting-policies)
  - [Interactions Between Contracts](#interactions-between-contracts)
  - [Security Considerations](#security-considerations)

## Introduction
This document provides a comprehensive overview of the smart contracts that power the MAYZ Protocol. Each contract plays a crucial role in the ecosystem, enabling the creation and management of decentralized investment funds, facilitating token swaps, and ensuring the overall security and efficiency of the protocol.

## Protocol Contract
The Protocol Contract serves as the backbone of the MAYZ Protocol, managing global parameters and administrative functions.

### Datums
- **ProtocolDatum**: Stores global protocol parameters, including:
  - Protocol version
  - Oracle payment public key
  - Admin wallets
  - Fund categories
  - Commission rates
  - Required MAYZ for various operations

### Redeemers
- **ValidatorRedeemerDatumUpdate**: Updates modifiable protocol parameters
- **ValidatorRedeemerUpdateMinADA**: Updates the minimum ADA requirement
- **ValidatorRedeemerEmergency**: Allows emergency actions with special admin token

### Key Functions
- Initialize protocol parameters
- Update global settings
- Manage admin access
- Handle emergency situations

## Fund Contract
The Fund Contract manages the lifecycle of individual investment funds within the MAYZ Protocol.

### Datums
- **FundDatum**: Stores fund-specific information, including:
  - Fund policy ID
  - Fund token name
  - Admin wallets
  - Fund category
  - Start and end dates
  - Commission rates
  - Holdings count and index

### Redeemers
- **ValidatorRedeemerDatumUpdate**: Updates fund parameters
- **ValidatorRedeemerFundHoldingAdd**: Adds a new fund holding
- **ValidatorRedeemerFundHoldingDelete**: Removes a fund holding
- **ValidatorRedeemerFinish**: Closes the fund
- **ValidatorRedeemerDelete**: Deletes the fund
- **ValidatorRedeemerUpdateMinADA**: Updates the minimum ADA requirement

### Key Functions
- Create and initialize new funds
- Manage fund parameters
- Handle fund holdings
- Process deposits and withdrawals
- Close and delete funds

## Fund Holding Contract
The Fund Holding Contract manages individual holdings within a fund, optimizing for concurrency and scalability.

### Datums
- **FundHoldingDatum**: Stores information about a specific fund holding, including:
  - Holding index
  - Minted FT subtotals
  - Commission subtotals
  - Minimum ADA requirement

### Redeemers
- **ValidatorRedeemerDeposit**: Processes user deposits
- **ValidatorRedeemerWithdraw**: Handles user withdrawals
- **ValidatorRedeemerReIndexing**: Adjusts holding composition during re-indexing
- **ValidatorRedeemerCollect_X_Commission**: Collects commissions for various parties
- **ValidatorRedeemerDelete**: Removes the holding
- **ValidatorRedeemerUpdateMinADA**: Updates the minimum ADA requirement
- **ValidatorRedeemerBalanceAssets**: Rebalances assets across holdings

### Key Functions
- Process user deposits and withdrawals
- Manage fund token (FT) minting and burning
- Handle commission calculations and collections
- Support fund re-indexing operations
- Balance assets across multiple holdings

## Invest Unit Contract
The Invest Unit Contract manages the composition of investment units within funds.

### Datums
- **InvestUnitDatum**: Defines the token composition of an investment unit

### Redeemers
- **ValidatorRedeemerReIndexing**: Updates the invest unit composition during re-indexing
- **ValidatorRedeemerUpdateMinADA**: Updates the minimum ADA requirement

### Key Functions
- Define and update fund compositions
- Support re-indexing operations
- Ensure proper valuation of investment units

## Swap Offer Contract
The Swap Offer Contract facilitates the exchange of ADA for Fund Tokens (FTs) and vice versa.

### Datums
- **SwapOfferDatum**: Stores information about a swap offer, including:
  - Seller details
  - Commission rates
  - Available FT and ADA amounts
  - Offer status

### Redeemers
- **ValidatorRedeemerUpdateStatus**: Changes the offer status
- **ValidatorRedeemerUpdateAskedCommissionRate**: Updates the commission rate
- **ValidatorRedeemerUpdateMinADA**: Updates the minimum ADA requirement
- **ValidatorRedeemerDeposit**: Adds funds to the offer
- **ValidatorRedeemerWithdraw**: Removes funds from the offer
- **ValidatorRedeemerSwapFTxADA**: Executes a swap of FT for ADA
- **ValidatorRedeemerSwapADAxFT**: Executes a swap of ADA for FT
- **ValidatorRedeemerDelete**: Removes the swap offer

### Key Functions
- Create and manage swap offers
- Process token exchanges
- Handle commissions for swaps
- Ensure price accuracy through oracle integration

## Delegation Contract
The Delegation Contract manages the delegation of MAYZ tokens to specific funds.

### Datums
- **DelegationDatum**: Stores information about a delegation, including:
  - Delegator details
  - Delegated MAYZ amount
  - Collected commissions

### Redeemers
- **ValidatorRedeemerDeposit**: Adds MAYZ to the delegation
- **ValidatorRedeemerWithdraw**: Removes MAYZ from the delegation
- **ValidatorRedeemerDelete**: Removes the delegation
- **ValidatorRedeemerUpdateMinADA**: Updates the minimum ADA requirement

### Key Functions
- Manage MAYZ token delegations to funds
- Process commission distributions to delegators
- Handle delegation deposits and withdrawals

## Script Contract
The Script Contract oversees deployed scripts on-chain, allowing for script management and ADA recovery.

### Datums
- **ScriptDatum**: Stores information about deployed scripts, including:
  - Admin wallet details
  - Script hash

### Redeemers
- **ValidatorRedeemerScriptDelete**: Removes a script and recovers locked ADA

### Key Functions
- Track deployed scripts
- Manage script lifecycle
- Facilitate ADA recovery from unused scripts

## Minting Policies
The MAYZ Protocol employs several minting policies to manage various tokens within the ecosystem:
- **Protocol ID Policy**: Mints the unique Protocol ID token
- **Fund ID Policy**: Mints Fund ID and Invest Unit ID tokens for each fund
- **Fund Holding ID Policy**: Mints Fund Holding ID tokens for each fund holding
- **Fund Token (FT) Policy**: Manages the minting and burning of fund-specific tokens
- **Swap Offer ID Policy**: Mints Swap Offer ID tokens
- **Delegation ID Policy**: Mints Delegation ID tokens
- **Script ID Policy**: Mints Script ID tokens for deployed scripts

Each policy ensures the uniqueness and integrity of its respective tokens within the protocol.

## Interactions Between Contracts
The MAYZ Protocol's smart contracts interact in complex ways to provide a seamless and secure user experience:
- The Protocol Contract sets global parameters used by all other contracts.
- Fund Contracts create and manage funds, interacting with Fund Holding and Invest Unit contracts.
- Fund Holding Contracts manage individual holdings, coordinating with the Fund Contract for deposits, withdrawals, and re-indexing.
- The Invest Unit Contract works closely with Fund and Fund Holding contracts during re-indexing operations.
- The Swap Offer contract interacts with Fund contracts to facilitate token exchanges.
- The Delegation Contract interacts with Fund contracts for commission distributions.
- The Script Contract manages the lifecycle of all deployed scripts, including the other contracts.

These interactions are carefully designed to maintain the integrity and security of the entire protocol.

## Security Considerations
The MAYZ Protocol implements several security measures across its smart contracts:
- **Access Control**: Admin functions are protected by signature checks or admin tokens.
- **Oracle Integration**: Price data for token swaps and re-indexing is provided by a secure, multi-source oracle system.
- **Commission Calculations**: High-precision arithmetic is used to prevent rounding errors in commission calculations.
- **Re-indexing Safeguards**: The re-indexing process includes checks to ensure the total fund value remains constant.
- **Multi-UTXO Design**: Fund holdings are spread across multiple UTXOs to enhance concurrency and resist certain types of attacks.
- **Emergency Mechanisms**: The Protocol Contract includes provisions for handling emergency situations.

Regular security audits and open-source development practices contribute to the ongoing security efforts of the protocol.

For more detailed information on each contract's implementation, please refer to the source code and accompanying comments in the `src/Protocol/` directory.
