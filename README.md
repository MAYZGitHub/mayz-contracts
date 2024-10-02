# MAYZ Protocol: Smart Contracts

## Table of Contents
- [MAYZ Protocol: Smart Contracts](#mayz-protocol-smart-contracts)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Key Features](#key-features)
  - [Architecture Overview](#architecture-overview)
  - [Smart Contracts](#smart-contracts)
  - [Key Concepts](#key-concepts)
    - [Investment Unit (IU)](#investment-unit-iu)
    - [Fund Tokens (FTs)](#fund-tokens-fts)
    - [Multi-UTXO Fund Holdings](#multi-utxo-fund-holdings)
    - [Re-indexing Process](#re-indexing-process)
    - [Commission System](#commission-system)
  - [Repository Structure](#repository-structure)
  - [Development Environment](#development-environment)
  - [Testing](#testing)
  - [Deployment](#deployment)
  - [Security Considerations](#security-considerations)
  - [Token Economics](#token-economics)
  - [Oracle Integration](#oracle-integration)
  - [Development Roadmap](#development-roadmap)
  - [Community and Support](#community-and-support)
  - [Contributing](#contributing)

## Introduction

Welcome to the MAYZ Protocol Smart Contracts Repository. This repository contains the core smart contracts that power the MAYZ Protocol, a groundbreaking decentralized finance (DeFi) ecosystem built on the Cardano blockchain. Our contracts are written in Plutus, Cardano's native smart contract language, aiming to democratize finance by providing a permissionless platform for creating and managing investment funds, facilitating liquidity, and empowering users through innovative mechanisms.

## Key Features

- **Decentralized Investment Funds**: Create, manage, and participate in a diverse range of investment funds composed of Cardano native tokens.
- **Dynamic Fund Tokens (FTs)**: Represent fund shares with real-time valuation based on underlying assets.
- **Swap Offers**: Seamlessly exchange ADA for Fund Tokens and vice versa through a decentralized marketplace.
- **Oracle-Powered Pricing**: Ensure accurate and up-to-date asset valuations through a robust oracle system.
- **Flexible Re-indexing**: Adapt fund compositions to market conditions while maintaining fairness and transparency.
- **Multi-UTXO Fund Holdings**: Optimize concurrency and scalability through innovative fund structure design.

## Architecture Overview

The MAYZ Protocol is built on a modular architecture, leveraging the power and security of Cardano's extended UTXO model. Our system comprises several interconnected components:

1. **Fund Factory**: Facilitates the creation and management of decentralized investment funds.
2. **Swap Offer Engine**: Enables liquidity provision and token exchanges.
3. **Oracle Service**: Provides real-time price data for accurate asset valuation.
4. **Re-indexing Mechanism**: Allows dynamic fund composition adjustments.
5. **Multi-UTXO Management**: Optimizes fund operations and user concurrency.

## Smart Contracts

Our protocol utilizes a suite of advanced smart contracts to ensure secure and efficient operations:

1. **Fund Contract**: Manages the creation, operation, and liquidation of investment funds.
   - Key functions: deposit, withdraw, re-index
   - Located in `src/Protocol/Fund/`

2. **Sell Offer Contract** (aka Swap Offer): Facilitates the exchange of ADA and Fund Tokens.
   - Key functions: create offer, execute swap, cancel offer
   - Located in `src/Protocol/SellOffer/`

3. **Invest Unit Contract**: Handles the representation and valuation of fund compositions.
   - Key functions: create invest unit, update invest unit
   - Located in `src/Protocol/InvestUnit/`

4. **Protocol Contract**: Manages global protocol parameters and upgrades.
   - Key functions: update parameters, emergency actions
   - Located in `src/Protocol/Protocol/`

## Key Concepts

### Investment Unit (IU)
- Defines the token composition of each fund
- Supports up to two decimal places for token amounts
- Impacts deposit and withdrawal operations

### Fund Tokens (FTs)
- Represent shares in a fund
- Minted upon deposit, burned upon withdrawal
- Value tied to the underlying assets' performance

### Multi-UTXO Fund Holdings
- Funds use multiple UTXOs to improve concurrency and scalability
- Introduces complexity in balancing and re-indexing operations

### Re-indexing Process
- Allows changes to fund composition
- Performed on a single Fund Holding UTXO but affects the entire fund
- Maintains constant total fund value in ADA

### Commission System
- Commissions paid upfront for the remaining fund lifetime
- Partial refunds available upon early withdrawal

## Repository Structure

```
mayz-protocol-contracts/
├── src/
│   ├── Protocol/
│   │   ├── Fund/
│   │   ├── SellOffer/
│   │   ├── InvestUnit/
│   │   ├── Protocol/
│   │   └── Constants.hs
│   ├── Generic/
│   │   ├── Types.hs
│   │   └── OnChainHelpers.hs
│   └── Main.hs
├── test/
├── scripts/
├── docs/
└── README.md
```

## Development Environment

- Haskell version: [specify version]

To set up the development environment:

1. Clone this repository
2. Run `cabal build` to compile the contracts

## Testing

We employ a comprehensive testing strategy:

- Unit tests for individual contract functions
- Property-based tests for edge cases
- Automatic tests
- Performance tests
- Transactions tests
- Testnet deployment for real-world scenario testing

Run tests with:
```
cabal test
```

## Deployment

[TODO: Add deployment instructions]

## Security Considerations

- Rounding errors: Commission calculations use high-precision arithmetic
- Oracle dependency: Price manipulations are mitigated through multi-source oracles

We are committed to ongoing security audits and encourage responsible disclosure of any vulnerabilities.

## Token Economics

The $MAYZ token is the lifeblood of our ecosystem, serving multiple crucial functions:

- **Fund Creation**: Lock $MAYZ to create and manage investment funds.
- **Liquidity Provision**: Stake $MAYZ to create Swap Offers and earn commissions.
- **Incentivization**: Reward active participants and align community interests.

Our tokenomics model is designed to foster long-term engagement, discourage malicious behavior, and ensure the protocol's sustainable growth.

## Oracle Integration

Accurate price data is crucial for the proper functioning of our protocol. Our oracle system:

- Aggregates price data from multiple reputable sources (e.g., CoinGecko, TAPtools).
- Implements advanced error detection and correction mechanisms.
- Provides real-time updates to ensure fair valuations and swaps.
- Integrates seamlessly with smart contracts for on-chain price verification.

## Development Roadmap

[Include key points from the development roadmap section]

## Community and Support

Join our vibrant community and stay updated on the latest developments:

- [Website](https://mayz.io)
- [Twitter](https://twitter.com/MAYZprotocol)
- [Discord](https://discord.gg/mayzprotocol)
- [Medium](https://medium.com/@MAYZprotocol)

For technical support or to report issues, please open a ticket in our GitHub repository or reach out to our support team on Discord.

## Contributing

We welcome contributions from the community. Please follow these steps:

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a new Pull Request

Please ensure your code adheres to our style guide and passes all tests.

---

For more detailed documentation on each contract and function, please refer to the `docs/` directory. If you have any questions or need assistance, feel free to open an issue or reach out to our development team on Discord.