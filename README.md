# Data Marketplace Contract

## Overview
The **Data Marketplace Contract** is a decentralized smart contract that enables companies to request data and allows bidders to submit offers. The contract facilitates transparent transactions, automated pricing, and fair selection of data providers based on predefined criteria.

## Features
- **Data Requests**: Companies can create data requests specifying budget, purpose, and timeframe.
- **Bidding System**: Data providers can place bids on active data requests.
- **Bid Acceptance**: Companies can accept bids, finalizing the transaction.
- **Automated Pricing Suggestion**: The contract provides a suggested price based on budget and quality score.
- **Secure Access**: Only the request creator can accept bids.

## Contract Components

### Constants
- `contract-owner`: The contract deployer.
- `err-owner-only`: Error returned when a non-owner attempts a restricted action.
- `err-not-found`: Error returned when a requested entity does not exist.
- `err-already-exists`: Error for duplicate entries.
- `err-invalid-bid`: Error for invalid bid submissions.

### Data Structures
- `data-requests` (Map): Stores requests made by companies.
- `bids` (Map): Stores bids submitted for specific data requests.
- `request-id-nonce` (Variable): Keeps track of unique request IDs.

### Public Functions
#### 1. `create-data-request (budget uint, purpose string, timeframe uint) → (ok uint)`
Allows a company to create a new data request with the following parameters:
- `budget`: Maximum budget allocated for the data.
- `purpose`: Description of why the data is needed.
- `timeframe`: The time period for data delivery.

Returns the unique request ID.

#### 2. `place-bid (request-id uint, amount uint, quality-score uint) → (ok true)`
Allows data providers to place bids on active data requests.
- `request-id`: ID of the data request.
- `amount`: Bid amount (should be within the request budget).
- `quality-score`: Quality rating (0-100) of the bid submission.

#### 3. `accept-bid (request-id uint, bidder principal) → (ok true)`
Allows the requesting company to accept a bid.
- Only the request creator can perform this action.
- The request status is updated to "accepted."

#### 4. `get-data-request (request-id uint) → (option map)`
Returns the details of a specific data request.

#### 5. `get-bid (request-id uint, bidder principal) → (option map)`
Returns the details of a bid for a given request and bidder.

#### 6. `calculate-suggested-price (request-id uint, quality-score uint) → (ok uint)`
Calculates a suggested price for a bid based on:
- 10% of the request's budget as a base price.
- A multiplier derived from the quality score (0-1.0).

## Security Considerations
- **Authorization Checks**: Only the data request creator can accept bids.
- **Error Handling**: Proper assertions ensure valid data operations.
- **Bid Constraints**: Bids exceeding the budget are rejected.

## Future Enhancements
- Implement automated payments upon bid acceptance.
- Add a dispute resolution mechanism.
- Introduce reputation tracking for data providers.

## Conclusion
The **Data Marketplace Contract** enables a secure and transparent marketplace for data transactions, ensuring fair pricing and quality assessment for both data requesters and providers.