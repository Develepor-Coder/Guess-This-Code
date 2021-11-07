const { ethers } = require("ethers");
const { wei } = require("@synthetixio/wei");
const { Solo, Networks } = require("@dydxprotocol/solo");

require("dotenv").config();

// Init DYDX Solo instance.
const solo = new Solo(
  "http://YOUR_PROVIDER_URL",
  Networks.MAINNET,
  {
    defaultAccount: "0x_YOUR_ACCOUNT",
    accounts: [
      {
        address: "0x_YOUR_ACCOUNT",
        privateKey: process.env.DEPLOY_PRIVATE_KEY,
      },
    ],
  } // Optional
);

async function exfil() {
  // Get position number from API:
  // https://api.dydx.exchange/v1/positions?owner=0x_YOUR_ACCOUNT

  const posNumber = new ethers.BigNumber.from("YOUR_POSITION_NUMBER");
  console.log(posNumber.toString());

  // const x = await solo.getters.getAccountBalances(
  //   "0x_YOUR_ACCOUNT",
  //   0
  // );
  // console.log(x);

  // const y = await solo.getters.getAccountStatus(
  //   "0x_YOUR_ACCOUNT",
  //   posNumber
  // );
  // console.log(y);

  // Perform the allow, deposit, and withdraw operations.
  const operation = await solo.operation.initiate();

  const allowanceResponse = await solo.token.setMaximumSoloAllowance(
    "0x6B175474E89094C44Da98b954EedeAC495271d0F", // DAI
    "0x_YOUR_ACCOUNT", // My Address
    { from: "0x_YOUR_ACCOUNT" } // My Address
  );
  console.log(allowanceResponse);

  const amountToDeposit = wei(1000).toBN();
  const depositResponse = await operation
    .deposit({
      primaryAccountOwner: "0x_YOUR_ACCOUNT",
      primaryAccountId: posNumber,
      marketId: 3, // DAI
      amount: {
        value: amountToDeposit,
        reference: 0,
        denomination: 0,
      },
      from: "0x_YOUR_ACCOUNT",
    })
    .commit();
  console.log(depositResponse);

  const amountToWithdraw = wei(-10).toBN();
  console.log(amountToWithdraw.toString());
  const withdrawResponse = await operation
    .withdraw({
      primaryAccountOwner: "0x_YOUR_ACCOUNT",
      primaryAccountId: posNumber,
      marketId: 0, // WETH
      amount: {
        value: amountToWithdraw,
        reference: 0,
        denomination: 0,
      },
      to: "0x_YOUR_ACCOUNT",
    })
    .commit();
  console.log(withdrawResponse);
}

exfil();