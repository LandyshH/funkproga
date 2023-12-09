module MyLib where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map.Strict as M

data Currency = USD | RUB deriving (Show, Eq)

data Account = Account
    { currency :: Currency,
      money :: Money } deriving (Show, Eq)

type User = String
type Bank = TVar (M.Map User (TVar Accounts), Account)
type Accounts = M.Map Int Account
type Money = Int
type AccountId = Int

registerUser :: User -> Bank -> STM ()
registerUser name bank = do
    (users, bankAccount) <- readTVar bank
    if M.member name users
        then return ()
    else do
        accounts <- newTVar M.empty
        writeTVar bank (M.insert name accounts users, bankAccount)


deleteUser :: User -> Bank -> STM ()
deleteUser name bank = do
    (users, bankAccount) <- readTVar bank
    let Just user = M.lookup name users
    accounts <- readTVar user
    if M.null accounts then do
        let updatedUsers = M.delete name users
        writeTVar bank (updatedUsers, bankAccount)
    else return ()

createAccount :: User -> Currency -> Bank -> STM Int
createAccount name currency bankState = do
    (users, _) <- readTVar bankState
    let Just user = M.lookup name users
    accounts <- readTVar user
    let accountId = (if M.null accounts then 0 else fst (M.findMax accounts)) + 1
    let account = Account { currency = currency, money = 0 }
    writeTVar user $ M.insert accountId account accounts
    return accountId

deleteAccount :: User -> AccountId -> Bank -> STM Int
deleteAccount name accountId bankState = do
    (users, _) <- readTVar bankState
    let Just user = M.lookup name users
    userAccounts <- readTVar user
    case M.lookup accountId userAccounts of
      Nothing -> return (-1)
      Just account ->
        if money account == 0 then do
          let updatedAccounts = M.delete accountId userAccounts
          writeTVar user updatedAccounts
          return 1
        else return 0

addMoney :: User -> AccountId -> Money -> Bank -> STM ()
addMoney name accountId addedMoney bank = do
  (users, bankAcc) <- readTVar bank
  let Just user = M.lookup name users
  accounts <- readTVar user
  let Just account = M.lookup accountId accounts
  let updatedAccount = account { money = addedMoney + money account }
  let updatedAccounts = M.insert accountId updatedAccount accounts
  writeTVar user updatedAccounts

takeMoney :: User -> AccountId -> Money -> Bank -> STM (Money, Currency)
takeMoney name accountId takedMoney bank = do
  (users, bankAccount) <- readTVar bank
  let Just user = M.lookup name users
  accounts <- readTVar user
  let Just account = M.lookup accountId accounts

  if money account < takedMoney
    then return (0, currency account)
  else do
    let tax = calculateTax takedMoney
    let taxInUsd = convertMoney (currency account) USD tax
    let updatedAccount = account { money = money account - takedMoney }
    let updatedAccounts = M.insert accountId updatedAccount accounts
    writeTVar user updatedAccounts
    let bankUpdatedAccount = bankAccount { money = money bankAccount + taxInUsd }
    writeTVar bank (users, bankUpdatedAccount)
    return (takedMoney - tax, currency account)

forwardMoneyToUser :: User -> AccountId -> User -> AccountId -> Money -> Currency -> Bank -> STM Bool
forwardMoneyToUser fromName fromAccountId toName toAccountId forwardedMoney toCurrency bank = do
    (users, bankAccount) <- readTVar bank

    let Just fromUser = M.lookup fromName users
    fromAccounts <- readTVar fromUser
    let Just fromAccount = M.lookup fromAccountId fromAccounts

    let Just toUser = M.lookup toName users
    toAccounts <- readTVar toUser
    let Just toAccount = M.lookup toAccountId toAccounts

    (takedMoney, fromCurrency) <- takeMoney fromName fromAccountId forwardedMoney bank

    if takedMoney == 0 then return False
    else do
        let convertedMoney = convertMoney fromCurrency toCurrency takedMoney
        addMoney toName toAccountId convertedMoney bank
        return True

forwardMoney :: User -> AccountId -> AccountId -> Money -> Currency -> Bank -> STM Bool
forwardMoney name fromAccountId toAccountId forwardedMoney toCurrency bank = forwardMoneyToUser name fromAccountId name toAccountId forwardedMoney toCurrency bank

convertMoney :: Currency -> Currency -> Money -> Money
convertMoney USD RUB m = m * 90
convertMoney RUB USD m = m `div` 90
convertMoney _ _ m = m

calculateTax :: Int -> Int
calculateTax m = max 1 (m `div` 100)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
