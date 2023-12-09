module Main (main) where

import MyLib

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Maybe

createBank = newTVarIO (M.empty, Account { currency = USD, money = 0 })

registerUserTest = do
    bank <- createBank

    atomically $ registerUser "A" bank

    (users, _) <- atomically $ readTVar bank
    let user = M.lookup "A" users

    isJust user @?= True

deleteUserTest = do
    bank <- createBank
    atomically $ registerUser "A" bank

    atomically $ deleteUser "A" bank

    (users, _) <- atomically $ readTVar bank
    let user = M.lookup "A" users
    isNothing user @?= True

createAccountTest = do
    bank <- createBank
    atomically $ registerUser "A" bank

    accountId <- atomically $ createAccount "A" USD bank

    accountId @?= 1

deleteAccountTest = do
    bank <- createBank
    atomically $ registerUser "A" bank
    accountId <- atomically $ createAccount "A" USD bank

    resultCode <- atomically $ deleteAccount "A" accountId bank

    resultCode @?= 1

addMoneyTest = do
    bank <- createBank
    atomically $ registerUser "A" bank
    accountId <- atomically $ createAccount "A" USD bank

    atomically $ addMoney "A" accountId 100 bank

    (users, bankAccount) <- atomically $ readTVar bank
    let Just user = M.lookup "A" users
    accounts <- atomically $ readTVar user
    let Just account = M.lookup accountId accounts

    money account @?= 100

takeMoneyTest = do
    bank <- createBank
    atomically $ registerUser "A" bank
    accountId <- atomically $ createAccount "A" USD bank
    atomically $ addMoney "A" accountId 200 bank

    (m, curr) <- atomically $ takeMoney "A" accountId 100 bank

    (users, bankAccount) <- atomically $ readTVar bank
    let Just user = M.lookup "A" users
    accounts <- atomically $ readTVar user
    let Just account = M.lookup accountId accounts

    money account @?= 100
    m @?= 99
    curr @?= USD
    money bankAccount @?= 1

forwardMoneyToUserTest = do
    bank <- createBank
    atomically $ registerUser "A" bank
    atomically $ registerUser "B" bank
    a_accountId <- atomically $ createAccount "A" USD bank
    b_accountId <- atomically $ createAccount "B" USD bank

    atomically $ addMoney "A" a_accountId 200 bank

    isOk <- atomically $ forwardMoneyToUser "A" a_accountId "B" b_accountId 100 USD bank

    (users, bankAccount) <- atomically $ readTVar bank
    let Just a_user = M.lookup "A" users
    a_accounts <- atomically $ readTVar a_user
    let Just a_account = M.lookup a_accountId a_accounts
    let Just b_user = M.lookup "B" users
    b_accounts <- atomically $ readTVar b_user
    let Just b_account = M.lookup b_accountId b_accounts

    isOk @?= True
    money a_account @?= 100
    money b_account @?= 99
    money bankAccount @?= 1

forwardMoneyTests = do
    bank <- createBank
    atomically $ registerUser "A" bank
    a_accountId <- atomically $ createAccount "A" USD bank
    b_accountId <- atomically $ createAccount "A" USD bank

    atomically $ addMoney "A" a_accountId 200 bank

    isOk <- atomically $ forwardMoney "A" a_accountId b_accountId 100 USD bank

    (users, bankAccount) <- atomically $ readTVar bank
    let Just a_user = M.lookup "A" users
    a_accounts <- atomically $ readTVar a_user
    let Just a_account = M.lookup a_accountId a_accounts
    let Just b_account = M.lookup b_accountId a_accounts

    isOk @?= True
    money a_account @?= 100
    money b_account @?= 99
    money bankAccount @?= 1

tests = testGroup "UNIT-tests"
  [
    testCase "Регистрация пользователей" registerUserTest,
    testCase "Удаление пользователей" deleteUserTest,
    testCase "Создание счета" createAccountTest,
    testCase "Удаление счета" deleteAccountTest,
    testCase "Положить деньги на счет" addMoneyTest,
    testCase "Снять деньги со счета" takeMoneyTest,
    testCase "Переслать деньги пользователю" forwardMoneyToUserTest,
    testCase "Переслать деньги между счетами" forwardMoneyTests
  ]

main = defaultMain tests
