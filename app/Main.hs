import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put, runState)
import Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import Data.Char (isDigit)
import Data.List
import Data.Time
import System.Directory

main :: IO ()
main = checkUserLoginState

data User = User {name :: String, password :: String, email :: String, phone :: String, balance :: String} deriving (Show)

data Food = Food {foodName :: String, price :: Int} deriving (Show)

data Order = Order {foodList :: [Food], totalOrder :: Int} deriving (Show)

headerLine = "------------------------------------\n"

headerData = "No.  |    Food     |     Price     |\n"

dividier = "=======================================\n"

menuFilename = "./assets/menu.txt"

retaurantBalanceFilename = "./assets/retaurantBalance.txt"

retaurantBalanceFilenameTemp = "./assets/retaurantBalanceTemp.txt"

retaurantTransactionHistoryFilename = "./assets/retaurantTransactionHistory.txt"

userTableFilename = "./assets/userTable.txt"

userTableFilenameTemp = "./assets/userTableTemp.txt"

userCookieFilename = "./assets/userCookie.txt"

calculateSpacePrefix :: Int -> String
calculateSpacePrefix i = replicate i ' '

calculateSpaceSufix :: Int -> Int -> String
calculateSpaceSufix i lengthChar = replicate (i - lengthChar) ' '

newtype Stack a = Stack {unStack :: StateT Int (WriterT String IO) a}

constructStack :: IO () -> String -> Stack ()
constructStack operation logMessage = Stack $ do
  liftIO operation
  liftIO $ print logMessage
  lift $ tell $ "Logging: " ++ logMessage
  return ()

constructUserStack :: IO () -> String -> User -> Stack ()
constructUserStack operation logMessage user = Stack $ do
  liftIO operation
  liftIO $ print logMessage
  lift $ tell $ "Logging: " ++ show user
  return ()

constructOrderStack :: IO () -> String -> Order -> Stack ()
constructOrderStack operation logMessage order = Stack $ do
  liftIO operation
  liftIO $ print logMessage
  lift $ tell $ "Logging: " ++ show order
  return ()

evalStack :: Stack a -> IO String
evalStack m = execWriterT (evalStateT (unStack m) 0)

executeEvalStack :: IO () -> String -> IO String
executeEvalStack operation logMessage = evalStack $ constructStack operation logMessage

executeUserEvalStack :: IO () -> String -> User -> IO String
executeUserEvalStack operation logMessage user = evalStack $ constructUserStack operation logMessage user

executeOrderEvalStack :: IO () -> String -> Order -> IO String
executeOrderEvalStack operation logMessage order = evalStack $ constructOrderStack operation logMessage order

login :: IO ()
login = do
  putStrLn "Welcome at Wangming Restaurant App"
  putStrLn "Please login as :\n1> User \n2> Guest\n3> Exit"
  loginType <- getLine
  case loginType of
    "1" -> doUserLogin
    "2" -> doUserOrderMenu Order {foodList = [], totalOrder = 0}
    "3" -> putStrLn "Thank you for using Wangming Restaurant App"
    _ -> do
      putStrLn "Please choose valid an option"
      login

doUserLogin :: IO ()
doUserLogin = do
  putStrLn "Wangming Restaurant Login Page"
  putStrLn "1> Login \n2> Register\n3> Exit"
  loginOption <- getLine
  case loginOption of
    "1" -> getUserLoginData
    "2" -> registerUser
    "3" -> do
      putStrLn "Exit page"
      login
    _ -> do
      putStrLn "Please choose valid option"
      doUserLogin

getUserLoginData :: IO ()
getUserLoginData = do
  putStrLn "Please enter your email"
  email <- getLine
  putStrLn "Please enter your password"
  password <- getLine
  putStrLn "Login . . ."
  loginValidation email password

loginValidation :: String -> String -> IO ()
loginValidation userEmail userPassword = do
  isFileExit <- doesFileExist userTableFilename
  if isFileExit then putStrLn "" else writeFile userTableFilename ""
  contents <- readFile userTableFilename
  let loginData = parseRawData contents "" (False, [])
  let userList = parseToUser loginData
  let isUserValid = isUserRegistered userEmail userPassword userList
  if isUserValid
    then do
      putStrLn "Success Login"
      createUserCookie userEmail
      doUserOrderMenu Order {foodList = [], totalOrder = 0}
    else do
      putStrLn "Login failure, email or password incorrect"
      doUserLogin

isUserRegistered :: String -> String -> [User] -> Bool
isUserRegistered _ _ [] = False
isUserRegistered userEmail userPassword (user : userList) = ((email user == userEmail) && (password user == userPassword)) || isUserRegistered userEmail userPassword userList

parseToUser :: [String] -> [User]
parseToUser [] = []
parseToUser (userName : userPass : userEmail : userPhone : userBalance : xs) =
  User
    { name = userName,
      password = userPass,
      email = userEmail,
      phone = userPhone,
      balance = userBalance
    } :
  parseToUser xs

checkUserLoginState :: IO ()
checkUserLoginState = do
  isFileExit <- doesFileExist userCookieFilename
  if isFileExit
    then doUserOrderMenu Order {foodList = [], totalOrder = 0}
    else login
parseRawData :: String -> String -> (Bool, [String]) -> [String]
parseRawData [] _ value = reverse $ snd value
parseRawData (x : xs) tempValue value = case x of
  '$' -> if fst value then parseRawData xs "" (False, reverse tempValue : snd value) else parseRawData xs "" (True, snd value)
  _ -> if fst value then parseRawData xs (x : tempValue) value else parseRawData xs tempValue value

registerUser :: IO ()
registerUser = do
  putStrLn "Register User Page"
  putStrLn "Please enter your name"
  name <- getLine
  putStrLn "Please enter your email"
  email <- getLine
  putStrLn "Please enter your password"
  password <- getLine
  putStrLn "Please enter your phone"
  phone <- getLine
  isFileExit <- doesFileExist userTableFilename
  if isFileExit then putStrLn "" else writeFile userTableFilename ""
  let userData = User {name = name, email = email, password = password, phone = phone, balance = "10000"}
  let userDataTableFormatted = constructUserTableFormat userTableFilename "" [User {name = name, email = email, password = password, phone = phone, balance = "10000"}]
  let registeringUser = appendFile userTableFilename userDataTableFormatted
  regusterUserResult <- executeUserEvalStack registeringUser "Registering User ..." userData
  print regusterUserResult
  createUserCookie email
  doUserOrderMenu Order {foodList = [], totalOrder = 0}

constructUserTableFormat :: String -> String -> [User] -> String
constructUserTableFormat fileName userData [] = userData
constructUserTableFormat fileName tempData (user : userList) = do
  let tempUserData = "Name : $" ++ name user ++ "$\n" ++ "Password : $" ++ password user ++ "$\nEmail : $" ++ email user ++ "$\n" ++ "Phone : $" ++ phone user ++ "$\nBalance : $" ++ balance user ++ "$\n" ++ dividier
  constructUserTableFormat fileName (tempData ++ tempUserData) userList

createUserCookie :: String -> IO ()
createUserCookie email = do
  putStrLn "Loading ..."
  writeFile userCookieFilename ""
  let createCookie = writeFile userCookieFilename $ "Email : $" ++ email ++ "$ Login Status : $True$\n"
  createdCookieResult <- executeEvalStack createCookie "Creating user cookie ..."
  print createdCookieResult

doUserOrderMenu :: Order -> IO ()
doUserOrderMenu order = do
  putStrLn "Wangming Restaurant Order Page"
  putStrLn "1> Order Menu \n2> Top Up \n3> Check Balance \n4> Check Order Menu \n5> Checkout Order \n6 Exit"
  orderOption <- getLine
  case orderOption of
    "1" -> orderMenu order
    "2" -> topUpBalance order
    "3" -> checkUserBalance order
    "4" -> seeUserOrderList order
    "5" -> checkoutUserOrder order
    "6" -> removeCookie
    _ -> do
      putStrLn "Please choose valid an option"
      doUserOrderMenu order

printMenu :: IO ()
printMenu = do
  contents <- readFile menuFilename
  let foodData = parseRawData contents "" (False, [])
  let foods = parseToFood foodData
  putStrLn $ constructMenu 1 "" foods

constructMenu :: Int -> String -> [Food] -> String
constructMenu _ previousData [] = headerLine ++ headerData ++ headerLine ++ previousData ++ headerLine
constructMenu index previousData (x : xs) = do
  let name = foodName x
  let foodPrice = price x
  let printResult = show index ++ "." ++ calculateSpaceSufix 4 (length $ show index) ++ "|" ++ calculateSpacePrefix 4 ++ name ++ calculateSpaceSufix 10 (length name) ++ "|" ++ calculateSpacePrefix 5 ++ show foodPrice ++ calculateSpaceSufix 10 (length $ show foodPrice) ++ "|\n"
  constructMenu (index + 1) (previousData ++ printResult) xs

parseToFood :: [String] -> [Food]
parseToFood [] = []
parseToFood (nameFood : priceFood : xs) = Food {foodName = nameFood, price = read priceFood :: Int} : parseToFood xs

orderMenu :: Order -> IO ()
orderMenu order = do
  contents <- readFile menuFilename
  let foodData = parseRawData contents "" (False, [])
  let foods = parseToFood foodData
  putStrLn $ constructMenu 1 "" foods
  putStrLn "Your current order list "
  print (foodList order)
  putStrLn "Please choose your action :\n1> Add order menu \n2> Remove order menu \n3> Exit"
  orderMenuOption <- getLine
  case orderMenuOption of
    "1" -> addOrderMenu foods order
    "2" -> removeOrderMenu order
    "3" -> doUserOrderMenu order
    _ -> do
      putStrLn "Please choose valid option"
      orderMenu order

addOrderMenu :: [Food] -> Order -> IO ()
addOrderMenu foodMenu order = do
  let foodsIndex = map show [1 .. length foodMenu]
  putStrLn "Please choose menu by choosing menu number or type E to exit add order page"
  menuOption <- getLine
  case menuOption of
    "E" -> orderMenu order
    _ -> do
      if menuOption `elem` foodsIndex
        then do
          let foodOrderList = foodList order
          let totalOrderPrice = totalOrder order
          let index = read menuOption :: Int
          let newOrderFood = foodMenu !! (index - 1)
          let foodPrice = price newOrderFood
          let newOrder = Order {foodList = newOrderFood : foodOrderList, totalOrder = foodPrice + totalOrderPrice}
          putStrLn "Do you want to order again ? Y> Yes N> No"
          orderOption <- getLine
          case orderOption of
            "Y" -> addOrderMenu foodMenu newOrder
            _ -> orderMenu newOrder
        else do
          putStrLn "Please input valid option"
          addOrderMenu foodMenu order

removeOrderMenu :: Order -> IO ()
removeOrderMenu order = do
  if null $ foodList order
    then do
      putStrLn "You havent order menu. Please order it first"
      orderMenu order
    else do
      let foodsIndex = map show [1 .. length $ foodList order]
      let menu = constructPrintOrder (foodList order) 1
      putStrLn menu
      putStrLn "Please choose number to remove from your order or type E to exit add order page"
      menuOption <- getLine
      case menuOption of
        "E" -> orderMenu order
        _ -> do
          if menuOption `elem` foodsIndex
            then do
              let foodOrderList = constructFoodsWithIndex (foodList order) 1
              let totalOrderPrice = totalOrder order
              let index = read menuOption :: Int
              let removedfoodPrice = price $ snd $ foodOrderList !! (index - 1)
              let newOrder = Order {foodList = removeOrderFoodByIndex foodOrderList index, totalOrder = totalOrderPrice - removedfoodPrice}
              putStrLn "Do you want to remove ordered food again ? Y> Yes N> No"
              orderOption <- getLine
              case orderOption of
                "Y" -> removeOrderMenu newOrder
                _ -> orderMenu newOrder
            else do
              putStrLn "Please input valid option"
              removeOrderMenu order

constructPrintOrder :: [Food] -> Int -> String
constructPrintOrder [] _ = "\n"
constructPrintOrder (food : xs) index = show index ++ ". " ++ foodName food ++ " Rp." ++ show (price food) ++ "\n" ++ constructPrintOrder xs (index + 1)

constructFoodsWithIndex :: [Food] -> Int -> [(Int, Food)]
constructFoodsWithIndex [] _ = []
constructFoodsWithIndex (food : xs) index = (index, food) : constructFoodsWithIndex xs (index + 1)

removeOrderFoodByIndex :: [(Int, Food)] -> Int -> [Food]
removeOrderFoodByIndex [] _ = []
removeOrderFoodByIndex (food : xs) index =
  if fst food /= index
    then snd food : removeOrderFoodByIndex xs index
    else removeOrderFoodByIndex xs index

seeUserOrderList :: Order -> IO ()
seeUserOrderList order = do
  if null $ foodList order
    then do
      putStrLn "You haven't order menu. Please order it first"
      doUserOrderMenu order
    else do
      let sortedFood = qsort $ foodList order
      let totalPrice = totalOrder order
      let printOrder = constructPrintOrder sortedFood 1
      putStrLn (printOrder ++ "Total Price = Rp." ++ show totalPrice)
      doUserOrderMenu order

qsort :: [Food] -> [Food]
qsort [] = []
qsort (x : xs) =
  qsort [a | a <- xs, price a < price x]
    ++ [x]
    ++ qsort [b | b <- xs, price b >= price x]

topUpBalance :: Order -> IO ()
topUpBalance order = do
  isFileExit <- doesFileExist userCookieFilename
  if isFileExit
    then do
      amount <- inputAmount
      cookieContents <- readFile userCookieFilename
      let cookieData = parseRawData cookieContents "" (False, [])
      let userEmail = head cookieData
      copyFile userTableFilename userTableFilenameTemp
      userTableContent <- readFile userTableFilenameTemp
      let userTableData = parseToUser $ parseRawData userTableContent "" (False, [])
      let newUserData = changeUserBalance userEmail userTableData (+ amount)
      let newUserDataFormated = constructUserTableFormat userTableFilename "" newUserData
      changeUserBalanceResult <- executeEvalStack (writeFile userTableFilename newUserDataFormated) "User balance updated"
      print changeUserBalanceResult
      removeFile userTableFilenameTemp
      createTopUpTransaction amount
      sendTopUpMoneyToRestaurantBalance amount
      putStrLn "Top Up Success"
      doUserOrderMenu order
    else do
      putStrLn "Sorry, you are not login. Please login"
      doUserOrderMenu order

changeUserBalance :: String -> [User] -> (Int -> Int) -> [User]
changeUserBalance _ [] _ = []
changeUserBalance userEmail (user : userList) changesOperator =
  if email user == userEmail
    then do
      let newBalance = changesOperator $ read (balance user)
      let newUser = User {name = name user, password = password user, email = email user, phone = phone user, balance = show newBalance}
      newUser : changeUserBalance userEmail userList changesOperator
    else user : changeUserBalance userEmail userList changesOperator

inputAmount :: IO Int
inputAmount = do
  putStrLn "Please input an valid amount"
  amount <- getLine
  let isAmountValid = all isDigit amount
  if isAmountValid
    then return (read amount :: Int)
    else inputAmount

createTopUpTransaction :: Int -> IO ()
createTopUpTransaction topUpAmount = do
  isFileExit <- doesFileExist retaurantTransactionHistoryFilename
  if isFileExit then putStrLn "" else writeFile retaurantTransactionHistoryFilename ""
  topUpTime <- getZonedTime
  let topUpTransaction = "Date = $" ++ show topUpTime ++ "$\n" ++ "Transaction Type = $Top Up$\n" ++ "Amount = $" ++ show topUpAmount ++ "$\n" ++ dividier ++ "\n"
  putStrLn $ filter (/= '$') topUpTransaction
  let createTopUpTransactionProcess = appendFile retaurantTransactionHistoryFilename topUpTransaction
  createTopUpTransactionResult <- executeEvalStack createTopUpTransactionProcess "Created top up transaction"
  print createTopUpTransactionResult

sendTopUpMoneyToRestaurantBalance :: Int -> IO ()
sendTopUpMoneyToRestaurantBalance topUpBalance = do
  isFileExit <- doesFileExist retaurantBalanceFilename
  if isFileExit then putStrLn "" else writeFile retaurantBalanceFilename "Balance = $0$"
  copyFile retaurantBalanceFilename retaurantBalanceFilenameTemp
  contents <- readFile retaurantBalanceFilenameTemp
  let balanceData = head $ parseRawData contents "" (False, [])
  let newBalance = topUpBalance + read balanceData
  let sendTopUpMoneyToRestaurantBalanceProcess = writeFile retaurantBalanceFilename $ "Balance = $" ++ show newBalance ++ "$"
  sendTopUpMoneyToRestaurantBalanceResult <- executeEvalStack sendTopUpMoneyToRestaurantBalanceProcess "Send user money to restaurant balance"
  print sendTopUpMoneyToRestaurantBalanceResult
  removeFile retaurantBalanceFilenameTemp

checkUserBalance :: Order -> IO ()
checkUserBalance order = do
  isFileExit <- doesFileExist userCookieFilename
  if isFileExit
    then do
      cookieContents <- readFile userCookieFilename
      let cookieData = parseRawData cookieContents "" (False, [])
      let userEmail = head cookieData
      userTableContent <- readFile userTableFilename
      let userTableData = parseToUser $ parseRawData userTableContent "" (False, [])
      let userBalance = checkBalance userEmail userTableData
      putStrLn $ "You balance is Rp." ++ show userBalance
      doUserOrderMenu order
    else do
      putStrLn "Sorry, you are not login. Please login"
      doUserOrderMenu order

checkBalance :: String -> [User] -> Int
checkBalance _ [] = 0
checkBalance userEmail (user : userList) =
  if email user == userEmail
    then do
      read (balance user)
    else checkBalance userEmail userList

getUserBalance :: IO Int
getUserBalance = do
  isFileExit <- doesFileExist userCookieFilename
  if isFileExit
    then do
      cookieContents <- readFile userCookieFilename
      let cookieData = parseRawData cookieContents "" (False, [])
      let userEmail = head cookieData
      userTableContent <- readFile userTableFilename
      let userTableData = parseToUser $ parseRawData userTableContent "" (False, [])
      return (checkBalance userEmail userTableData)
    else do
      return (-1)

checkoutUserOrder :: Order -> IO ()
checkoutUserOrder order = do
  if null $ foodList order
    then do
      putStrLn "You haven't order menu, please order it first"
      doUserOrderMenu order
    else do
      isFileExit <- doesFileExist userCookieFilename
      if isFileExit
        then do
          let totalAmountOrder = totalOrder order
          userBalance <- getUserBalance
          if userBalance >= totalAmountOrder
            then do
              let totalAmountOrderDiscounted = totalAmountOrder - (totalAmountOrder * 10 `div` 100)
              cookieContents <- readFile userCookieFilename
              let cookieData = parseRawData cookieContents "" (False, [])
              let userEmail = head cookieData
              copyFile userTableFilename userTableFilenameTemp
              userTableContent <- readFile userTableFilenameTemp
              let userTableData = parseToUser $ parseRawData userTableContent "" (False, [])
              let newUserData = changeUserBalance userEmail userTableData $ (-) totalAmountOrder
              let newUserDataFormated = constructUserTableFormat userTableFilename "" newUserData
              let changeUserTableDataProcess = writeFile userTableFilename newUserDataFormated
              changeUserTableDataResult <- executeEvalStack changeUserTableDataProcess "User balance updated"
              removeFile userTableFilenameTemp
              createOrderTransaction order totalAmountOrderDiscounted
              sendTopUpMoneyToRestaurantBalance totalAmountOrderDiscounted
              putStrLn "Checkout order success"
              doUserOrderMenu Order {foodList = [], totalOrder = 0}
            else do
              putStrLn "Your balance is not enough :(\n Please top up your balance first"
              doUserOrderMenu order
        else do
          createOrderTransaction order $ totalOrder order
          sendTopUpMoneyToRestaurantBalance $ totalOrder order
          putStrLn "Checkout order success"
          doUserOrderMenu Order {foodList = [], totalOrder = 0}

createOrderTransaction :: Order -> Int -> IO ()
createOrderTransaction order orderAmount = do
  let sortedFood = qsort $ foodList order
  let menu = constructPrintOrder sortedFood 1
  isFileExit <- doesFileExist retaurantTransactionHistoryFilename
  if isFileExit
    then putStrLn ""
    else do
      result <- executeEvalStack (writeFile retaurantTransactionHistoryFilename "") "Creating Retaurant Transaction History table"
      print result
  topUpTime <- getZonedTime
  let orderTransaction = "Date = $" ++ show topUpTime ++ "$\n" ++ "Transaction Type = $Order Food$\n" ++ menu ++ "\n" ++ "Total Order Amount = $" ++ show orderAmount ++ "$\n" ++ dividier ++ "\n"
  let orderTransactionFormated = filter (/= '$') orderTransaction
  putStrLn orderTransactionFormated
  let createOrderTransactionProcess = appendFile retaurantTransactionHistoryFilename orderTransaction
  createOrderTransactionResult <- executeEvalStack createOrderTransactionProcess "Created order transaction"
  print createOrderTransactionResult

removeCookie :: IO ()
removeCookie = do
  isFileExit <- doesFileExist userCookieFilename
  if isFileExit
    then do
      result <- executeEvalStack (removeFile userCookieFilename) "Clear temporary cache"
      print result 
      login
    else login
