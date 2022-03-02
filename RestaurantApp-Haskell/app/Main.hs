import Data.Char (isDigit)
import Data.List
import Data.Time
import System.Directory
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = checkUserLoginState

data User = User {name :: String, password :: String, email :: String, phone :: String, balance :: String} deriving (Show)

data Food = Food {foodName :: String, price :: Int} deriving (Show)

data Order = Order {foodList :: [Food], totalOrder :: Int} deriving (Show)

headerLine = "------------------------------------\n"

headerData = "No.  |    Food     |     Price     |\n"

dividier = "=======================================\n"

calculateSpacePrefix :: Int -> String
calculateSpacePrefix i = replicate i ' '

calculateSpaceSufix :: Int -> Int -> String
calculateSpaceSufix i lengthChar = replicate (i - lengthChar) ' '

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
  let filename = "userTable.txt"
  isFileExit <- doesFileExist filename
  if isFileExit then putStrLn "" else writeFile filename ""
  contents <- readFile filename
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
  let filename = "userCookie.txt"
  isFileExit <- doesFileExist filename
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
  putStrLn "Registering User ..."
  let filename = "userTable.txt"
  isFileExit <- doesFileExist filename
  if isFileExit then putStrLn "" else writeFile filename ""
  let userData = constructUserTableFormat filename "" [User {name = name, email = email, password = password, phone = phone, balance = "10000"}]
  appendFile filename userData
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
  let filename = "userCookie.txt"
  writeFile filename ""
  contents <- readFile filename
  putStrLn contents
  appendFile filename $ "Email : $" ++ email ++ "$ Login Status : $True$\n"

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
  let filename = "menu.txt"
  contents <- readFile filename
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
  let filename = "menu.txt"
  contents <- readFile filename
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
        _   -> do
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
  let cookieFilename = "userCookie.txt"
  isFileExit <- doesFileExist cookieFilename
  if isFileExit
    then do
      amount <- inputAmount
      cookieContents <- readFile cookieFilename
      let cookieData = parseRawData cookieContents "" (False, [])
      let userEmail = head cookieData
      let userTableFilename = "userTable.txt"
      let userTableTempFilename = "userTableTemp.txt"
      copyFile userTableFilename userTableTempFilename
      userTableContent <- readFile userTableTempFilename
      let userTableData = parseToUser $ parseRawData userTableContent "" (False, [])
      let newUserData = changeUserBalance userEmail userTableData (+ amount)
      let newUserDataFormated = constructUserTableFormat userTableFilename "" newUserData
      writeFile userTableFilename newUserDataFormated
      removeFile userTableTempFilename
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
  let filename = "retaurantTransactionHistory.txt"
  isFileExit <- doesFileExist filename
  if isFileExit then putStrLn "" else writeFile filename ""
  topUpTime <- getZonedTime
  let topUpTransaction = "Date = $" ++ show topUpTime ++ "$\n" ++ "Transaction Type = $Top Up$\n" ++ "Amount = $" ++ show topUpAmount ++ "$\n" ++ dividier ++ "\n"
  putStrLn $ filter (/= '$') topUpTransaction
  appendFile filename topUpTransaction

sendTopUpMoneyToRestaurantBalance :: Int -> IO ()
sendTopUpMoneyToRestaurantBalance topUpBalance = do
  let filename = "retaurantBalance.txt"
  let filenameTemp = "retaurantBalanceTemp.txt"
  isFileExit <- doesFileExist filename
  if isFileExit then putStrLn "" else writeFile filename "Balance = $0$"
  copyFile filename filenameTemp
  contents <- readFile filenameTemp
  let balanceData = head $ parseRawData contents "" (False, [])
  let newBalance = topUpBalance + read balanceData
  writeFile filename $ "Balance = $" ++ show newBalance ++ "$"
  removeFile filenameTemp
  
checkUserBalance :: Order -> IO ()
checkUserBalance order = do
  let cookieFilename = "userCookie.txt"
  isFileExit <- doesFileExist cookieFilename
  if isFileExit
    then do
      cookieContents <- readFile cookieFilename
      let cookieData = parseRawData cookieContents "" (False, [])
      let userEmail = head cookieData
      let userTableFilename = "userTable.txt"
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
  let cookieFilename = "userCookie.txt"
  isFileExit <- doesFileExist cookieFilename
  if isFileExit
    then do
      cookieContents <- readFile cookieFilename
      let cookieData = parseRawData cookieContents "" (False, [])
      let userEmail = head cookieData
      let userTableFilename = "userTable.txt"
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
      let cookieFilename = "userCookie.txt"
      isFileExit <- doesFileExist cookieFilename
      if isFileExit
        then do
          let totalAmountOrder = totalOrder order
          userBalance <- getUserBalance
          if userBalance >= totalAmountOrder
            then do
              let totalAmountOrderDiscounted = totalAmountOrder - (totalAmountOrder * 10 `div` 100)
              cookieContents <- readFile cookieFilename
              let cookieData = parseRawData cookieContents "" (False, [])
              let userEmail = head cookieData
              let userTableFilename = "userTable.txt"
              let userTableFilenameTemp = "userTableTemp.txt"
              copyFile userTableFilename userTableFilenameTemp
              userTableContent <- readFile userTableFilenameTemp
              let userTableData = parseToUser $ parseRawData userTableContent "" (False, [])
              let newUserData = changeUserBalance userEmail userTableData $ (-) totalAmountOrder
              let newUserDataFormated = constructUserTableFormat userTableFilename "" newUserData
              writeFile userTableFilename newUserDataFormated
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
  let filename = "retaurantTransactionHistory.txt"
  isFileExit <- doesFileExist filename
  if isFileExit then putStrLn "" else writeFile filename ""
  topUpTime <- getZonedTime
  let orderTransaction = "Date = $" ++ show topUpTime ++ "$\n" ++ "Transaction Type = $Order Food$\n" ++ menu ++ "\n" ++ "Total Order Amount = $" ++ show orderAmount ++ "$\n" ++ dividier ++ "\n"
  let orderTransactionFormated = filter (/= '$') orderTransaction
  putStrLn orderTransactionFormated
  appendFile filename orderTransaction

removeCookie :: IO ()
removeCookie = do
  let filename = "userCookie.txt"
  isFileExit <- doesFileExist filename
  if isFileExit
    then do
      removeFile filename
      login
    else login