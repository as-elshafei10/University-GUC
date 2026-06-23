data Month = Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec   deriving (Show, Eq)

type Date = (Int, Month, Int)
type Price = Float
type Quantity = Int

type Supply = (String, Quantity, Price)

type Delivery = (Date, [Supply]) 

data Ingredient = 
    SimpleIngredient String
  | Recipe String [Ingredient] deriving (Show, Eq) 

data Expense = 
    Item String Price Date              
  | Category String [Expense]     
  deriving (Show, Eq)
  
  
  
daysInMonth :: Month -> Int
daysInMonth m
	| m == Jan = 31
	| m == Feb = 28
	| m == Mar = 31
	| m == Apr = 30
	| m == May = 31
	| m == Jun = 30
	| m == Jul = 31
	| m == Aug = 31
	| m == Sep = 30
	| m == Oct = 31
	| m == Nov = 30
	| otherwise = 31

previousMonth :: Month -> Month
previousMonth m
	| m == Jan = Dec
	| m == Feb = Jan
	| m == Mar = Feb
	| m == Apr = Mar
	| m == May = Apr
	| m == Jun = May
	| m == Jul = Jun
	| m == Aug = Jul
	| m == Sep = Aug
	| m == Oct = Sep
	| m == Nov = Oct
	| otherwise = Nov
	

previousMonthYear :: Month -> Int -> (Month,Int)
previousMonthYear m y 
	| m == Jan = (previousMonth m,(y - 1))
	| otherwise = (previousMonth m, y)
	

subtractDaysFromDate :: Date -> Int -> Date
subtractDaysFromDate (day , month , year) 0 = (day , month , year)
subtractDaysFromDate (day , month , year) subtractedDays
	| day > subtractedDays = ((day - subtractedDays), month, year)
	| otherwise = subtractDaysFromDate (newDay, newMonth, newYear) remainingDays where
	 (newMonth , newYear) = previousMonthYear month year
	 newDay = daysInMonth newMonth
	 remainingDays = subtractedDays - day

	 
getIngredientInfo :: String -> [(String, Int, Price)] -> (Int, Price)
getIngredientInfo ingredient [] = error "Ingredient Not Found"
getIngredientInfo ingredient ((name , del , price): t)
	| ingredient == name = (del , price)
	| otherwise = getIngredientInfo ingredient t

	
calculateSimpleDelivery :: Date -> String -> (Date, (String, Price))
calculateSimpleDelivery requiredDate ingredient = (deliveryDate, (ingredient, price))
    where
        (days, price) = getIngredientInfo ingredient ingredient_info
        deliveryDate = subtractDaysFromDate requiredDate days
	

calculateDeliveryDates :: Date -> [Ingredient] -> [(Date, (String, Price))]
calculateDeliveryDates requiredDate [] = []
calculateDeliveryDates requiredDate ((SimpleIngredient ingredient):rest) = calculateSimpleDelivery requiredDate ingredient : calculateDeliveryDates requiredDate rest
calculateDeliveryDates requiredDate ((Recipe recipeName ingredients):rest) = calculateDeliveryDates requiredDate ingredients ++ calculateDeliveryDates requiredDate rest

getIngredientsForDate :: Date -> [(Date, [Ingredient])] -> [Ingredient]
getIngredientsForDate wantedDate [] = []
getIngredientsForDate wantedDate ((date, ingredients):rest)
    | wantedDate == date = ingredients
    | otherwise = getIngredientsForDate wantedDate rest


allDeliveryItems :: [Date] -> [(Date, (String, Price))]
allDeliveryItems [] = []
allDeliveryItems (date:rest) =
    calculateDeliveryDates date (getIngredientsForDate date shopping_list) ++ allDeliveryItems rest


addSupplyToList :: Supply -> [Supply] -> [Supply]
addSupplyToList (ingredient, qty, price) [] = [(ingredient, qty, price)]
addSupplyToList (ingredient, qty, price) ((name, oldQty, oldPrice):rest)
    | ingredient == name = (name, oldQty + qty, oldPrice + price) : rest
    | otherwise = (name, oldQty, oldPrice) : addSupplyToList (ingredient, qty, price) rest


addDeliveryItem :: (Date, (String, Price)) -> [Delivery] -> [Delivery]
addDeliveryItem (date, (ingredient, price)) [] =
    [(date, [(ingredient, 1, price)])]

addDeliveryItem (date, (ingredient, price)) ((deliveryDate, supplies):rest)
    | date == deliveryDate =
        (deliveryDate, addSupplyToList (ingredient, 1, price) supplies) : rest
    | otherwise =
        (deliveryDate, supplies) : addDeliveryItem (date, (ingredient, price)) rest


groupDeliveryItems :: [(Date, (String, Price))] -> [Delivery]
groupDeliveryItems [] = []
groupDeliveryItems (item:rest) =
    addDeliveryItem item (groupDeliveryItems rest)
	

monthNumber :: Month -> Int
monthNumber m
    | m == Jan = 1
    | m == Feb = 2
    | m == Mar = 3
    | m == Apr = 4
    | m == May = 5
    | m == Jun = 6
    | m == Jul = 7
    | m == Aug = 8
    | m == Sep = 9
    | m == Oct = 10
    | m == Nov = 11
    | otherwise = 12


dateBeforeOrEqual :: Date -> Date -> Bool
dateBeforeOrEqual (d1, m1, y1) (d2, m2, y2)
    | y1 < y2 = True
    | y1 > y2 = False
    | monthNumber m1 < monthNumber m2 = True
    | monthNumber m1 > monthNumber m2 = False
    | otherwise = d1 <= d2


insertDeliverySorted :: Delivery -> [Delivery] -> [Delivery]
insertDeliverySorted delivery [] = [delivery]
insertDeliverySorted (date1, supplies1) ((date2, supplies2):xs)
    | dateBeforeOrEqual date1 date2 = (date1, supplies1) : (date2, supplies2) : xs
    | otherwise = (date2, supplies2) : insertDeliverySorted (date1, supplies1) xs


sortDeliveries :: [Delivery] -> [Delivery]
sortDeliveries [] = []
sortDeliveries (x:xs) =
    insertDeliverySorted x (sortDeliveries xs)


insertSupplySorted :: Supply -> [Supply] -> [Supply]
insertSupplySorted supply [] = [supply]
insertSupplySorted (name1, qty1, price1) ((name2, qty2, price2):xs)
    | name1 <= name2 = (name1, qty1, price1) : (name2, qty2, price2) : xs
    | otherwise = (name2, qty2, price2) : insertSupplySorted (name1, qty1, price1) xs


sortSupplies :: [Supply] -> [Supply]
sortSupplies [] = []
sortSupplies (x:xs) =
    insertSupplySorted x (sortSupplies xs)


sortSuppliesInDeliveries :: [Delivery] -> [Delivery]
sortSuppliesInDeliveries [] = []
sortSuppliesInDeliveries ((date, supplies):rest) =
    (date, sortSupplies supplies) : sortSuppliesInDeliveries rest
	
summarizeAllDeliveries :: [Date] -> [Delivery]
summarizeAllDeliveries dates =
    sortDeliveries (sortSuppliesInDeliveries (groupDeliveryItems (allDeliveryItems dates)))
	
suppliesToExpenses :: Date -> [Supply] -> [Expense]
suppliesToExpenses date [] = []
suppliesToExpenses date ((name, qty, price):rest) =
    Item name price date : suppliesToExpenses date rest


deliveriesToExpenses :: [Delivery] -> [Expense]
deliveriesToExpenses [] = []
deliveriesToExpenses ((date, supplies):rest) =
    suppliesToExpenses date supplies ++ deliveriesToExpenses rest


getDeliveryExpenses :: [Delivery] -> Expense
getDeliveryExpenses deliveries =
    Category "Food Supplies" (deliveriesToExpenses deliveries)
	
	
countOccurrences :: String -> [String] -> Int
countOccurrences dish [] = 0
countOccurrences dish (x:xs)
    | dish == x = 1 + countOccurrences dish xs
    | otherwise = countOccurrences dish xs


removeAll :: String -> [String] -> [String]
removeAll dish [] = []
removeAll dish (x:xs)
    | dish == x = removeAll dish xs
    | otherwise = x : removeAll dish xs


removeDuplicates :: [String] -> [String]
removeDuplicates [] = []
removeDuplicates (x:xs) =
    x : removeDuplicates (removeAll x xs)


maxTwo :: Int -> Int -> Int
maxTwo a b
    | a > b = a
    | otherwise = b


maxDishCount :: [String] -> [String] -> Int
maxDishCount [] allDishes = 0
maxDishCount (x:xs) allDishes =
    maxTwo (countOccurrences x allDishes) (maxDishCount xs allDishes)


dishesWithCount :: [String] -> [String] -> Int -> [String]
dishesWithCount [] allDishes maxCount = []
dishesWithCount (x:xs) allDishes maxCount
    | countOccurrences x allDishes == maxCount =
        x : dishesWithCount xs allDishes maxCount
    | otherwise =
        dishesWithCount xs allDishes maxCount


mostPopularDish :: [String] -> [String]
mostPopularDish [] = []
mostPopularDish dishes =
    dishesWithCount uniqueDishes dishes highest
    where
        uniqueDishes = removeDuplicates dishes
        highest = maxDishCount uniqueDishes dishes
		
calculateTotalExpenses :: Expense -> Price
calculateTotalExpenses (Item name price date) = price
calculateTotalExpenses (Category name expenses) =
    foldr (+) 0 (map calculateTotalExpenses expenses)
	
countAllItems :: Expense -> Int
countAllItems (Item name price date) = 1
countAllItems (Category name expenses) =
    foldr (+) 0 (map countAllItems expenses)


countCategoryItems :: String -> Expense -> Int
countCategoryItems wantedCategory (Item name price date) = 0
countCategoryItems wantedCategory (Category categoryName expenses)
    | wantedCategory == categoryName = countAllItems (Category categoryName expenses)
    | otherwise = foldr (+) 0 (map (countCategoryItems wantedCategory) expenses)
