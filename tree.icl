module tree

import StdEnv, StdLib, GenEq

:: T23 a = Empty
         | N2 (T23 a) a (T23 a)
         | N3 (T23 a) a (T23 a) a (T23 a)
         | NERR
         
:: RT23 a = REmpty
          | RN2 (RT23 a) a (RT23 a)
          | RN3 (RT23 a) a (RT23 a) a (RT23 a)
          | RN4 (RT23 a) a (RT23 a) a (RT23 a) a (RT23 a)
          | RNERR
          
:: KeyVal k v = KV k v 

derive gEq T23
derive gEq RT23
derive gEq Maybe
derive gEq KeyVal
       
Start = (and (flatten allTests), allTests)
	where
		allTests = 
			[ test_t23_to_rt23
			, test_rt23_to_t23
			, test_t23_empty
			, test_t23_lookup
			, test_rt23_propagateSplit
			, test_rt23_insert
			, test_t23_insert
			, test_fold
		//	, test_toList
			]
//1
EmptyInt :: T23 Int
EmptyInt = Empty

REmptyInt :: RT23 Int
REmptyInt = REmpty

t23_to_rt23 :: (T23 a) -> RT23 a
t23_to_rt23 (N2 Empty i Empty) = RN2 REmpty i REmpty
t23_to_rt23 (N3 Empty i Empty j Empty) = RN3 REmpty i REmpty j REmpty
t23_to_rt23 (N2 a i b) = RN2 (t23_to_rt23 a) i (t23_to_rt23 b)
t23_to_rt23 (N3 a i b j c) = RN3 (t23_to_rt23 a) i (t23_to_rt23 b) j (t23_to_rt23 c)
t23_to_rt23 a = REmpty
//1_test
test_t23_to_rt23 :: [Bool]
test_t23_to_rt23 = 
	[ t23_to_rt23 EmptyInt === REmptyInt
	, t23_to_rt23 (N2 Empty 10 Empty) === RN2 REmpty 10 REmpty
	, t23_to_rt23 (N3 Empty 10 Empty 20 Empty) === RN3 REmpty 10 REmpty 20 REmpty
	, t23_to_rt23 (N2 (N3 (N2 Empty 14 Empty) 15 (N2 Empty 25 Empty) 35 (N3 Empty 40 Empty 48 Empty)) 51 (N3 (N2 Empty 54 Empty) 55 (N2 Empty 59 Empty) 60 (N2 Empty 75 Empty)))
  		===
	  RN2 (RN3 (RN2 REmpty 14 REmpty) 15 (RN2 REmpty 25 REmpty) 35 (RN3 REmpty 40 REmpty 48 REmpty)) 51 (RN3 (RN2 REmpty 54 REmpty) 55 (RN2 REmpty 59 REmpty) 60 (RN2 REmpty 75 REmpty))
	]
	
	  
//2
/*
	rt23_to_t23_rn4_err_rec is recursively called to each node, and it will returns: (itself, True is there is RN4 
		somewhere below it / False otherwise)
	
	rt23_to_t23_rn4_err_main will return the original tree, if there is no RN4 in it, and it will return RNERR 
		otherwise	
	
	rt23_to_t23_converter converts the tree to the given form, but if there is RN4 in the tree, it will return NERR
	
	rt23_to_t23_helper and rt23_to_t23_helper2 will decide based upon the results above, if we can actually do the
		conversion

*/
rt23_to_t23 :: (RT23 a) -> Maybe (T23 a)
rt23_to_t23 a = rt23_to_t23_helper2 (rt23_to_t23_helper a)

rt23_to_t23_helper2 :: (T23 a) -> Maybe (T23 a)
rt23_to_t23_helper2 NERR = Nothing
rt23_to_t23_helper2 a = Just a

rt23_to_t23_helper :: (RT23 a) -> T23 a
rt23_to_t23_helper a = rt23_to_t23_converter (rt23_to_t23_rn4_err_main (rt23_to_t23_rn4_err_rec a))

rt23_to_t23_converter :: (RT23 a) -> T23 a
rt23_to_t23_converter RNERR = NERR
rt23_to_t23_converter (RN2 REmpty i REmpty) = N2 Empty i Empty
rt23_to_t23_converter (RN3 REmpty i REmpty j REmpty) = N3 Empty i Empty j Empty
rt23_to_t23_converter (RN2 a i b) = N2 (rt23_to_t23_converter a) i (rt23_to_t23_converter b)
rt23_to_t23_converter (RN3 a i b j c) = N3 (rt23_to_t23_converter a) i (rt23_to_t23_converter b) j (rt23_to_t23_converter c) 
rt23_to_t23_converter a = Empty

rt23_to_t23_rn4_err_main :: (RT23 a, Bool) -> RT23 a
rt23_to_t23_rn4_err_main s
	| snd(s) == True = RNERR
	| otherwise = fst(s)

rt23_to_t23_rn4_err_rec :: (RT23 a) -> (RT23 a, Bool)
rt23_to_t23_rn4_err_rec REmpty = (REmpty, False)
rt23_to_t23_rn4_err_rec RNERR = (RNERR, True)
rt23_to_t23_rn4_err_rec (RN4 a i b j c k d) = (RNERR, True)
rt23_to_t23_rn4_err_rec (RN2 RNERR i b) = (RNERR, True)
rt23_to_t23_rn4_err_rec (RN2 a i RNERR) = (RNERR, True)
rt23_to_t23_rn4_err_rec (RN3 RNERR i b j c) = (RNERR, True)
rt23_to_t23_rn4_err_rec (RN3 a i RNERR j c) = (RNERR, True)
rt23_to_t23_rn4_err_rec (RN3 a i b j RNERR) = (RNERR, True)
rt23_to_t23_rn4_err_rec (RN2 REmpty i REmpty) = (RN2 REmpty i REmpty, False)
rt23_to_t23_rn4_err_rec (RN3 REmpty i REmpty j REmpty) = (RN3 REmpty i REmpty j REmpty, False)
rt23_to_t23_rn4_err_rec (RN2 a i b) = (RN2 a i b, snd(rt23_to_t23_rn4_err_rec a) || snd(rt23_to_t23_rn4_err_rec b) )
rt23_to_t23_rn4_err_rec (RN3 a i b j c) = (RN3 a i b j c, snd(rt23_to_t23_rn4_err_rec a) || snd(rt23_to_t23_rn4_err_rec b) || snd(rt23_to_t23_rn4_err_rec c))
//2_test
test_rt23_to_t23 :: [Bool]
test_rt23_to_t23 = 
	[ rt23_to_t23 REmptyInt === Just EmptyInt
	, rt23_to_t23 (RN2 REmpty 10 REmpty) === Just (N2 Empty 10 Empty)
	, rt23_to_t23 (RN3 REmpty 10 REmpty 20 REmpty) === Just (N3 Empty 10 Empty 20 Empty)
	, rt23_to_t23 (RN2 (RN3 (RN2 REmpty 14 REmpty) 15 (RN2 REmpty 25 REmpty) 35 (RN3 REmpty 40 REmpty 48 REmpty)) 51 (RN3 (RN2 REmpty 54 REmpty) 55 (RN2 REmpty 59 REmpty) 60 (RN2 REmpty 75 REmpty)))
  		===
	  Just (N2 (N3 (N2 Empty 14 Empty) 15 (N2 Empty 25 Empty) 35 (N3 Empty 40 Empty 48 Empty)) 51 (N3 (N2 Empty 54 Empty) 55 (N2 Empty 59 Empty) 60 (N2 Empty 75 Empty)))
	, rt23_to_t23 (RN2 (RN2 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty)) 30 (RN2 (RN4 REmpty 35 REmpty 36 REmpty 37 REmpty) 40 (RN2 REmpty 45 REmpty)))
  		===
	  Nothing
	]
		
	
//3
EmptyString :: T23 String
EmptyString = Empty

t23_empty :: T23 a
t23_empty = Empty
//3_test
test_t23_empty :: [Bool]
test_t23_empty = 
	[ t23_empty === EmptyInt
	, t23_empty === EmptyString
	]


//4
t23_lookup :: a (T23 a) -> Maybe a | Eq a & Ord a
t23_lookup x Empty = Nothing
t23_lookup x (N2 a i b)
	| i == x = Just x
	| x < i  
		| t23_lookup x a == Just x = Just x
		| otherwise = Nothing
	| otherwise
		| t23_lookup x b == Just x = Just x
		| otherwise = Nothing
t23_lookup x (N3 a i b j c)
	| i == x = Just x
	| x < i 
		| t23_lookup x a == Just x = Just x
		| otherwise = Nothing
	| j == x = Just x
	| x < j
		| t23_lookup x b == Just x = Just x
		| otherwise = Nothing
	| otherwise
		| t23_lookup x c == Just x = Just x
		| otherwise = Nothing
//4_test
test_t23_lookup :: [Bool]
test_t23_lookup =
	[ t23_lookup 1 Empty == Nothing
	, t23_lookup 14 (N2 (N3 (N2 Empty 14 Empty) 15 (N2 Empty 25 Empty) 35 (N3 Empty 40 Empty 48 Empty)) 51 (N3 (N2 Empty 54 Empty) 55 (N2 Empty 59 Empty) 60 (N2 Empty 75 Empty)))
  		== 
  	  Just 14
	, t23_lookup 1 (N2 (N3 (N2 Empty 14 Empty) 15 (N2 Empty 25 Empty) 35 (N3 Empty 40 Empty 48 Empty)) 51 (N3 (N2 Empty 54 Empty) 55 (N2 Empty 59 Empty) 60 (N2 Empty 75 Empty)))
  		==
  	  Nothing
  	]


//5
rt23_propagateSplit :: (RT23 a) -> RT23 a
rt23_propagateSplit a
	| rt23_RN4_exist (rt23_to_t23_rn4_err_main (rt23_to_t23_rn4_err_rec a)) = rt23_propagateSplit_splitter(a)
	| otherwise = a

rt23_RN4_exist :: (RT23 a) -> Bool
rt23_RN4_exist RNERR = True
rt23_RN4_exist a = False

rt23_propagateSplit_splitter :: (RT23 a) -> RT23 a
rt23_propagateSplit_splitter (RN4 a i b j c k d) = RN2 (RN2 a i b) j (RN2 c k d)
rt23_propagateSplit_splitter (RN2 (RN4 a i b j c k d) m e) = RN3 (RN2 a i b) j (RN2 c k d) m e
rt23_propagateSplit_splitter (RN2 a i (RN4 b j c k d m e)) = RN3 a i (RN2 b j c) k (RN2 d m e)
rt23_propagateSplit_splitter (RN3 (RN4 a i b j c k d) m e n f) = RN4 (RN2 a i b) j (RN2 c k d) m e n f
rt23_propagateSplit_splitter (RN3 a i (RN4 b j c k d m e) n f) = RN4 a i (RN2 b j c) k (RN2 d m e) n f
rt23_propagateSplit_splitter (RN3 a i b j (RN4 c k d m e n f)) = RN4 a i b j (RN2 c m d) k (RN2 e n f)

//5_test
test_rt23_propagateSplit :: [Bool]
test_rt23_propagateSplit = 
	[ rt23_propagateSplit (RN2 (RN2 REmpty 5 REmpty) 10 (RN4 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty) 30 (RN2 REmpty 35 REmpty) 40 (RN2 REmpty 45 REmpty)))
  		===
  	  RN3 (RN2 REmpty 5 REmpty) 10 (RN2 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty)) 30 (RN2 (RN2 REmpty 35 REmpty) 40 (RN2 REmpty 45 REmpty))
	, rt23_propagateSplit (RN3 (RN2 REmpty 5 REmpty) 10 (RN4 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty) 30 (RN2 REmpty 35 REmpty) 40 (RN2 REmpty 45 REmpty)) 50 (RN2 REmpty 55 REmpty))
  		===
  	  RN4 (RN2 REmpty 5 REmpty) 10 (RN2 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty)) 30 (RN2 (RN2 REmpty 35 REmpty) 40 (RN2 REmpty 45 REmpty)) 50 (RN2 REmpty 55 REmpty)
	, rt23_propagateSplit (RN4 (RN2 REmpty 5 REmpty) 10 (RN2 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty)) 30 (RN2 (RN2 REmpty 35 REmpty) 40 (RN2 REmpty 45 REmpty)) 50 (RN2 REmpty 55 REmpty))
  		===
  	  RN2 (RN2 (RN2 REmpty 5 REmpty) 10 (RN2 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty))) 30 (RN2 (RN2 (RN2 REmpty 35 REmpty) 40 (RN2 REmpty 45 REmpty)) 50 (RN2 REmpty 55 REmpty))
	, rt23_propagateSplit (RN2 (RN2 (RN2 REmpty 5 REmpty) 10 (RN2 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty))) 30 (RN2 (RN2 (RN2 REmpty 35 REmpty) 40 (RN2 REmpty 45 REmpty)) 50 (RN2 REmpty 55 REmpty)))
  		===
  	  RN2 (RN2 (RN2 REmpty 5 REmpty) 10 (RN2 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty))) 30 (RN2 (RN2 (RN2 REmpty 35 REmpty) 40 (RN2 REmpty 45 REmpty)) 50 (RN2 REmpty 55 REmpty))
	]


//6
rt23_insert :: a (RT23 a) -> Maybe (RT23 a) | Eq a & Ord a
rt23_insert x REmpty = Just (RN2 REmpty x REmpty)
rt23_insert x a
	| rt23_RN4_exist (rt23_to_t23_rn4_err_main (rt23_to_t23_rn4_err_rec a)) = Nothing
	| rt23_node_already_in x a = Just a
	| otherwise = Just (rt23_normalize (rt23_inserter x a))
	
rt23_node_already_in :: a (RT23 a) -> Bool | Eq a & Ord a
rt23_node_already_in x REmpty = False
rt23_node_already_in x (RN2 a i b) 
	| x == i = True
	| x < i = rt23_node_already_in x a
	| x > i = rt23_node_already_in x b
rt23_node_already_in x (RN3 a i b j c)
	| x == i = True
	| x < i = rt23_node_already_in x a
	| x == j = True
	| x < j = rt23_node_already_in x b
	| x > j = rt23_node_already_in x c
	
rt23_normalize :: (RT23 a) -> RT23 a | Eq a & Ord a
rt23_normalize a
	| rt23_RN4_exist (rt23_to_t23_rn4_err_main (rt23_to_t23_rn4_err_rec a)) = rt23_normalize (rt23_propagateSplit_splitter(a))
	| otherwise = a
	
rt23_inserter :: a (RT23 a) -> RT23 a | Eq a & Ord a
rt23_inserter x (RN2 REmpty i REmpty)
	| x < i = RN3 REmpty x REmpty i REmpty
	| otherwise = RN3 REmpty i REmpty x REmpty
rt23_inserter x (RN3 REmpty i REmpty j REmpty)
	| x < i = RN4 REmpty x REmpty i REmpty j REmpty
	| x < j = RN4 REmpty i REmpty x REmpty j REmpty
	| otherwise = RN4 REmpty i REmpty j REmpty x REmpty
rt23_inserter x (RN2 a i b)
	| x < i = RN2 (rt23_inserter x a) i b
	| otherwise = RN2 a i (rt23_inserter x b)
rt23_inserter x (RN3 a i b j c)
	| x < i = RN3 (rt23_inserter x a) i b j c
	| x < j = RN3 a i (rt23_inserter x b) j c
	| otherwise = RN3 a i b j (rt23_inserter x c)

//6_test
test_rt23_insert :: [Bool]
test_rt23_insert =
	[ rt23_insert 10 REmpty === Just (RN2 REmpty 10 REmpty)
	, rt23_insert 5 (RN2 (RN3 REmpty 10 REmpty 20 REmpty) 30 (RN3 REmpty 40 REmpty 50 REmpty))
  		===
  	  Just (RN3 (RN2 REmpty 5 REmpty) 10 (RN2 REmpty 20 REmpty) 30 (RN3 REmpty 40 REmpty 50 REmpty))
	, rt23_insert 15 (RN2 (RN3 REmpty 10 REmpty 20 REmpty) 30 (RN3 REmpty 40 REmpty 50 REmpty))
  		===
  	  Just (RN3 (RN2 REmpty 10 REmpty) 15 (RN2 REmpty 20 REmpty) 30 (RN3 REmpty 40 REmpty 50 REmpty))
	, rt23_insert 45 (RN3 (RN3 REmpty 10 REmpty 20 REmpty) 30 (RN3 REmpty 40 REmpty 50 REmpty) 60 (RN3 REmpty 70 REmpty 80 REmpty))
  		===
  	  Just (RN2 (RN2 (RN3 REmpty 10 REmpty 20 REmpty) 30 (RN2 REmpty 40 REmpty)) 45 (RN2 (RN2 REmpty 50 REmpty) 60 (RN3 REmpty 70 REmpty 80 REmpty)))
	, rt23_insert 40 (RN4 (RN2 REmpty 15 REmpty) 20 (RN2 REmpty 25 REmpty) 30 (RN2 REmpty 35 REmpty) 40 (RN2 REmpty 45 REmpty))
  		===
  	  Nothing
	, rt23_insert 10 (RN2 (RN4 REmpty 1 REmpty 2 REmpty 3 REmpty) 4 ((RN4 REmpty 5 REmpty 6 REmpty 7 REmpty)))
  		===
  	  Nothing
	, rt23_insert 30 (RN2 (RN3 REmpty 10 REmpty 20 REmpty) 30 (RN3 REmpty 40 REmpty 50 REmpty))
  		===
  	  Just (RN2 (RN3 REmpty 10 REmpty 20 REmpty) 30 (RN3 REmpty 40 REmpty 50 REmpty))
	]


//7
t23_insert :: a (T23 a) -> T23 a | Eq a & Ord a
t23_insert x Empty = N2 Empty x Empty
t23_insert x a = fromJust(   rt23_to_t23(  fromJust( rt23_insert x (t23_to_rt23 a) )  )    )

//7_test
test_t23_insert :: [Bool]
test_t23_insert =
	[ t23_insert 10 Empty === N2 Empty 10 Empty
	, t23_insert 5 (N2 (N3 Empty 10 Empty 20 Empty) 30 (N3 Empty 40 Empty 50 Empty))
  		===
  	  N3 (N2 Empty 5 Empty) 10 (N2 Empty 20 Empty) 30 (N3 Empty 40 Empty 50 Empty)
	, t23_insert 15 (N2 (N3 Empty 10 Empty 20 Empty) 30 (N3 Empty 40 Empty 50 Empty))
  		=== 
  	  N3 (N2 Empty 10 Empty) 15 (N2 Empty 20 Empty) 30 (N3 Empty 40 Empty 50 Empty)
	, t23_insert 45 (N3 (N3 Empty 10 Empty 20 Empty) 30 (N3 Empty 40 Empty 50 Empty) 60 (N3 Empty 70 Empty 80 Empty))
  		=== 
  	  N2 (N2 (N3 Empty 10 Empty 20 Empty) 30 (N2 Empty 40 Empty)) 45 (N2 (N2 Empty 50 Empty) 60 (N3 Empty 70 Empty 80 Empty))
	, t23_insert 30 (N2 (N3 Empty 10 Empty 20 Empty) 30 (N3 Empty 40 Empty 50 Empty))
 		=== 
 	  N2 (N3 Empty 10 Empty 20 Empty) 30 (N3 Empty 40 Empty 50 Empty)
	]


//8
class Foldable t
where
	fold :: (a b->b) b (t a) -> b
	
instance Foldable []
where
	fold :: (a b->b) b [a] -> b
	fold op r list = foldr op r list

instance Foldable T23
where
	fold :: (a b->b) b (T23 a) -> b
	fold op r Empty = r
	fold op r (N2 a i b) = fold op (op i (fold op r b)) a
	fold op r (N3 a i b j c) = fold op (op i (fold op (op j (fold op r c)) b)) a

//8_test
test_fold :: [Bool]
test_fold = 
	[ fold (+) 12 Empty == 12
	, fold (+) 12 (N2 Empty 22 Empty) == 34
	, fold (+) 0 (N2 (N3 Empty 10 Empty 20 Empty) 30 (N3 Empty 40 Empty 50 Empty)) == 150
	, fold (\x len -> len + 1) 0 (N2 (N3 Empty 10 Empty 20 Empty) 30 (N3 Empty 40 Empty 50 Empty)) == 5
	]


//9
//toList :: (t a) -> [a] | Foldable t
/*toList a = fold push_back [] a

class Appender a b
where
	push_back :: [Int] a -> b
	
instance Appender Int [Int]
where
	push_back :: [Int] Int -> [Int]
	push_back list a = [a:list]
	
instance Appender [Int] [Int]
where
	push_back :: [Int] [Int] -> [Int]
	push_back list1 list2 = list1 ++ list2
	*/
	
	
	
//9_test
/*test_toList :: [Bool]
test_toList = 
	[ toList (N2 (N2 (N2 (N2 Empty 14 Empty) 15 (N2 Empty 25 Empty)) 35 (N2 (N2 Empty 40 Empty) 48 (N2 Empty 49 Empty))) 51 (N2 (N2 (N2 Empty 54 Empty) 55 (N2 Empty 59 Empty)) 60 (N2 (N2 Empty 75 Empty) 80 (N2 Empty 82 Empty))))
  		==
  	  [14,15,25,35,40,48,49,51,54,55,59,60,75,80,82]
	, toList [14,15,25,35,40,48,49,51,54,55,59,60,75,80,82]
  		==
  	  [14,15,25,35,40,48,49,51,54,55,59,60,75,80,82]
  	]*/



//10
/*instance == (KeyVal k v)
where
//	(==) :: (KeyVal k v) (KeyVal k v) -> Bool
	(==) (KV k1 v1) (KV k2 v2) = k1 == k2
	*/
/*instance < KeyVal
where
	(<) :: (KeyVal k v) (KeyVal k v) -> Bool
	(<) (KV k1 v1) (KV k2 v2) = (k1 < k2)
	*/
//10_test

//11
generic gJSON a :: a -> String
gJSON {|Char|} x = (toString '{') +++ (toString '"') +++ "type" +++ (toString '"') +++ (toString ':') +++ (toString '"') +++ "char" +++ (toString '"') +++ (toString ',') +++ (toString '"') +++ "value" +++ (toString '"') +++ (toString ':') +++ (toString x) +++ (toString '}');
gJSON {|Bool|} x = (toString '{') +++ (toString '"') +++ "type" +++ (toString '"') +++ (toString ':') +++ (toString '"') +++ "bool" +++ (toString '"') +++ (toString ',') +++ (toString '"') +++ "value" +++ (toString '"') +++ (toString ':') +++ (toString x) +++ (toString '}');
gJSON {|String|} x = (toString '{') +++ (toString '"') +++ "type" +++ (toString '"') +++ (toString ':') +++ (toString '"') +++ "string" +++ (toString '"') +++ (toString ',') +++ (toString '"') +++ "value" +++ (toString '"') +++ (toString ':') +++ x +++ (toString '}');
gJSON {|Real|} x = (toString '{') +++ (toString '"') +++ "type" +++ (toString '"') +++ (toString ':') +++ (toString '"') +++ "real" +++ (toString '"') +++ (toString ',') +++ (toString '"') +++ "value" +++ (toString '"') +++ (toString ':') +++ (toString x) +++ (toString '}');
gJSON {|Int|} x = (toString '{') +++ (toString '"') +++ "type" +++ (toString '"') +++ (toString ':') +++ (toString '"') +++ "int" +++ (toString '"') +++ (toString ',') +++ (toString '"') +++ "value" +++ (toString '"') +++ (toString ':') +++ (toString x) +++ (toString '}');
gJSON {|UNIT|} x = ""
gJSON {|CONS of c|} fx (CONS x) = (toString '{') +++ (toString '"') +++ "constructor" +++ (toString '"') +++ (toString ':') +++ (toString '"') +++ c.gcd_name +++ (toString '"') +++ (toString ',') +++ (toString '"') +++ "params" +++ (toString '"') +++ (toString ':') +++ (toString '[') +++ fx x +++ (toString ']') +++ (toString '}');
gJSON {|EITHER|} xl xr (LEFT x) = xl x 
gJSON {|EITHER|} xl xr (RIGHT x) = xr x
gJSON {|PAIR|} xf xs (PAIR x y) = xf x +++ (toString ',') +++ xs y
gJSON {|OBJECT of o|} fx (OBJECT x) = (toString '{') +++ (toString '"') +++ "type" +++ (toString '"') +++ (toString ':') +++ (toString '"') +++ o.gtd_name +++ (toString '"') +++ (toString ',') +++ (toString '"') +++ "value" +++ (toString '"') +++ (toString ':') +++ fx x +++ (toString '}');

derive gJSON KeyVal
derive gJSON T23
//11_test

//12
EmptyChar :: T23 Char
EmptyChar = Empty

toJSON :: a -> String | gJSON{|*|} a
toJSON a = ""
//12_test































