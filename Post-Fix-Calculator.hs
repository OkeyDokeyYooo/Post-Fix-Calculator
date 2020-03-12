-- Assignment 4: A Postfix Calculator in Haskell
-- Allen Huang 301280711

--data type of Token
data Token = Num Double | Op String | Error String
    deriving (Show, Eq)

--source code from : https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Haskell
--check the input string is numeric or not
isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False
   
isDouble s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False
   
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

isOp :: String -> Bool
isOp s = if    s == "inc" 
            || s == "dec" 
            || s == "sqrt" 
            || s == "sin" 
            || s == "cos" 
            || s == "inv" 
            || s == "+" 
            || s == "-" 
            || s == "/" 
            || s == "*"
            || s == "+all" 
            || s == "*all"
            || s == "dup"
            || s == "pop"
            || s == "clear"
            || s == "swap"
        then True
        else False 

--tokenlize the input string -> convert the string into [token]
tokenlize_string string = tokenlize (words string)

tokenlize :: [String] -> [Token]
tokenlize []                    = []
tokenlize lst
        | isNumeric first_token = Num (read first_token :: Double) : (tokenlize rest_token)
        | isOp first_token      = Op first_token : (tokenlize rest_token)
        | otherwise             = (Error "UnKnow Input") : (tokenlize rest_token) 
              where first_token = head lst
                    rest_token  = tail lst

--helper function : check the Token belong to which type
tokenToString :: Token -> String
tokenToString (Num x)   = show x
tokenToString (Op  x)   = id x
tokenToString (Error x) = id x

tokenToDouble :: Token -> Double
tokenToDouble (Num x)   = id x
tokenToDouble otherwise = error "Stack Error: Can not Calc this Value"

token_is_Num :: Token -> Bool
token_is_Num (Num _)   = True
token_is_Num otherwise = False

token_is_Op :: Token -> Bool
token_is_Op (Op _)    = True
token_is_Op otherwise = False

num_token_in_string :: String -> Int
num_token_in_string s = length(words s)

remove_two :: [Token] -> [Token]
remove_two lst = remove_one (remove_one lst)

remove_one :: [Token] -> [Token]
remove_one (x:xs) = xs

find_seond_last :: [Token] -> Int -> Token
find_seond_last lst index
    | index == 2   = head lst
    | otherwise    = find_seond_last (tail lst) (index-1)  


--Unary Operation
inc_op :: [Token] -> [Token]
inc_op lst 
    | length lst == 0  = (Error "Stack Error: Empty Stack") : lst
    | otherwise        = (Num result) : (remove_one lst)
        where result   = (tokenToDouble (head lst)) + 1.0

dec_op :: [Token] -> [Token]
dec_op lst
    | length lst == 0  = (Error "Stack Error: Empty Stack") : lst
    | otherwise        = (Num result) : (remove_one lst)
        where result   = (tokenToDouble (head lst)) - 1.0

sqrt_op :: [Token] -> [Token]
sqrt_op lst
    | length lst == 0  = (Error "Stack Error: Empty Stack") : lst
    | otherwise        = (Num result) : (remove_one lst)
        where result   = sqrt (tokenToDouble (head lst))

sin_op :: [Token] -> [Token]
sin_op lst
    | length lst == 0  = (Error "Stack Error: Empty Stack") : lst
    | otherwise        = (Num result) : (remove_one lst)
        where result   = sin (tokenToDouble (head lst))


cos_op :: [Token] -> [Token]
cos_op lst
    | length lst == 0  = (Error "Stack Error: Empty Stack") : lst
    | otherwise        = (Num result) : (remove_one lst)
        where result   = cos (tokenToDouble (head lst))

inv_op :: [Token] -> [Token]
inv_op lst
    | length lst == 0 = (Error "Stack Error: Empty Stack") : lst
    | element == 0.0  = (Error "Infinity") : (remove_one lst)
    | otherwise       = (Num (1/element))    : (remove_one lst)
        where element = (tokenToDouble (head lst))


--Binary Operation
add_op:: [Token] -> [Token]
add_op lst
    | len == 0       = (Error "+: not args") : lst
    | len == 1       = (Error "+: not enough args") : (remove_one lst)
    | otherwise      = (Num (top + bot)) : (remove_two lst)
        where top    = tokenToDouble (head lst)
              bot    = tokenToDouble  (head (tail lst)) 
              len    = length lst

minus_op:: [Token] -> [Token]
minus_op lst
    | len   == 0                    = (Error "-: not args") : lst
    | len   == 1                    = (Error "-: not enough args") : (remove_one lst)
    | otherwise                     = (Num (bot - top)) : (remove_two lst)
                        where top   = tokenToDouble (head lst)
                              bot   = tokenToDouble  (head (tail lst)) 
                              len   = length lst

times_op:: [Token] -> [Token]
times_op lst
    | len   == 0                  = (Error "*: not args") : lst
    | len   == 1                  = (Error "*: not enough args") : (remove_one lst)
    | otherwise                   = (Num (bot * top)) : (remove_two lst)
                    where   top   = tokenToDouble (head lst)
                            bot   = tokenToDouble  (head (tail lst)) 
                            len   = length lst

devid_op:: [Token] -> [Token]
devid_op lst
    | len   == 0                    = (Error "/: not args") : lst
    | len   == 1                    = (Error "/: not enough args") : (remove_one lst)
    | otherwise                     = (Num (bot / top)) : (remove_two lst)
                        where top   = tokenToDouble (head lst)
                              bot   = tokenToDouble  (head (tail lst)) 
                              len   = length lst

add_all_op :: [Token] -> [Token]
add_all_op lst
    | len == 0    = (Error "+all: empty stack") : lst
    | has_err lst = [find_err lst]
    | otherwise   = [Num (sum_stack lst)]
        where len = length lst

sum_stack :: [Token] -> Double
sum_stack lst
    | len == 1        =  element
    | otherwise       =  element + (sum_stack (tail lst))
        where element = (tokenToDouble (head lst))
              len     = length lst

times_all_op :: [Token] -> [Token]
times_all_op lst
    | length lst == 0 = (Error "*all: empty stack") : lst
    | has_err lst     = [find_err lst]
    | otherwise       = [Num (times_stack lst)]

times_stack :: [Token] -> Double
times_stack lst
    | length lst == 1  = element
    | otherwise        = element * (times_stack (tail lst))
         where element = (tokenToDouble (head lst))


--extra operation
dup_op :: [Token] -> [Token]
dup_op lst
    | length lst == 0 = (Error "Stack Error: Empty Stack") : lst
    | otherwise       = (head lst) : lst

pop_op :: [Token] -> [Token]
pop_op lst
    | length lst == 0                                = []
    | length lst == 1 && is_err (head lst) == False  = (Error "Empty Stack") : (remove_one lst)
    | is_err (head lst)                              = lst
    | otherwise                                      = remove_one lst

clear_op :: [Token] -> [Token]
clear_op lst 
    | length lst == 0               = []
    | length lst > 0 && has_err lst = clear_op_helper lst
    | otherwise                     = []


clear_op_helper :: [Token] -> [Token]
clear_op_helper lst 
    | length lst == 0   = []
    | is_err (head lst) = head lst : clear_op_helper (tail lst)
    | otherwise         = clear_op_helper (tail lst)


swap_op :: [Token] -> [Token]
swap_op lst
    | length lst == 1 && is_err (head lst)  = lst
    | length lst == 0                       = (Error "swap: Empty Stack") : lst
    | length lst == 1                       = (Error "swap: Only One Stack") : (remove_one lst)
    | is_err bot || is_err top              = lst
    | otherwise                             = bot : (top : (remove_two lst))
        where bot                           = head (tail lst)
              top                           = head lst

------------------------------------------------------------------------------

find_op :: Token -> [Token] -> [Token]
find_op (Op operation) stack
    | operation == "+"     = add_op stack
    | operation == "-"     = minus_op stack
    | operation == "*"     = times_op stack
    | operation == "/"     = devid_op stack
    | operation == "+all"  = add_all_op stack
    | operation == "*all"  = times_all_op stack
    | operation == "inc"   = inc_op stack
    | operation == "dec"   = dec_op stack
    | operation == "sqrt"  = sqrt_op stack
    | operation == "sin"   = sin_op stack
    | operation == "cos"   = cos_op stack
    | operation == "inv"   = inv_op stack
    | operation == "dup"   = dup_op stack
    | operation == "pop"   = pop_op stack
    | operation == "clear" = clear_op stack
    | operation == "swap"  = swap_op stack
    | otherwise            = (Error "UNKNOW OPERATION") : stack 
find_op otherwise stack    = (Error "NOT THE RIGHT FORMAT") : stack

is_err :: Token -> Bool
is_err (Error _) = True
is_err otherwise = False

has_err :: [Token] -> Bool
has_err [] = False
has_err lst
    | is_err (last lst) = True
    | otherwise         = has_err (take (len - 1) lst)
              where len = length lst

-- call this function , the input must has Error
find_err :: [Token] -> Token
find_err lst
    | is_err (last lst) = last lst
    | otherwise                          = find_err (take (len - 1) lst)
                               where len = length lst

-- return the top of the stack
find_ans :: [Token] -> Token
find_ans lst
    | length lst == 0                         = (Error "Empty Stack")
    | has_err lst == False && length lst >= 1 = head lst
    | otherwise                               = find_err lst 


start_calc :: [Token] -> [Token] -> [Token]
start_calc lst stack
    | length lst == 0     = stack
    | token_is_Num token  = start_calc (tail lst) (token : stack)
    | token_is_Op  token  = (start_calc (tail lst) (find_op token stack))
    | otherwise           = start_calc (tail lst) ((Error "UnKnow Input") : stack)
              where token = head lst

--Main Function
calc :: String -> String
calc s
    | num_token_in_string s == 0 = "Error: Empty Stack"
    | otherwise                  = tokenToString (find_ans (start_calc (tokenlize_string s) []))


calcStack :: String -> String
calcStack s
    | num_token_in_string s == 0 = "Empty String"
    | otherwise                  = calcStack_helper (tokenlize_string s)


calcStack_helper :: [Token] -> String
calcStack_helper lst
    | length lst == 0  = ""
    | length lst == 1  = tokenToTokenString (head lst) ++ calcStack_helper (tail lst)
    | otherwise        = tokenToTokenString (head lst) ++ " " ++ calcStack_helper (tail lst)

tokenToTokenString :: Token -> String
tokenToTokenString (Num x)   = "Number " ++ (show x)
tokenToTokenString (Op op)   = "Op "     ++ (id op)
tokenToTokenString otherwise = "Error UnKnow Input"