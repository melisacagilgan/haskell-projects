--Question #1
calculate num1 op num2 = if (op == '+' || op == 's') then num1+num2
                         else if (op == '-' || op == 'r') then num1-num2
                         else if (op == '*' || op == 'm') then num1*num2
                         else if (op == '/' || op == 'd') then num1/num2
                         else if (op == 'x') then product [(min num1 num2)..(max num1 num2)]
                         else if (op == '.') then sum [(min num1 num2)..(max num1 num2)]
                         else 0
--Question #2
evaluate [] _ = 0
evaluate [x] _ = x
evaluate (x:xs) op = calculate x op (evaluate xs op)
--Question #3
polynomial [] _ _ = 0
polynomial [x] _ _ = x
polynomial (x:xs) num op = calculate (x*(num^(length xs))) op (polynomial xs num op)
--Bonus
bonus [] _ _ = 0
bonus [x] _ _ = x
bonus (x:xs) _  [] = 0 --OR bonus _ _ [] because the first base case covered the case that both of them are empty
bonus (x:xs) num (y:ys) = calculate (x*(num^(length xs))) y (bonus xs num ys)

{--Alternative Question #1
trailingProduct 0 = 1
trailingProduct n = n * trailingProduct (n-1)

trailingSum 0 = 0
trailingSum n = n + trailingSum (n-1)

calculate num1 op num2 = if (op == '+' || op == 's') then num1+num2
                         else if (op == '-' || op == 'r') then num1-num2
                         else if (op == '*' || op == 'm') then num1*num2
                         else if (op == '/' || op == 'd') then num1/num2
                         else if (op == 'x' && num1>num2) then (trailingProduct num1)/(trailingProduct num2)*num2
                         else if (op == 'x' && num2>num1) then (trailingProduct num2)/(trailingProduct num1)*num1
                         else if (op == '.' && num1>num2) then (trailingSum num1)-(trailingSum num2)+num2
                         else if (op == '.' && num2>num1) then (trailingSum num2)-(trailingSum num1)+num1
                         else if ((op == 'x' || op == '.') && num1 == num2) then num1
                         else 0--}