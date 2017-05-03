import FurnitureResources

--statsList
-----------------------------------------------------------------------------------------------------------------------------
getRight object [] _ = []
getRight object (x:xs) training = if((rightFreqHelper0 object x training)>0) then ((x,"right",(rightFreqHelper0 object x training)):(getRight object xs training))
								else (getRight object xs training)

--get frequency in all rooms
rightFreqHelper0 object o2 training = foldr (+) 0 (map (rightFreqHelper1 object o2)  training)

--get the frequency in one room
rightFreqHelper1 object o2 room = foldr (+) 0 (map (rightFreqHelper2 object o2)  room)

--get the frequency in one row
rightFreqHelper2 _ _ [x] = 0
rightFreqHelper2 object o2 (x:y:ys) = if (x==object && y==o2) then 1+ (rightFreqHelper2 object o2 (y:ys))
	                                 else rightFreqHelper2 object o2 (y:ys)

getBelow object [] _ = []
getBelow object (x:xs) training = if((belowFreqHelper0 object x training)>0) then ((x,"below",(belowFreqHelper0 object x training)):(getBelow object xs training))
								else (getBelow object xs training)

--get frequency in all rooms
belowFreqHelper0 object o2 training = foldr (+) 0 (map (belowFreqHelper1 object o2)  training)

--get the frequency in one room
belowFreqHelper1 _ _ [_] = 0 
belowFreqHelper1 object o2 (x:y:xs) = (belowFreqHelper2 object o2 x y) + (belowFreqHelper1 object o2 (y:xs))

--get the frequency in two rows
belowFreqHelper2 _ _ [] [] = 0
belowFreqHelper2 object o2 (x:xs) (y:ys) = if (x==object && y==o2) then 1+(belowFreqHelper2 object o2 xs ys) 
	            						else (belowFreqHelper2 object o2 xs ys )
insert1 x [] = [x]
insert1 x (y:ys) = if (fx>fy) then [x]++(y:ys) else y:(insert1 x ys) where ((_,_,fx),(_,_,fy))=(x,y)

sortByFreq []=[]
sortByFreq [x]=[x]
sortByFreq (x:xs) = insert1 x (sortByFreq xs)

statsListHelper [] _=[]
statsListHelper (x:xs) training1=((x,[sortByFreq(getRight x furniture2 training1),sortByFreq(getBelow x furniture2 training1)]):(statsListHelper xs training1))

statsList = statsListHelper furniture2 training

--findFurnitureUpdate
--------------------------------------------------------------
memberR x [] = False
memberR (a,[[(b,c,f1)],[]]) ((a1,[[],_]):xs)=if(a==a1) then False
													else (memberR (a,[[(b,c,f1)],[]]) xs)
memberR (a,[[(b,c,f1)],[]]) ((a1,[((b1,c1,f2):m),x]):xs)= if (a1==a)
														then if (b1==b && c==c1) 
															then True 
															else if (m==[])
																then False
																else(memberR (a,[[(b,c,f1)],[]]) ((a1,[m,x]):xs))
														else (memberR (a,[[(b,c,f1)],[]]) xs)
memberB x [] = False
memberB (a,[[],[(b,c,f1)]]) ((a1,[_,[]]):xs)=if(a==a1) then False
													else (memberB (a,[[],[(b,c,f1)]]) xs)
memberB (a,[[],[(b,c,f1)]]) ((a1,[x,((b1,c1,f2):m)]):xs)= if (a1==a)
														then if (b1==b && c==c1) 
															then True 
															else if (m==[])
																then False
																else (memberB (a,[[],[(b,c,f1)]]) ((a1,[x,m]):xs))
														else (memberB (a,[[],[(b,c,f1)]]) xs)
							
replaceRight (a,[[(b,"right",f1)],[]]) ((a1,[m,x]):xs)= 
		if (a1/=a)then ((a1,[m,x]):(replaceRight (a,[[(b,"right",f1)],[]]) xs))
			else (((replaceRightH (a,[[(b,"right",f1)],[]]) [((a1,[m,x]))])) ++xs)
																							
replaceRightH (a,[[(b,"right",f1)],[]]) ((a1,[((b1,"right",f2):m),x]):xs)=if (b1==b) 
																		then ((a,[((b,"right",(f1+f2)):m),x]):xs)
																		else [(a1,[((b1,"right",f2):newList),x])]
																		where [(a11,[newList,x1])]=(replaceRightH (a,[[(b,"right",f1)],[]]) ((a1,[m,x]):xs)) 
																			
																		--else []
																		--((a1,[((b1,"right",f2):m),x]):(replaceRight (a,[[(b,"right",f1)],[]]) xs))
																		
replaceBelow (a,[[],[(b,"below",f1)]]) ((a1,[x,m]):xs)= if (a1/=a)then ((a1,[x,m]):(replaceBelow (a,[[],[(b,"below",f1)]]) xs))
																else (replaceBelowH (a,[[],[(b,"below",f1)]]) ((a1,[x,m]):xs))
																		
replaceBelowH (a,[[],[(b,"below",f1)]]) ((a1,[x,((b1,"below",f2):m)]):xs)=if (b1==b) 
																		then ((a,[x,((b,"below",(f1+f2)):m)]):xs)
																		else [(a1,[x,((b1,"below",f2):newList)])]
																		where [(a11,[x,newList])]=(replaceBelowH(a,[[],[(b,"below",f1)]]) ((a1,[x,m]):xs))
insertRightHelper x []=[x]																							
insertRightHelper (a,[[m],[]]) ((a1,[list,x]):xs) = if(a==a1) 
													then ((a,[(insert1 m list),x]):xs)
													else (a1,[list,x]):(insertRightHelper (a,[[m],[]]) xs)
insertBelowHelper x []=[x]
insertBelowHelper (a,[[],[m]]) ((a1,[x,list]):xs) = if(a==a1) 
													then ((a,[x,(insert1 m list)]):xs)
													else (a1,[x,list]):(insertBelowHelper (a,[[],[m]]) xs)

insertRight x []=[x]											
insertRight x list=if(memberR x list) then (replaceRight x list)
									else (insertRightHelper x list)
insertBelow x []=[x]				
insertBelow x list=if(memberB x list) then (replaceBelow x list)
									else (insertBelowHelper x list)

findFurnitureUpdate a b c currentState = 
	if(c=="right")
	then (insertRight (a,[[(b,c,1)],[]]) currentState)
	else (insertBelow (a,[[],[(b,c,1)]]) currentState)


--generate
-------------------------------------------------------------------------------------------------------------------
generateHelper1 room = statsListHelper furniture2 [room]

generateHelper2 [] currentState=currentState
generateHelper2 ((a,[rights,belows]):xs) currentState= generateHelper2 xs (generateHelper3 a rights (generateHelper3 a belows currentState))

findFurnitureUpdateWithFreq a b c f currentState = 
	if(c=="right")
	then (insertRight (a,[[(b,c,f)],[]]) currentState)
	else (insertBelow (a,[[],[(b,c,f)]]) currentState)

generateHelper3 _ [] currentState=currentState
generateHelper3 a ((b,c,f):xs) currentState= generateHelper3 a xs (findFurnitureUpdateWithFreq a b c f currentState)

generate room currentState= generateHelper2 (generateHelper1 room) currentState

--furnishRoom
---------------------------------------------------------------------------------------------
generateRow _ 0 _ _=[]
generateRow row column object roomSoFar=(object:(generateRow row (column-1) (getPossibleNeighbour rights (getBelowsOfTheUpper (row+1) (column) roomSoFar)) roomSoFar))
										where [rights,_]=(getFurnStat object)
										
furnishRoom3 row column object roomSoFar=(roomSoFar++[generateRow row column object roomSoFar])

--columns j
furnishRoom4 0 _ _ roomSoFar=roomSoFar
furnishRoom4 row column object roomSoFar=furnishRoom4 (row-1) column (getPossibleNeighbour [] belows) (furnishRoom3 row column object roomSoFar)
					where [_,belows]=(getFurnStat object)
				
getCorrectColumn _ [] = []				
getCorrectColumn 0 (x:xs)=x				
getCorrectColumn j (x:xs)=if(j<0)then [] else getCorrectColumn (j-1) xs

getCorrectRow _ [] = []				
getCorrectRow 0 (x:xs)=x
getCorrectRow i (x:xs)=if(i<0)then [] else getCorrectRow (i-1) xs

getCell i j roomSoFar=getCorrectRow i (getCorrectColumn j roomSoFar)

getBelowsOfTheUpper _ _ []=[]
getBelowsOfTheUpper i j roomSoFar=belows 
							where [_,belows]=getFurnStat (getCell i j roomSoFar)
					
furnishRoom n object = furnishRoom4 n n object []

--getFurnStat
---------------------------------------------------------------------------------------
getFurnStat object =[sortByFreq(getRight object furniture2 training),sortByFreq(getBelow object furniture2 training)]

--getPossibleNeighbor
----------------------------------------------------------------------------------------
merge [] [] = []
merge [] (x:xs) = [x]++merge [] xs
merge (x:xs) list2 = [x]++merge xs list2  

sumOfFreq [] = 0
sumOfFreq ((_,_,f):xs) = f+sumOfFreq xs

getFreq _ [] = 0
getFreq x ((y,r,f):xs) = if(x==y) then f+(getFreq x xs) else (getFreq x xs)

getFreqHelp [] l = []
getFreqHelp ((y,r,f):xs) l= if (member (y,r,f) xs) then getFreqHelp xs l else ((y,r,(getFreq y l)):(getFreqHelp xs l))

listOfObjects x 0= []
listOfObjects x n= x:listOfObjects x (n-1)

method [] = []
method ((y,r,f):xs) = listOfObjects y (getFreq y ((y,r,f):xs))++(method xs)

member _ [] = False
member (y,r,f) ((x,r1,f1):xs) = if (y==x) then True else member (y,r,f) xs

randomObject 0 (x:xs) = x
randomObject n (x:xs) = randomObject (n-1) xs

getPossibleNeighbour rights belows = randomObject (randomZeroToX (sumOfFreq (merge rights belows)-1)) (method (getFreqHelp (merge rights belows) (merge rights belows)))
