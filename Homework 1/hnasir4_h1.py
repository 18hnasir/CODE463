
print("\033c")

#Function that returns the prime factors for a number n
def prime_factors(n):
    pFactors = [] #contains all prime factors of n
    currentNum = n
    currentPrime = 2 #acts like our index

    #First check if n is 1
    if (n == 1):
        return []
    
    #Check if n is a prime number
    if (is_prime(n) == True):
        pFactors.append(n)
        return pFactors

    while currentPrime < n:
        if (is_prime(currentPrime) == False): #increment currentPrime until its a prime num
            currentPrime += 1
            continue
        if (currentNum % currentPrime == 0): #no decimal values
            pFactors.append(currentPrime)
            if (is_prime(currentNum // currentPrime) == True): #if the division result equals a prime then finish
                pFactors.append(currentNum // currentPrime)
                break
            else:
                currentNum = currentNum // currentPrime
        else:
            currentPrime += 1
        
    return pFactors

#Function to see if two numbers are coprime
def coprime(a, b):
    result = True
    aFactors = all_factors(a)
    bFactors = all_factors(b)

    #go thru each number's factors and see if they share a
    #factor with each other
    for i in aFactors:
        if (i == 1): 
            continue
        for k in bFactors:
            if (i == k):
                result = False
                break

    return result

#Function returns the nth tribbonaci number
def trib(n):
    currentSum = 0
    bottomThree = 1 #keeps track of the last number within the 3
    middleThree = 1 #keeps track of the middle number within the 3
    upperThree = 1 #keeps track of the first number within the 3

    #If n is less than 3, then trib num is 1
    if (n < 3): 
        return 1
    else:
        while(n > 2):
           currentSum = bottomThree + middleThree + upperThree #add the current 3 numbers
           bottomThree = middleThree
           middleThree = upperThree
           upperThree = currentSum #before continuing, make upperThree the last summation done
           n -= 1

    return currentSum   

#Function finds the max two numbers in a list
def max_two(xs):
    maxList = []
    currentMax = None
    dupNum = None
    firstIteration = True #To see if its the first interation in the loop

    #First check if list is empty
    if (xs == []):
        return []

    #Check if list is only one element
    if (len(xs) == 1):
        return xs

    #Set current max to first element
    currentMax = xs[0]

    #Find the first max
    for i in xs:
        if (currentMax < i):
            currentMax = i
        elif (currentMax == i and firstIteration == False):
            dupNum = i
        firstIteration = False #First iteration done
    
    maxList.append(currentMax)

    #If there was a duplicate of the max, then add to list and return
    if (currentMax == dupNum):
        maxList.append(dupNum)
        return maxList

    #reset currentMax to the first element, if the max is the first element,
    #then set currentMax to the second element in the list
    currentMax = xs[0]
    if (maxList[0] == xs[0]):
        currentMax = xs[1]

    #Find the second max if no duplicates exist
    for i in xs: 
        if (currentMax < i and i != maxList[0]):
            currentMax = i

    maxList.append(currentMax)

    return maxList

#Function to reverse and return given list
def reversed(xs):
    rList = []
    i = len(xs)

    while(i > 0): #start from the back and decrement
        rList.append(xs[i - 1]) 
        i -= 1
    
    return rList

#Function to return a clockwise double list
def clockwise(grid):
    clockwiseList = []
    
    #First check to see if grid is empty
    if (grid == []):
        return grid

    listSize = len(grid) - 1 #gets the size of the list
    innerListSize = len(grid[0]) #gets the size of the first inner list to compare

    #Check to see if its rectangular
    for i in grid:
        if (len(i) != innerListSize):
            raise ValueError("not rectangular")

    #If its rectangular, then form clockwise list
    for k in range(innerListSize): #k = inner list index
        temp = []
        for i in range(listSize, -1, -1): #i = outter list index, decrement by 1
            temp.append(grid[i][k])
        clockwiseList.append(temp)
   
    return clockwiseList

#Function returns True if any items in list are true and false if all are false
def any(xs):

    #checks for empty list
    if (xs == []):
        return False

    for i in xs:
        if (i == True): #if an element is True, return True immediately
            return True

    return False

#Function retuns a list of passed values for a given predicate
def select(p, xs):
    returnList = []

    #Go through each value and see if it returns true for the predicate
    for i in xs:
        if (p(i) == True):
            returnList.append(i)
        
    return returnList

#Function returns results of applied values to the given function
def zip_with(f, xs, ys):
    returnList = []
    firstListSize = len(xs)
    secondListSize = len(ys)
    shortestListSize = firstListSize #the size of the shortest list, defaults to first

    #First check if any list is empty
    if (xs == [] or ys == []):
        return returnList

    #Figure out wich list is shorter
    if (firstListSize > secondListSize):
        shortestListSize = secondListSize
    
    #Apply values to functiono and append to return list
    for i in range(0, shortestListSize):
        temp = f(xs[i], ys[i])
        returnList.append(temp)

    return returnList

#Function returns a 2D augmented identity matrix 
def augdentity(r, c):
    returnList = []
    oneIndex = 0 #keeps track of where 1 will be

    for i in range(r): #creating rows
        temp = []
        for k in range(c): #creating the columns
            if (k == oneIndex): #append 1 if at one index
                temp.append(1)
            else:
                temp.append(0) #otherwise append 0
        oneIndex += 1 #increment one index
        returnList.append(temp)

    return returnList


#   HELPER FUNCTIONS

#Function to see if a number is prime or not
def is_prime(n):
    result = True

    for i in range(2, n): 
        if (n % i == 0):
            result = False
    
    return result

#Function to get all factors of a number n
def all_factors(n): 
    factors = []

    for i in range(1, n + 1):
        if (n % i == 0):
            factors.append(i)

    return factors
