from collections import deque

def check(string):
  seen = set()
  for i in range(len(string)):
    if i == 0:
      seen.add(string[i])
      continue
    if not (string[i] == string[i-1]):
      if string[i] not in seen:
        seen.add(string[i])
      else:
        return False
  return True

def complement(base):
  if base == "A":
    return "U"
  if base == "U":
    return "A"
  if base == "C":
    return "G"
  return "C"

def myprint(d):
  return "".join(d)

def complemented(currentleft):
    newleft = []
    for i in range(len(currentleft)):
      base = currentleft.pop()
      newleft.append(complement(base))
    newleft.reverse()
    return newleft

def solver(currentleft):
    currentright = []
    history = []
    remaining = deque()
    seen = set()

    # 1st push
    copy1 = deque(currentleft)
    base = copy1.pop()
    copy2 = deque()
    copy2.append((base))
    remaining.append((copy1,copy2,['p'],'p','',False))
    seen.add(("".join(copy2),False))

    while remaining:
      (currentleft,currentright,history,lastmove,secondlastmove,comp) = remaining.popleft()
      #check answer
      if not currentleft:
        if check(myprint(currentright)) :
          return myprint(history)
        else:
          continue

      # complement
      if not lastmove == 'c' and not (lastmove == 'r' and secondlastmove == 'c'):
        newleft = complemented(deque(currentleft))
        copy2 = deque(currentright)
        string = "".join(currentright)
        if (string,not comp) not in seen:
          copy3 = list(history)
          copy3.append('c')
          remaining.append((deque(newleft),copy2,copy3,'c',lastmove,not comp))
          seen.add((string,not comp))

      #push
      char1 = currentleft.pop()
      char2 = currentright.popleft()
      if char1 == char2 or char1 not in currentright:
        currentleft.append(char1)
        currentright.appendleft(char2)
        copy1 = deque(currentleft)
        movingbase = copy1.pop()
        copy2 = deque(currentright)
        copy2.appendleft(movingbase)
        string = "".join(copy2)
        if (string,comp) not in seen:
          copy3 = list(history)
          copy3.append('p')
          remaining.append((copy1,copy2,copy3,'p',lastmove,comp))
          seen.add((string,comp))
      else:
        currentleft.append(char1)
        currentright.appendleft(char2)


      #reverse
      if not lastmove == "r" and not(lastmove == 'c' and secondlastmove == 'r'):
        copy1 = deque(currentleft)
        currentright.reverse()
        copy2 = deque(currentright)
        string = "".join(copy2)
        if (string,comp) not in seen:
            copy3 = list(history)
            copy3.append('r')
            remaining.append((copy1,copy2,copy3,'r',lastmove,comp))
            seen.add((string,comp))
    return "What did i do wrong"

def main():
    # fileopen = open("vaccine.in11", "r")
    fileopen = open(str(sys.argv[1]), "r")

    counter = 0
    cases = []
    for newline in fileopen:
      if counter == 0:
        numberofcases = int(newline)
      else:
        cases.append(newline)
      counter += 1
    fileopen.close()

    stackcases = []
    for i in range (numberofcases):
      casetostack = []
      for j in range (len(cases[i])-1):
        if (cases[i][j] =="A"):
          casetostack.append("A")
        elif (cases[i][j] =="U"):
          casetostack.append("U")
        elif (cases[i][j] =="C"):
          casetostack.append("C")
        else :
          casetostack.append("G")
      stackcases.append(deque(casetostack))
    cases = deque(stackcases)

    while cases:
      current = cases.popleft()
      print(solver(current))

    return
