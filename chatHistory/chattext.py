import re
b = open("chat.txt","r")
a = open("messages.txt","w")
y = b.readline().decode('utf-8-sig').encode('utf-8')
while y:
    if(y != '\r\n'):
        temp = y.split(": ",2)
        try:
            x = temp[1]
            x = re.sub('([\:\;][\)\|\\\/dDOoPp\(\'\"][\(\)DOo]?)','',x)
            x = re.sub('[?\.#_]','',x)
            x = re.sub('[\s]+',' ',x)
            a.write(x+"\n")
        except IndexError:
            pass
        y = b.readline()

