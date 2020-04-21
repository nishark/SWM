#/Users/apurvabharatia/Desktop/SWM/sample_dataset/users.csv
import collections
import pickle

'''

likes_d = collections.defaultdict(str)

likes = open("/Users/apurvabharatia/Desktop/SWM/sample_dataset/likes.csv")

#print(likes.readline())

#print(likes.readline())
#print(likes.readline())

for x in likes:
    try:
        lst = x.split(",")
        if lst[0][1:5] == "feca":
            print(lst[0])
        likes_d[lst[0][1:-1]] = " ".join(lst[1:])

    except:
        print(x)


print(str("feca46ddb8ef04f86172ace0cb7e004c") in likes_d)






users = open("/Users/apurvabharatia/Desktop/SWM/sample_dataset/users.csv")

users_d = collections.defaultdict(list)

for x in users:
    arr = x.split(",")
    arr.append(" ")
    users_d[arr[0][1:-1]] = arr[1:]


i = 0


subusers = open("/Users/apurvabharatia/Desktop/SWM/sample_dataset/rowsOfUsers.csv")
#/Users/apurvabharatia/Desktop/SWM/sample_dataset/rowsOfUsers.csv
new_users = collections.defaultdict(list)

for x in subusers:
    lst = x.split(",")
    userid = lst[1][1:-2]
    #print(userid in users_d)
    new_users[userid] = (users_d[userid])



ul = open("/Users/apurvabharatia/Desktop/SWM/sample_dataset/users-likes.csv")
i = 0
for x in ul:
    if i == 0:
        i += 1
        continue
    userid, likeid = x.split(",")
    userid = userid[1:-1]
    likeid = likeid[1:]
    likeid = likeid[:-3]
    if userid in new_users and likeid in likes_d:
        #print("check: ", likes_d[likeid])
        likeVal = likes_d[likeid].split(" ")
        likeVal = " ".join(likeVal)
        likeVal = likeVal[1:-3]
        likeVal += " "
        #print("see: ", likeVal)
        #likeVal = " ".join(likes_d[likeid])
        #users_d[userid] = users_d[userid]
        new_users[userid][-1] += likeVal
        #print(userid, "++", new_users[userid])
        






a = {'hello': 'world'}

with open('new_users.pickle', 'wb') as handle:
    pickle.dump(new_users, handle, protocol=pickle.HIGHEST_PROTOCOL)

print(len(new_users))


'''
with open('new_users.pickle', 'rb') as handle:
    new_users = pickle.load(handle)

o_words = ""
c_words = ""
e_words = ""
a_words = ""
n_words = ""


i = 0
for key, value in new_users.items():
    #print(key, value)

    try:
        o, c, e, a, n = float(value[3]), float(value[4]), float(value[5]), float(value[6]), float(value[7][:-4])
        if o >= 1.9:
            o_words += (" " + value[8])
        if c >= 1.9:
            c_words += (" " + value[8])
        if e >= 1.9:
            e_words += (" " + value[8])
        if a >= 1.9:
            a_words += (" " + value[8])
        if n >= 1.9:
            n_words += (" " + value[8])
    except:
        print(o, c, e, a, n)

print(n_words)


#'1', '23', 'NA', '0.61', '-2.34', '-1.46', '-0.09', '0.55\r\n',