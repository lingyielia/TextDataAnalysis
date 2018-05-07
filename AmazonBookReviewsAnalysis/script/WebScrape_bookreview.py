#from urllib.request import urlopen as uReq
from bs4 import BeautifulSoup
import requests
import pandas as pd
import numpy as np
import time

my_url = 'https://www.amazon.com/Fire-Fury-Inside-Trump-White/' +
'product-reviews/1250158060/ref=cm_cr_getr_d_paging_btm_1' +
'?ie=UTF8&reviewerType=all_reviews&pageNumber=1'

page = requests.get(my_url)
html_contents = page.text
#html_contents

soup = BeautifulSoup(html_contents, "html.parser")
print(soup.prettify())

# 1. Vote
votes = soup.find_all('span', 'review-votes')
print (len(votes))
#votesvot_list = []
for i in range(len(votes)-2):
    m = votes[i+2].text
    like_num = m.lstrip('\n ').rstrip(' people found this helpful.\n ')
    like_num = int(float(like_num.replace(",","")))
    vot_list.append(like_num)
vot_listdef get_votes():
    votes = soup.find_all('span', 'review-votes')
    vot_list = []
    if len(votes) == 10:
        for i in range(len(votes)):
            m = votes[i].text
            like_num = m.lstrip('\n ').rstrip(' people found this helpful.\n ')
            try:
                like_num = int(float(like_num.replace(",","")))
            except ValueError:
                like_num = 1
            vot_list.append(like_num)
        return vot_list
    elif len(votes) == 12:
        for i in range(len(votes)-2):
            m = votes[i+2].text
            like_num = m.lstrip('\n ').rstrip(' people found this helpful.\n ')
            try:
                like_num = int(float(like_num.replace(",","")))
            except ValueError:
                like_num = 1
            vot_list.append(like_num)
        return vot_list
    else:
        vot_list = [0,0,0,0,0,0,0,0,0,0]
        print("votes data are wrong")
        return vot_list

# # 2. Title
#the first two duplicate reviews
title_top = soup.find_all('span', 'a-size-base review-title a-text-bold')

def get_titles():
    titles = soup.find_all('a', 'a-size-base a-link-normal review-title a-color-base a-text-bold')
    tit_list = []
    for i in range(len(titles)):
        title = titles[i].text
        tit_list.append(title)
    return tit_list

# 3. Date
def get_dates():
    dates = soup.find_all('span', 'a-size-base a-color-secondary review-date')
    dat_list = []
    for i in range(len(dates)-2):
        m = dates[i+2].text
        date = m.lstrip('on ')
        date = date.replace(",","").replace(" ","-")
        dat_list.append(date)
    return dat_list


# 4. Auther
def get_authers():
    authers = soup.find_all('a', 'a-size-base a-link-normal author')
    aut_list = []
    for i in range(len(authers)):
        auther = authers[i].text
        aut_list.append(auther)
    return aut_list


# 5. Star
#starting from the 4th valid star, to the end before the last 5.
stars = soup.find_all('span', 'a-icon-alt')

if len(stars) > 34:
    sta_list = []
    for i in range(len(stars)-29):
        m = stars[(i+1)*3+1].text
        star = m.rstrip(' out of 5 stars')
        star = float(star)
        sta_list.append(star)
    sta_list
else:
    sta_list = []
    for i in range(len(stars)-24):
        m = stars[(i+1)*3+1].text
        star = m.rstrip(' out of 5 stars')
        star = float(star)
        sta_list.append(star)
    sta_listdef get_stars():
    stars = soup.find_all('span', 'a-icon-alt')
    if len(stars) > 34:
        sta_list = []
        for i in range(len(stars)-29):
            m = stars[(i+1)*3+1].text
            star = m.rstrip(' out of 5 stars')
            star = float(star)
            sta_list.append(star)
        return sta_list
    else:
        sta_list = []
        for i in range(len(stars)-24):
            m = stars[(i+1)*3+1].text
            star = m.rstrip(' out of 5 stars')
            star = float(star)
            sta_list.append(star)
        return sta_list

def get_stars():
    stars = soup.find_all('span', 'a-icon-alt')
    print(len(stars))
    sta_list = []
    for i in range(4,len(stars)):
        m = stars[i].text
        if m == "|":
            pass
        else:
            star = m.rstrip(' out of 5 stars')
            star = float(star)
            sta_list.append(star)
    sta_list = sta_list[:-5]
    return sta_list

# 6. Body
def get_revbody():
    review_body = soup.find_all('span', 'a-size-base review-text') 
    bod_list = []
    for i in range(len(review_body)):
        body = review_body[i].text
        #body = m.rstrip(' out of 5 stars')
        bod_list.append(body)
    return bod_list

onepage = {'reviewer': aut_list,
           'rating': sta_list,
           'date': dat_list,
           'title': tit_list,
           'content': bod_list}
df = pd.DataFrame.from_dict(onepage)

for i in range(2,900):
    my_url = 'https://www.amazon.com/Fire-Fury-Inside-Trump-White/' +
    'product-reviews/1250158060/ref=cm_cr_getr_d_paging_btm_' + str(i) +
    '?ie=UTF8&reviewerType=all_reviews&pageNumber=' + str(i)
    page = requests.get(my_url)

    randtime = np.random.random(900) + np.random.randint(2,8,900)
    time.sleep(randtime[i-2])
    
    html_contents = page.text
    soup = BeautifulSoup(html_contents, "html.parser")   
    
    onepage = {'reviewer': get_authers(),
           'rating': get_stars(),
           'date': get_dates(),
           'title': get_titles,
           'content': get_revbody()}
    try:
        df_new = pd.DataFrame.from_dict(onepage)
        df = pd.concat([df, df_new], ignore_index=True)
        print("get page {}, length of df: {}".format(i, len(df)))
    except ValueError:
        print ("get page {}, arrays must all be same length, length of df: {}".format(i, len(df)))

df.to_csv('bookreviews.csv')
