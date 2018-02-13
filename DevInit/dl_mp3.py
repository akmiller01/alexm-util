import requests
from bs4 import BeautifulSoup as bs
from os.path import basename
import shutil

base_url = "https://www.livelingua.com"
page_url = "https://www.livelingua.com/course/fsi/French_-_Basic_Course_(Volume_1)"
selector = "a[href$='.mp3']"
output_folder = "C:\\Users\\Alex\\Documents\\French\\"

result = requests.get(page_url)
c = result.content

soup = bs(c)
mp3s = soup.select(selector)
for mp3 in mp3s:
    href = base_url+mp3.get("href")
    bn = basename(href)
    print("Downloading {}".format(bn))
    filename = output_folder+bn
    r = requests.get(href,stream=True)
    if r.status_code == 200:
        with open(filename,'wb') as f:
            r.raw.decode_content = True
            shutil.copyfileobj(r.raw,f)
    else:
        print("Non-200 HTTP response...")