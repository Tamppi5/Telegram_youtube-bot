import urllib.request
import re

search = input("Enter youtube search:")

html = urllib.request.urlopen("https://www.youtube.com/results?search_query=" + search.replace(" ",""))
video_ids = re.findall(r"watch\?v=(\S{11})", html.read().decode())
print("https://www.youtube.com/watch?v=" + video_ids[0])
