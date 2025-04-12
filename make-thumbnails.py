#!/usr/bin/env python3

import math
from io import BytesIO
from PIL import Image
import sqlite3

maxsize = 300

con = sqlite3.connect("db.sqlite")
cur = con.cursor()
res = cur.execute("select * from images left join thumbs on images.id = thumbs.id where thumbs.id is null")
images = res.fetchall()

for i in images:
    f = BytesIO(i[3])
    img = Image.open(f)
    ratio = maxsize/max(img.size)
    newsize = (math.ceil(img.size[0]*ratio), math.ceil(img.size[1]*ratio))
    print(i[0] + ":", img.size, '->', newsize)
    thumb = img.resize(newsize)
    out = BytesIO()
    thumb.save(out, format='PNG')
    cur.execute("insert into thumbs (id, file) values (?, ?)", [i[0], out.getvalue()])

con.commit()
