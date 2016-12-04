#!/bin/bash
#gmt pscoast -R-180/180/-70/70 -JM0/14/7 -Ba60g30f15/a30g30f15WSen >! map1.ps
gmt pscoast -R120/160/20/70 -JM5/3i -B10/1:.'Hello,plot!': -Gchocolate  >plot.ps

