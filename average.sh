#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

val txt = scala.io.Source.fromFile(args(0) + ".txt").getLines().mkString

val list = txt.split(" ").map(e => e.toFloat)

val average = list.sum / list.size

print("The average is : " + average);
