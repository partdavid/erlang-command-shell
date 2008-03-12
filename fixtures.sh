#!/bin/sh

export TZ=
# I don't want any timezone stuff affecting this, the unit tests are
# based on universal time. -pd

rm -rf ecs_test*
mkdir -p ecs_test/d1/dd1 ecs_test/d2
cd ecs_test
cat >birds <<EOF
The mighty eagle soars high above.
The lowly parakeet is green.
The crane will go for your eyes.
EOF

cat >mustelids <<EOF
The weasel cranes its neck.
Only a wolverine would do that.
A wine gourd, torn, deceives an otter.
EOF

touch -m -t 200008011345.32 zzz

tf=`mktemp`
cat >$tf <<EOF
Jan:1
Feb:2
Mar:3
Apr:4
May:5
Jun:6
Jul:7
Aug:8
Sep:9
Oct:10
Nov:11
Dec:12
EOF

cd ..
ls -l ecs_test | grep -v total | while read mode links uid gid size mon day hhmm name
do
   year=`date +%Y`
   type=`echo $mode | cut -c1`
   if [ $type = d ]
   then
      type=directory
   fi
   if [ $type = - ]
   then
      type=regular
   fi
   mode=`echo $mode | cut -c2-`
   hour=`echo $hhmm | cut -f1 -d:`
   min=`echo $hhmm | cut -f2 -d:`
   if [ $name = zzz ]
   then
      year=2000
      mon=Aug
      day=1
      hour=13
      min=45
   fi
   mnum=`grep $mon $tf | cut -f2 -d:`

   echo "{\"$name\", [{type, $type}, {size, $size}, {mtime, {{$year, $mnum, $day}, {$hour, $min, 0}}}, {links, $links}]}."
done >ecs_test.lsl
rm -rf $tf

ls ecs_test | while read f
do
   echo "\"$f\"".
done >ecs_test.ls

# Yes, this is cheating, but it's so much harder to get this
# to work in sh that this is probably more robust. -pd
cat >ecs_test.lsd <<EOF
["birds", "mustelids", "zzz"].
{"d1", ["dd1"]}.
{"d2", []}.
EOF

for text in birds mustelids
do
while read line
do
   echo "\"$line\"."
done <ecs_test/$text >ecs_test.$text
done

find ecs_test | while read f
do
   f=`echo $f | sed 's/\/$//'`
   echo "\"$f\"."
done >ecs_test.find

find ecs_test -type f | while read f
do
   f=`echo $f | sed 's/\/$//'`
   echo "\"$f\"."
done >ecs_test.findf

find ecs_test -mtime +1 | while read f
do
   f=`echo $f | sed 's/\/$//'`
   echo "\"$f\"."
done >ecs_test.findm
