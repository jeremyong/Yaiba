for file in ~/Projects/Yaiba/Yaiba/*.hs
do hsColour -html -anchor $file >`dirname $file`/`basename $file .hs`.html
done