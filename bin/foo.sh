rm ./foo.out
for id in `pbpaste | awk 'ORS=NR%1?RS:"\n"' RS=","`; do
echo "db2 select * from nonuser where forwardurl like '%instid $id%'"  >> ./foo.out
done
chmod 777 foo.out
