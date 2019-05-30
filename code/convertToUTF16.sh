for file in 190*
do
	echo $file
	cd $file
	sub=$(ls .)
	cd "${sub}"
	
	for sample in *.D
	do
		echo $sample
		cd $sample
		iconv -f utf-16 -t utf-8 New.TXT > New2.txt
		cd ..
	done 
	cd ../..
done 
