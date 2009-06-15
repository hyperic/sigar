all:
	perl -Mlib=.. -MSigarWrapper -e generate Erlang .
	cd src && make
	cd c_src && make

shell:
	make all
	erl -pa ebin

clean:
	cd src && make clean
	cd c_src && make clean
