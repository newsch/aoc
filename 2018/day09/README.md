## timing tests

interpreted list

```
> time ./day09.ml < input.txt
404502

________________________________________________________
Executed in  106.14 secs   fish           external
   usr time  105.73 secs  1208.00 micros  105.73 secs
   sys time    0.07 secs  271.00 micros    0.07 secs

```

interpreted array

```
> time ./day09p2.ml < input.txt
404502

________________________________________________________
Executed in   53.94 secs   fish           external
   usr time   44.68 secs  1252.00 micros   44.68 secs
   sys time    9.16 secs  279.00 micros    9.16 secs

```

interpreted circular list

```
> time ./day09p2.ml < input.txt 2> /tmp/foo
404502

________________________________________________________
Executed in  185.40 millis    fish           external
   usr time  177.38 millis  735.00 micros  176.64 millis
   sys time    8.15 millis  119.00 micros    8.03 millis

```

compiled list (`ocamlopt` defaults)

```
> time ./day09 < input.txt
404502

________________________________________________________
Executed in   52.25 secs   fish           external
   usr time   51.55 secs  494.00 micros   51.55 secs
   sys time    0.62 secs   96.00 micros    0.62 secs

```

compiled array (`ocamlopt` defaults)

```
> time ./day09p2 < input.txt
404502

________________________________________________________
Executed in   24.70 secs   fish           external
   usr time   17.52 secs    0.00 micros   17.52 secs
   sys time    7.16 secs  1657.00 micros    7.16 secs

```

compiled circular list (`ocamlopt` defaults)

```
> time ./day09p2 < input.txt 2> /tmp/foo
404502

________________________________________________________
Executed in   45.90 millis    fish           external
   usr time   41.10 millis  542.00 micros   40.56 millis
   sys time    3.78 millis   96.00 micros    3.69 millis

```
