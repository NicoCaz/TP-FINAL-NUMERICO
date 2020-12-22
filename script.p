 set autoscale
 unset log                              # quita la escala logaritmica (si la hubiera)
 unset label                            # quita los titulos anteriores
 set xtic auto                          # establece automaticamente las divisiones del eje x
 set ytic auto                          # establece automaticamente las divisiones del eje y
 set grid
 set title " EDO PVI "
 set xlabel "x"
 set ylabel "y"
 plot "datoshfijo.dat" using 1:2 title "ELASTICA" with lines,\
