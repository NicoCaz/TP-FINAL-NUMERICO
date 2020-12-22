 set autoscale
 unset log                              # quita la escala logaritmica (si la hubiera)
 unset label                            # quita los titulos anteriores
 set xtic auto                          # establece automaticamente las divisiones del eje x
 set ytic auto                          # establece automaticamente las divisiones del eje y
 set grid
 set title " EDO PVI "
 set xlabel "x"
 set ylabel "y"
 plot "datoshfijo.dat" using 1:2 title "Y1" with lines,\
 "datoshfijo.dat" using 1:3 title "Y2" with lines,\
 "datoshfijo.dat" using 1:1 title "Y3" with lines,\
