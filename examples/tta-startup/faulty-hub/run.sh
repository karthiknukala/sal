#!/bin/sh 





#---------------------------------------
#feedback on
#
#var:
#nodes: number of nodes in the cluster
#startuptime: worst case startup time under test
#feedback: feedback method enabled/disabled 




trace_dir=trace

mkdir ${trace_dir}

nodes=3

startuptime=23

for nodes in 3 4 5
do

for big_bang in TRUE FALSE
do

    let startuptime=7*${nodes}-5
  
    filename=startup_${nodes}_${startuptime}_${big_bang}

    query_nodes='s/@par_nodes/'${nodes}'/g'
    query_feedback='s/@par_feedback/'${feedback}'/g'
    query_filename='s/@par_filename/'${filename}'/g'
    query_degree='s/@par_degree/'${degree}'/g'
    query_startuptime='s/@par_startuptime/'${startuptime}'/g'
    query_big_bang='s/@par_big_bang/'${big_bang}'/g'
    

    sed -e ${query_nodes} -e ${query_filename} -e ${query_startuptime} -e ${query_big_bang} startup-faulty-guardian.sal > ${filename}.sal


    echo 'proofing '${trace_dir}'/'${filename}'_safety_2'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} safety_2 > ${trace_dir}/${filename}_safety_2_stdout.trc 2> ${trace_dir}/${filename}_safety_2_stderr.trc



    echo 'proofing '${trace_dir}'/'${filename}'_safety'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} safety > ${trace_dir}/${filename}_safety_stdout.trc 2> ${trace_dir}/${filename}_safety_stderr.trc

    echo 'proofing '${trace_dir}'/'${filename}'_startuptime'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} startuptime > ${trace_dir}/${filename}_startuptime_stdout.trc 2> ${trace_dir}/${filename}_startuptime_stderr.trc


done

done
