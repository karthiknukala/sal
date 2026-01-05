#---------------------------------------
#feedback on
#
#var:
#nodes: number of nodes in the cluster
#startuptime: worst case startup time under test
#feedback: feedback method enabled/disabled 
#degree: failure degree 


degree=6

trace_dir=trace

feedback='TRUE'

mkdir ${trace_dir}

for nodes in  3 4 5 
do
    let startuptime=7*${nodes}-5
  
    filename=startup_${nodes}_${startuptime}_${feedback}

    query_nodes='s/@par_nodes/'${nodes}'/g'
    query_feedback='s/@par_feedback/'${feedback}'/g'
    query_filename='s/@par_filename/'${filename}'/g'
    query_degree='s/@par_degree/'${degree}'/g'
    query_startuptime='s/@par_startuptime/'${startuptime}'/g'

    sed -e ${query_nodes} -e ${query_feedback} -e ${query_filename} -e ${query_degree} -e  ${query_startuptime} startup-skel.sal > ${filename}.sal


    echo 'proofing '${trace_dir}'/'${filename}'_startuptime'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} startuptime > ${trace_dir}/${filename}_startuptime_stdout.trc 2> ${trace_dir}/${filename}_startuptime_stderr.trc



    echo 'proofing '${trace_dir}'/'${filename}'_safety'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} safety > ${trace_dir}/${filename}_safety_stdout.trc 2> ${trace_dir}/${filename}_safety_stderr.trc



    echo 'proofing '${trace_dir}'/'${filename}'_ok'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} ok > ${trace_dir}/${filename}_ok_stdout.trc 2> ${trace_dir}/${filename}_ok_stderr.trc

done

for nodes in  3 4 5 
do
    let startuptime=7*${nodes}-6
  
    filename=startup_${nodes}_${startuptime}_${feedback}

    query_nodes='s/@par_nodes/'${nodes}'/g'
    query_feedback='s/@par_feedback/'${feedback}'/g'
    query_filename='s/@par_filename/'${filename}'/g'
    query_degree='s/@par_degree/'${degree}'/g'
    query_startuptime='s/@par_startuptime/'${startuptime}'/g'

    sed -e ${query_nodes} -e ${query_feedback} -e ${query_filename} -e ${query_degree} -e  ${query_startuptime} startup-skel.sal > ${filename}.sal


    echo 'proofing '${trace_dir}'/'${filename}'_startuptime'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} startuptime > ${trace_dir}/${filename}_startuptime_stdout.trc 2> ${trace_dir}/${filename}_startuptime_stderr.trc


done



#feedback off

feedback='FALSE'

for nodes in  3 4 5 
do
    let startuptime=7*${nodes}-5
  
    filename=startup_${nodes}_${startuptime}_${feedback}

    query_nodes='s/@par_nodes/'${nodes}'/g'
    query_feedback='s/@par_feedback/'${feedback}'/g'
    query_filename='s/@par_filename/'${filename}'/g'
    query_degree='s/@par_degree/'${degree}'/g'
    query_startuptime='s/@par_startuptime/'${startuptime}'/g'

    sed -e ${query_nodes} -e ${query_feedback} -e ${query_filename} -e ${query_degree} -e  ${query_startuptime} startup-skel.sal > ${filename}.sal


    echo 'proofing '${trace_dir}'/'${filename}'_startuptime'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} startuptime > ${trace_dir}/${filename}_startuptime_stdout.trc 2> ${trace_dir}/${filename}_startuptime_stderr.trc



    echo 'proofing '${trace_dir}'/'${filename}'_safety'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} safety > ${trace_dir}/${filename}_safety_stdout.trc 2> ${trace_dir}/${filename}_safety_stderr.trc



    echo 'proofing '${trace_dir}'/'${filename}'_ok'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} ok > ${trace_dir}/${filename}_ok_stdout.trc 2> ${trace_dir}/${filename}_ok_stderr.trc

done

for nodes in  3 4 5 
do
    let startuptime=7*${nodes}-6
  
    filename=startup_${nodes}_${startuptime}_${feedback}

    query_nodes='s/@par_nodes/'${nodes}'/g'
    query_feedback='s/@par_feedback/'${feedback}'/g'
    query_filename='s/@par_filename/'${filename}'/g'
    query_degree='s/@par_degree/'${degree}'/g'
    query_startuptime='s/@par_startuptime/'${startuptime}'/g'

    sed -e ${query_nodes} -e ${query_feedback} -e ${query_filename} -e ${query_degree} -e  ${query_startuptime} startup-skel.sal > ${filename}.sal


    echo 'proofing '${trace_dir}'/'${filename}'_ok'

    sal-smc -v 10 --disable-traceability --num-reorders=2 --enable-slicer ${filename} ok > ${trace_dir}/${filename}_ok_stdout.trc 2> ${trace_dir}/${filename}_ok_stderr.trc

done
