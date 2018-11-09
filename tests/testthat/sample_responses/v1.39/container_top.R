## version: 1.39
## method: get
## path: /containers/{id}/top
## code: 200
## response: {"Titles":["UID","PID","PPID","C","STIME","TTY","TIME","CMD"],"Processes":[["root","13642","882","0","17:03","pts/0","00:00:00","/bin/bash"],["root","13735","13642","0","17:06","pts/0","00:00:00","sleep 10"]]}
list(titles = c("UID", "PID", "PPID", "C", "STIME", "TTY", "TIME", "CMD"),
     processes = list(
       c("root", "13642", "882", "0", "17:03", "pts/0", "00:00:00",
         "/bin/bash"),
       c("root", "13735", "13642", "0", "17:06", "pts/0", "00:00:00",
         "sleep 10")))
