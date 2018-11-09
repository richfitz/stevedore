## version: 1.38
## method: get
## path: /containers/{id}/changes
## code: 200
## response: [{"Path":"/dev","Kind":0},{"Path":"/dev/kmsg","Kind":1},{"Path":"/test","Kind":1}]
data.frame(path = c("/dev", "/dev/kmsg", "/test"),
           kind = c(0, 1, 1),
           stringsAsFactors = FALSE)
