## version: 1.28
## method: get
## path: /plugins/privileges
## code: 200
## response: [{"Name":"network","Description":"","Value":"host"},{"Name":"mount","Description":"","Value":"/data"},{"Name":"device","Description":"","Value":"/dev/cpu_dma_latency"}]
data.frame(
  name = c("network", "mount", "device"),
  description = "",
  value = I(list("host", "/data", "/dev/cpu_dma_latency")),
  stringsAsFactors = FALSE)
