## version: 1.36
## method: get
## path: /exec/{id}/json
## code: 200
process_config <- list(
  privileged = FALSE,
  user = "1000",
  tty = TRUE,
  entrypoint = "sh",
  arguments = c("-c", "exit 2"))

## NOTE: CanRemove is missing in the spec (for 1.29 at least)

list(
  can_remove = FALSE,
  detach_keys = "",
  id = "f33bbfb39f5b142420f4759b2348913bd4a8d1a6d7fd56499cb41a1bb91d7b3b",
  running = FALSE,
  exit_code = 2L,
  process_config = process_config,
  open_stdin = TRUE,
  open_stderr = TRUE,
  open_stdout = TRUE,
  container_id =
    "b53ee82b53a40c7dca428523e34f741f3abc51d9f297a14ff874bf761b995126",
  pid = 42000L)
