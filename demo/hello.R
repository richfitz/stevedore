docker <- stevedore::docker_client()
docker$container$run("richfitz/iterate", c("10", "0.1"), rm = TRUE)
