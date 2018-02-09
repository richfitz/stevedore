docker <- stevedore::docker_client()
docker$containers$run("richfitz/iterate", c("10", "0.1"), rm = TRUE)
