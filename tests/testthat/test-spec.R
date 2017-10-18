context("sample responses")

dat <- docker_client_data("v1.29")
describe_api(dat)

run_sample_responses(docker_client_data("v1.29"))
run_sample_responses(docker_client_data("v1.30"))
