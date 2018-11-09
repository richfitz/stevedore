## version: 1.38
## method: get
## path: /distribution/{name}/json
## code: 200
## response: {"Descriptor":{"MediaType":"application/vnd.docker.distribution.manifest.v2+json","Digest":"sha256:c0537ff6a5218ef531ece93d4984efc99bbf3f7497c0a7726c88e2bb7584dc96","Size":3987495,"URLs":""},"Platforms":[{"Architecture":"amd64","OS":"linux","OSVersion":"","OSFeatures":"","Variant":"","Features":""}]}
list(
  descriptor = list(
    media_type = "application/vnd.docker.distribution.manifest.v2+json",
    size = 3987495L,
    digest = "sha256:c0537ff6a5218ef531ece93d4984efc99bbf3f7497c0a7726c88e2bb7584dc96",

    urls = ""),
  platforms = data.frame(
    architecture = "amd64",
    os = "linux",
    os_version = "",
    os_features = I(list("")),
    variant = "",
    features = I(list("")),
    stringsAsFactors = FALSE))
