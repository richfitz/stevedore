## version: 1.27
## method: post
## path: /images/prune
## code: 200
## response: {"ImagesDeleted": [{"Untagged": "string","Deleted": "string"}],"SpaceReclaimed": 0}
list(images_deleted =
       data.frame(untagged = "string",
                  deleted = "string",
                  stringsAsFactors = FALSE),
     space_reclaimed = 0L)
