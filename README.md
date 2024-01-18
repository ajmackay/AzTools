## Building the Package

1.  Make Changes
2.  usethis::use_package("any new dependencies")
3.  devtools::document()
4.  load
5.  devtools::check()
6.  usethis::version

## Function Ideas

Function to post issues to github

<code>post.issue \<- function(title, body) { gh::gh(endpoint = "POST /repos/ajmackay/AzTools/issues", title = title, body = body) }</code>
