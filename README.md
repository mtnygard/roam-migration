CAUTION: This is incomplete and broken. I am in the process of converting it from text processing on a directory of markdown files to using the RoamResearch database from an EDN export. Once it works, I'll add more instructions here.


# Roam-migration

A command-line tool to convert [Roam Research](https://roamresearch.com/) exported files to [Org-roam](https://github.com/org-roam/org-roam) compatible markdown.


## Remaining tasks

- [ ] Why does (line-count success.txt) + (line-count errors.txt) not = the total # of nodes?
- [ ] Parse image links in Roam, emit in Org (db-id 68414 has an example)
