CAUTION: This is incomplete and broken. I am in the process of converting it from text processing on a directory of markdown files to using the RoamResearch database from an EDN export. Once it works, I'll add more instructions here.


# Roam-migration

A command-line tool to convert [Roam Research](https://roamresearch.com/) exported files to [Org-roam](https://github.com/org-roam/org-roam) compatible markdown.


## Remaining tasks

- [X] Why does (line-count success.txt) + (line-count errors.txt) not = the total # of nodes? (Some Roam page names convert to identical file names)
- [X] Parse image links in Roam, emit in Org (db-id 68414 has an example)
- [ ] Emitting bold text with inline code inside it doesn't format correctly in org
- [X] Daily notes should go in a "daily" subdirectory, which will be created if it doesn't exist
- [X] Internal links should use "id:xxx" as their href.
- [X] All files should have a PROPERTIES drawer with an ID property in it.

## Big features

- [ ] MAYBE: Download firebase images to local folder?
- [ ] MAYBE: Make lowest-level block into text instead of an org bullet
