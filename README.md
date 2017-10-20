# Cassim

*Open Sesame!*

Cassim is responsible for storing security information for clustered databases 
in a clustered database (``cassim``). This information is then heavily cached 
for reference at runtime.

Note: this is not yet used in CouchDB 2.x yet, but is planned to land in the future.

# TODO

- [ ] Fix Security objects cannot be synced if nodes are in maintenance mode https://github.com/apache/couchdb/issues/602
- [ ] Merge into couchdb.git repo.