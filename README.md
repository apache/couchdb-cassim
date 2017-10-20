# Cassim

*Open Sesame!*

Cassim is responsible for storing security information for clustered databases 
in a clustered database (``cassim``). This information is then heavily cached 
for reference at runtime.

Note: this is not yet used in CouchDB 2.x yet, but is planned to land in the future.

# TODO

- [ ] Fix Security objects cannot be synced if nodes are in maintenance mode https://github.com/apache/couchdb/issues/602
- [ ] Merge into couchdb.git repo.


# Notes

[22:12:49]  <+jan____>	chewbranca: davisp: what’s the state of cassim at Cloudant?
[22:13:12]  <chewbranca>	    heh... fabric:get_security is terrible and cassim would be a great replacement for it
[22:13:32]  <chewbranca>	    well, at least for the primary case of caching the data, you still need fabric:get_security behind cassim 
[22:15:13]  <chewbranca>	    cassim is not used right now because of the CouchDB security model, and Cloudant supports both. The Cloudant security model happens in chttpd so it's on the coordinator side and is perfectly fine for dealing with clustered databases. The CouchDB security model on the other hand operates on the shard level, which makes it really awkward to be used with cassim as cassim is powered by the clustered _metadata db, so you run into
[22:15:13]  <chewbranca>	    a chicken and egg situation with opening the shards 
[22:16:33]  <chewbranca>	    I've got it on my todo list to get the cassim conversation going again, because fabric:get_security really is bad and is a significant resource usage and is now called now once but *twice* per doc request, further negatively impacting performance 
[22:17:02]  <chewbranca>	    so that leaves the question with what to do about the CouchDB security model 
[22:17:43]  <chewbranca>	    we _could_ leave things as is and change cassim to be a node local clustered database like the nodes/dbs dbs, although I would prefer avoiding that 
[22:18:06]  <banjiewen>		    Why would you prefer to avoid that, chewbranca?
[22:18:09]  <chewbranca>	    the other option is we rip out the current security model and use a security model on the coordinator side 
[22:18:40]  <+Wohali>		    security on the coordinator side sounds like an advantage
[22:18:51]  <chewbranca>	    banjiewen: just seems clunky and conflict inducing, perhaps not though, it could be ok
[22:19:11] banjiewen		    nods
[22:19:23]  <banjiewen>		    Perhaps, but if it avoids a painful deprecation path maybe it's worth it
[22:19:36]  <chewbranca>	    Wohali: yeah, fwiw when you set the security properties on Cloudant, if you don't use the CouchDB style security then all security checks happen on the coordinator side
[22:19:55]  <chewbranca>	    banjiewen: eh we're just delaying the inevitable though, the security models of both Cloudant and CouchDB need to be rewritten 
[22:20:00]  <chewbranca>	    neither of them are sufficient 
[22:20:15]  <banjiewen>		    Sure, that's fair
[22:21:18]  <+Wohali>		    chewbranca: that rings a distant bell, though it's been a long time since i looked at dbcore/dbnext
[22:21:33]  <chewbranca>	    but yeah, RE Cassim, it turns out that fabric:get_security is a serious hog and calling it twice per doc request is brutal. We've seen on the order of a 30% throughput improvement by turning Cassim on 
[22:24:19]  <+Wohali>		    chewbranca: sounds like a great candidate for 2.2 ;)
[22:24:22]  <+Wohali>		    land that and PSE :D
[22:25:53]  <+Wohali>		    there was a good comment in #couchdb about improving the compaction daemon to preferentially pick shards that have the biggest potential gains to be had by compression
[22:26:40]  <+jan____>		    chewbranca: thanks, that’s lots of good context.
[22:26:58]  <chewbranca>	    Wohali: well either need to rewrite the security engine or rewrite the storage layer of cassim, either way there's a bit of work 
[22:27:46]  <chewbranca>	    jan____: np, I'm not sure why Cloudant security didn't land in the merge, probably just to keep things simple and not add in a second not quite redundant security model, but this has needed to be fixed for years now 
[22:28:18]  <+jan____>		    I haven’t put any thought into this, but would there be a way to model CouchDB security semantics on top of code that lives in chttpd?
[22:30:58]  <chewbranca>	    jan____: yeah that's exactly what the Cloudant security does, there's just a different implementation of https://github.com/apache/couchdb/blob/master/src/chttpd/src/chttpd_auth.erl#L44-L45
[22:32:04]  <+jan____>		    chewbranca: right, I mean, how hard would it be to lift couchdb security up into chttpd? 
[22:33:14]  <chewbranca>	    not hard at all, the plumbing is already there, the bigger issue is what to do about roles and what not, which _could_ be done orthogonally, but needs to be done at some point 


