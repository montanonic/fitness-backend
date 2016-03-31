Implement logging for the rest of the data entities that would make sense to
log information about.

Rewrite any hefty queries to use more efficient data types and potentially the
Conduit library as well.

Give Conversations the option of showing newly invited or reinvited users the
previous history, or ensure that they can only query messages created after they
accepted their invite.

** Tests have been desynchronizing as I've added more, and mgsloan has suggested
I attempt to minimally reproduce this problem and open up an issue on the [yesod
github page](https://github.com/yesodweb/yesod/issues).


--------------------------------------------------------------------------------

####Longer term

Introduce editing, as well as edit histories for Wall Posts and Comments.
