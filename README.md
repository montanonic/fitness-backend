#fitness-backend

Currently have a lot of the essential groundwork laid out, with database
logging for all actions as well, and authorization logic preventing the client
from carrying out actions that they aren't allowed to carry out.

User and Profile currently has solid functionality, with strong error messages
that make clear what are causing any exceptions to proper functioning.
FriendshipRequests exist, but a notification system is needed for them to be
at a desirable level.

Iteration times are fast so far, and coming soon are:

* a notification system that will be layered into the current implementation of
FriendRequests, among other things.

* database logic and url handlers for group messaging, refining the notification
system as needed.

The second bullet is where I expect to spend a while. After that, there won't be
too much left to implement to get to a **prototype-worthy** version of the
server, and priorities will begin shifting over to getting the server hosted on
AWS. Authentication and Emails through AWS will be tackled around then.

## By around summer

Once hosted (irrespective of the status on Authentication, Emails, and/or Push
Notifications) for the homepage I'll make a simple but straightforward interface
with explanations of what each url request does, along with how to use it, so
that you guys can start testing them out and giving me feedback. This will be
useful for me to test with too, so it will definitely be worth spending a couple
days to get this set up. To emphasize: this will be a web-page interface for
out-of-mobile-app testing.

Regarding any feedback: Instead of relying solely upon emailing, I think it will
be nice to have a simple development page where you guys can tag any url route
with issues or requests for changes, along with telling me the type of data
queries or actions you'd like to see implemented that currently lack a url
request, so that I can know what to make next to best address the needs of your
client application.

Again, this is a stage I expect to be at around Summer, with a loose timeline of
about 2-4 months. Hosting on AWS is going to take a while, I suspect, as it's
still a new problem-domain for me. Thankfully, web development is finally
something I have some comfort in now, so improving the server is going to remain
a much smoother process, even if the actual hosting of it, and acclimation to
AWS tooling, is not smooth.

At the tail-end of this process, we can get a live development cycle going, and
start working together on offloading your client-side data code to efficient
server-side handling. The transitioning period will likely have some unexpected
difficulties, as we'll probably all be in a foreign problem-domain, but once we
figure out how to get things running smoothly, almost all data-related concerns
will be able to be offloaded onto server-side handling, and reduced to simple
http requests within your mobile client. This will allow you guys to focus on
the View code and performance of your application, while I'll get the bulk of
your data concerns to deal with. 
