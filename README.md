#fitness-backend

Currently have a lot of the essential groundwork laid out, with database
logging for all actions as well, and authorization logic preventing the client
from carrying out actions that they aren't allowed to carry out.

User, Profile, and Friendship entities currently have solid functionality, with
strong error messages that make clear what are causing any exceptions to proper
functioning.

Iteration times are fast so far, and it won't be very long before all of the
core logic for the App's models are implemented.

Once a **prototype-worthy** version of the server is ready, priorities will
begin shifting over to getting the server hosted on AWS, at which point the
focus will be on getting Emailing and Push notifications working through AWS
services.

## By around summer

Once hosted, I'll make a simple but straightforward html interface  with
explanations of what each url request does, along with how to use it, as the
server homepage, so that you guys can start testing them out and giving me
feedback. This will be useful for me to test with too, so it will definitely be
worth spending a couple days to get this set up properly. To emphasize: this
will be a web-page interface for out-of-mobile-app testing of the live server.

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
AWS tooling, is not as smooth.

At the tail-end of this process, we can get a live development cycle going, and
start working together on offloading your client-side data code to efficient
server-side handling. The transitioning period will likely have some unexpected
difficulties, as we'll probably all be in a foreign problem-domain, but once we
figure out how to get things running smoothly, almost all data-related concerns
will be able to be offloaded onto server-side handling, and reduced to simple
http requests within your mobile client. This will allow you guys to focus on
the View code and performance of your application, while I'll get the bulk of
your data concerns to deal with.
