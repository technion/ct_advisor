ct_advisor
==========

ct_advisor is a proactive alerting tool for [Google's Certificate Transparency](https://www.certificate-transparency.org/).

It is running live on this [this link](https://ctadvisor.lolware.net) and we encourage you to register your domains there.

Google offers a number of great options for an administrator to utilise this feature. Unfortunately being an early adopter, particularly if you run Windows servers or run SSL on appliances, makes it difficult to take advantage of this service.

As an alternative option, this service continually polls the CT log, and will trigger alerts if a certificate is ever registered for your domain, by any CA in the CT program. This can be used to identify fraudulent certificates.

This image this ct_advisor in action:

![CT Advisor Email](https://lolware.net/ct_advisor_email.jpg)


Monitoring your domain
----------------------

This application is currently running live on my servers. At this point I consider it stable, whilst noting some database schema changes may occur to facilitate the upcoming front-end project. If you would like my server to monitor your domain in the meantime, please email me your request: technion@lolware.net. Please provide all names to monitor and associated email addresses.

Note that monitors are not instant. Some certificates have taken several days to show up in CT monitor logs.

Setup
-----

This application uses a PostgreSQL database, and an SMTP server.

- Install the front end, ct_advisor_int
- Create tables using the Rails frontend
- Create priv/credentials.rr in the following format:

```erlang
{database, {credentials, "localhost", "ct_advisor", "password"}}.
{smtp, {credentials, "email-relay.com", "username", "password"}}.
```

Build
-----

This application bundles the tested version of rebar3, and will pull its own external dependancies, of which there are several. Both eunit and Common Test suites are utilised.

```shell
$ ./rebar3 xref
$ ./rebar3 dialyzer
$ ./rebar3 eunit
$ ./rebar3 ct
$ /.rebar3 release
```

In development
--------------
It's far easier to utilise my instance of this tool than to attempt to run it yourself - I recommend doing so unless you wish to be involved in development.


Contributing
------------

* In line with the above, potential contributors should be aware I am unlikely to merge and changes relating to features that I won't be using.
* Code must produce no errors under dialyzer, xref or elvis
* Complex functions must include eunit tests
* Leave your politics at the door

